# PIAAC Frame generation

# Reset
rm(list = ls())

# Packages
library(data.table)

# General dwelling frame
dat <- fread(file = "data/frame.csvy.gz", yaml = T)

# Remove double quotes from addresses
dat[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]
dat[, adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]

str(dat)

dat[, .N]
dat[, .N, keyby = .(pop_majo_kods, pop_majo_apraksts, pop_majo)]

dat[pop_majo == 1, .N]

dat[pop_majo == 1, summary(pers_sk)]
dat[pop_majo == 1, summary(pers_sk_16_65)]

dat[pop_majo == 1, .N, keyby = .(pop_piaac, pers_sk_16_65 > 0)]

dat[pop_piaac == 1, .N]
# dat[, .N, keyby = .(der_capi_kods, der_capi_apraksts)]


# Extract flat number

# Number of flats in each building
dat[, n_flats := .N, by = .(ind_maja)]
dat[, .N, keyby = .(n_flats > 1)]


# Function to extract flat number from an address

# Pattern for flat number
# digits with optional letter
# several flat numbers can be combined with /

extract_flat_num <- function(x, # address
                             flat_num_pat = "-[0-9]+[a-z]?(/[0-9]+[a-z]?)*,") {
  
  # Covert to lowercase
  x <- tolower(x)

  # Pattern test
  n <- stringr::str_count(tolower(x), flat_num_pat)
  if (any(n > 1)) stop("Multiple pattern match")
  
  # Extract flat number and remove first and last character
  flat_num <- stringr::str_extract(x, flat_num_pat) |>
    stringr::str_sub(start = 2, end = -2)
  
  # Extract first number and count digits in it
  n <- stringr::str_extract(flat_num, "^[0-9]+") |>
    stringr::str_count(pattern = "[0-9]")
  n[is.na(n)] <- 0
  max_n <- max(n)
  
  # Add prefix for ordering
  for (i in sort(unique(n[n > 0]))) {
    prefix <- rep("0", max_n - i + 1) |> paste(collapse = "")
    flat_num[n == i] <- paste0(prefix, flat_num[n == i])
  }
  
  return(flat_num)

}

# Extract flat numbers from both addresses
dat[n_flats > 1,
    flat_num_ir := extract_flat_num(adrese_ir)]
dat[n_flats > 1 & adr_kods_dziv != "",
    flat_num_vzd := extract_flat_num(adrese_vzd)]

dat[n_flats > 1, .N, keyby = .(flat_num_ir)]
dat[n_flats > 1, .N, keyby = .(flat_num_vzd)]

dat[flat_num_ir != flat_num_vzd]

dat[n_flats > 1 & is.na(flat_num_vzd) &
      adr_kods_dziv != "" & adrese_vzd != "", .(adrese_vzd)]

setorder(dat, atvk, ind_maja, flat_num_ir)

dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, .N] # 217
dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, .(adrese_ir)]

tmp <- dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, ind_maja]
# dat[ind_maja %in% tmp,
#     .(adrese_ir, flat_num_ir, pers_sk_16_65)] |> View()

# Those are virtual flats for persons declared at building level
# in buildings with flats
# Possibly wrong addresses

dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, .N] # 217
dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, der_piaac := 0L]
dat[n_flats > 1 & is.na(flat_num_ir) & der_piaac == 1, .N] # 0

dat[pop_piaac == 1, .N,
    keyby = .(der_capi_kods, der_capi_apraksts, der_capi, der_piaac)]

dat[pop_piaac == 1, .N]
dat[pop_piaac == 1, .N, keyby = .(der_piaac)][, P := prop.table(N)][]
dat[pop_piaac == 1, .N, keyby = .(der_piaac, der_iec2019)][, P := prop.table(N)][]

dat[pop_piaac == 1 & der_piaac == 1, .N]





# Filtrs

dat <- dat[pop_piaac == 1 & der_piaac == 1]

fwrite(x = dat, file = "data/frame_piaac.csvy.gz", yaml = T)
