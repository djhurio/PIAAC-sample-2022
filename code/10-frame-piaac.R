# PIAAC Frame generation

# Reset
rm(list = ls())

# Packages
library(data.table)
library(TSP)
library(ggplot2)

# TSP sort and plot
source("code/TSP-sort-plot.R")

# General dwelling frame
dat <- fread(file = "data/frame.csvy.gz", yaml = T)

# Remove double quotes from addresses
dat[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]
dat[, adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]

# str(dat)

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

extract_flat_num <- function(
    x, # address
    flat_num_pat = "-[0-9]+[a-z]?(/[0-9]+[a-z]?)*,"
  ) {
  
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
dat[pop_piaac == 1, .N,
    keyby = .(der_piaac, der_iec2019)][, P := round(prop.table(N), 3)][]

dat[pop_piaac == 1 & der_piaac == 1, .N]


# Incomplete Addresses
dat[grep("LV-9999", adrese_ir), .N]
dat[grep("LV-9999", adrese_vzd), .N]

dat[grep("LV-9999", adrese_ir), .N, keyby = .(lks92_imp, lks92_imp_flag)]
dat[grep("LV-9999", adrese_ir), .N, keyby = .(adrese_vzd != "")]

dat[grep("LV-9999", adrese_ir)] |> head()

dat[grep("LV-9999", adrese_ir), .N, keyby = .(pop_piaac, der_piaac)]

# Exclude from PIAAC frame
dat[grepl("LV-9999", adrese_ir) & der_piaac == 1L, .N]
dat[grepl("LV-9999", adrese_ir) & der_piaac == 1L, der_piaac := 0L]
dat[grepl("LV-9999", adrese_ir) & der_piaac == 1L, .N]


# Filter PIAAC frame
dat <- dat[pop_piaac == 1 & der_piaac == 1]
dat[, .N]


# Order buildings by TSP
dat_building <- dat[, .(iec2019, ind_maja, adr_kods_maja, lks92x, lks92y)] |>
  unique()
dat_building[, anyDuplicated(ind_maja)]
dat_building[adr_kods_maja != "", anyDuplicated(adr_kods_maja)]

# Test TSP sort
tmp <- dat_building[iec2019 == min(iec2019)]
plot.DT(tmp)
sort.DT(DT = tmp) |> plot.DT()
rm(tmp)

# Sort buildings in each PSU
set.seed(174754)
dat_building <- sort.DT.by(DT = dat_building, by = "iec2019")
setnames(dat_building, "i", "build_order")

plot.DT(dat_building[iec2019 == sample(iec2019, 1)])

dat_building[, .(iec2019, ind_maja, build_order)]



# Add building order to the DU frame
dat <- merge(dat,
             dat_building[, .(iec2019, ind_maja, build_order)],
             by = c("iec2019", "ind_maja"))


# Flat numbers
# dat[n_flats > 1, .(adrese_ir, flat_num_ir)] |> View()
dat[n_flats > 1, .N, keyby = .(flat_num_ir)]
dat[n_flats > 1, .N, keyby = .(flat_num_ir)][order(N)]

# Is any flat without flat number?
dat[n_flats > 1 & is.na(flat_num_ir)]

# Order by strata, PSU, building, flat
setorder(dat, strata2019, iec2019, build_order, flat_num_ir)

dat[, .(strata2019, iec2019, build_order, flat_num_ir)]
dat[, .(strata2019, iec2019, build_order, flat_num_ir)] |> anyDuplicated()


# PIAAC variables

# Household operational ID
# Required if household sampling;
# subset of PERSID that will be assigned in screening;
# Does not include CNTRYID;
# Matches to other PIAAC databases when combined with CNTRYID;
# Blank if persons are selected from registries.
dat[, CASEID := 1e6L + .I]

# Sampling ID: Primary sampling unit (PSU) identification number
# Required if area PSUs are selected; Blank if no PSU selection.
dat[, ID_PSU := match(iec2019, unique(iec2019))]
dat[, as.list(range(ID_PSU)), keyby = .(strata2019)]

# Sampling ID: Household (HH) identification number
# Required if HHs are selected;
# Blank if no household selection,
# otherwise a sequential number is assigned within geographic clusters.
dat[, ID_HH := 1:.N, by = .(iec2019)]
dat[, max(ID_HH), keyby = .(strata2019)]

# Sort order for PSU selection 
# Required if systematic sampling of PSUs; blank otherwise.
dat[, SORT_PSU := ID_PSU]

# Sort order for HH selection
# Required if systematic sampling of households; blank otherwise.
dat[, SORT_HH := ID_HH]

dat[, .(CASEID, ID_PSU, ID_HH, SORT_PSU, SORT_HH)]
dat[, .(CASEID)] |> anyDuplicated()
dat[, .(ID_PSU, ID_HH)] |> anyDuplicated()
dat[, .(SORT_PSU, SORT_HH)] |> anyDuplicated()

dat[, max(nchar(CASEID))]   <= 9
dat[, max(nchar(ID_PSU))]   <= 4
dat[, max(nchar(ID_HH))]    <= 5
dat[, max(nchar(SORT_PSU))] <= 5
dat[, max(nchar(SORT_HH))]  <= 5

# Save PIAAC frame
fwrite(x = dat, file = "data/frame_piaac.csvy.gz", yaml = T)
