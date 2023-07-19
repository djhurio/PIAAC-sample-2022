# Weighting and non-response analyses variables

# Reset
rm(list = ls())
gc()
source(".Rprofile")

# Env
Sys.getenv("DISPLAY")
Sys.setenv("DISPLAY" = ":0.0")
Sys.getenv("DISPLAY")
clipr::clipr_available()

# Packages
library(data.table)
library(pxweb)
library(sf)
library(ggplot2)


# Apkaimes
# https://data.gov.lv/dati/lv/dataset/apkaimes
shp_apk <- read_sf(dsn = "apkaimes")
shp_apk
shp_apk[c("Code", "geometry")]
shp_apk[c("Code", "geometry")] |> plot()
st_crs(shp_apk)

# ATVK on 2021-01-01
# https://data.gov.lv/dati/dataset/robezas
# https://data.gov.lv/dati/dataset/robezas/resource/ed8be0c5-1a13-4a35-b4c7-a202e30de5aa
shp_atvk_2021 <- read_sf(dsn = "Territorial_units_LV_1.2m_(2021.01.01.)")
shp_atvk_2021
shp_atvk_2021[c("L1_code", "geometry")] |> plot()
shp_atvk_2021[c("L0_code", "geometry")] |> plot()
st_crs(shp_atvk_2021)

# ATVK on 2022-07-01
# https://data.gov.lv/dati/dataset/robezas
# https://data.gov.lv/dati/dataset/robezas/resource/d09c36c8-bcb2-4429-b88f-3a08d79c6aab
shp_atvk_2022 <- read_sf(dsn = "Territorial_units_LV_1.2m_(2022.07.01.)")
shp_atvk_2022
shp_atvk_2022[c("L1_code", "geometry")] |> plot()
shp_atvk_2022[c("L0_code", "geometry")] |> plot()
st_crs(shp_atvk_2022)


# Load sample file
dat <- fread(file = "data/sample_piaac.csvy", yaml = T)

# Remove double quotes
dat[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]

# Select only main sample
dat[, .N, keyby = .(sample_du, sample_ft)]
dat <- dat[sample_du == 1L]
dat[, c("sample_du", "sample_ft") := NULL]

# Remove FT variables
grep("_FT$", names(dat), value = T)
dat[, c(grep("_FT$", names(dat), value = T)) := NULL]


# Spatial join
dat[is.na(lks92x) | is.na(lks92y)]

dat_sf <- st_as_sf(x = dat[, .(lks92x, lks92y)],
                   coords = c("lks92x", "lks92y"))
dat_sf
class(dat_sf)

st_crs(shp_apk) == st_crs(shp_atvk_2021)

st_crs(dat_sf)
st_crs(dat_sf) <- st_crs(shp_apk)
st_crs(dat_sf)
dat_sf$geometry[is.na(dat_sf$geometry)]

# ATVK 2021
dat_atvk_2021 <- st_join(x = dat_sf, y = shp_atvk_2021,
                         join = st_nearest_feature)
dat[, atvk_2021           := dat_atvk_2021$L1_code]
dat[, adm_terit_kods_2021 := dat_atvk_2021$L0_code]
rm(dat_atvk_2021)

# ATVK 2022
dat_atvk_2022 <- st_join(x = dat_sf, y = shp_atvk_2022,
                         join = st_nearest_feature)
dat[, atvk_2022           := dat_atvk_2022$L1_code]
dat[, adm_terit_kods_2022 := dat_atvk_2022$L0_code]
rm(dat_atvk_2022)

dat[grep("Ķekava|Mārupe", atvk_nosauk), .N,
    keyby = .(atvk, atvk_nosauk, atvk_2021, atvk_2022)]

dat[is.na(atvk_2021), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(atvk_2021), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]
dat[is.na(adm_terit_kods_2021), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(adm_terit_kods_2021), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]

dat[is.na(atvk_2022), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(atvk_2022), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]
dat[is.na(adm_terit_kods_2022), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(adm_terit_kods_2022), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]

# dat[, .N, keyby = .(adm_terit_kods, adm_terit_kods_2021)] |> View()
dat[, .N, keyby = .(adm_terit_kods_2021)]
dat[, .N, keyby = .(adm_terit_kods)]
dat[, .N, keyby = .(adm_terit_kods_2022)]


# Add apkaimes
atvk_with_apk <- c("0001000", "0054010") # Rīga un Valmiera
atvk_with_apk

dat_apk <- st_join(x = dat_sf, y = shp_apk)

names(dat)
dat[, apk_kods   := dat_apk$Code]
dat[, apk_nosauk := dat_apk$Name]

dat[, .N, keyby = .(atvk %chin% atvk_with_apk,
                    !is.na(apk_kods),
                    !is.na(apk_nosauk))]

rm(dat_apk)

# # Test
# dat[atvk %chin% atvk_with_apk, first(adrese_ir),
#     keyby = .(atvk, atvk_nosauk, apk_kods, apk_nosauk)] |> View()


# DU frame
dat_frame <- fread("data/frame_piaac.csvy.gz", yaml = T)

# Remove double quotes
dat_frame[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]
dat_frame[, adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]

grep("pers_sk", names(dat), value = T)
grep("pers_sk", names(dat_frame), value = T)

dat_frame[, .(CASEID, pers_sk)]

dat <- merge(dat, dat_frame[, .(CASEID, pers_sk)], by = "CASEID", all.x = T)

dat[, .N, keyby = .(pers_sk)]

if (dat[is.na(pers_sk), .N]) stop("is.na(pers_sk)")
if (dat[pers_sk < pers_sk_16_65, .N]) stop("pers_sk < pers_sk_16_65")



# SDIF
dat_sdif <- fread(file = "data/sample_piaac_sdif.csv",
                  colClasses = list(character = "CASEID"))



# SDIF variables
dat_sdif[, .(STRAT_PSU, SORT_PSU, SORT_HH)]

# STRAT_PSU N5
# SORT_PSU  N5
# SORT_HH   N5

# Dwelling Unit (DU)-Level Variables for UNKNOWN ELIGIBILITY AND NONRESPONSE Adjustment

# Number of declared residents
dat[, DUVAR_ALL1 := pers_sk]
dat[, range(DUVAR_ALL1)]

# DUVAR_ALL N2
dat[DUVAR_ALL1 > 99, .(DUVAR_ALL1)]
dat[DUVAR_ALL1 > 99,   DUVAR_ALL1 := 99L]
dat[DUVAR_ALL1 > 99, .(DUVAR_ALL1)]
dat[, range(DUVAR_ALL1)]


# Number of declared residents aged 16-65
dat[, DUVAR_ALL2 := pers_sk_16_65]
dat[, range(DUVAR_ALL2)]

# DUVAR_ALL N2
dat[DUVAR_ALL2 > 99, .(DUVAR_ALL2)]
dat[DUVAR_ALL2 > 99,   DUVAR_ALL2 := 99L]
dat[DUVAR_ALL2 > 99, .(DUVAR_ALL2)]
dat[, range(DUVAR_ALL2)]

dat_sdif <- merge(dat_sdif, dat[, .(CASEID, DUVAR_ALL1, DUVAR_ALL2)],
                  by = "CASEID", all.x = T)


# Area-Level Variables for UNKNOWN ELIGIBILITY AND NONRESPONSE Adjustment

# REGION
dat_sdif[, .(REGION)]
dat_sdif[, AREAVAR1 := REGION]

# JURISDICTION
# We recently found out that one of the software packages we use for the weighting process cannot handle unordered categorical variables with over 30 categories.
# According to your preliminary SDIF codebook, one of the weighting variables (AREAVAR2) contains 43 jurisdictions.
# Could you please combine the categories so that it contains no more than 30 categories in the final SDIF?

tab_adm_terit <- dat_frame[, .(n = .N),
                           keyby = .(reg_stat_kods, reg_stat_nosauk,
                                     adm_terit_kods, adm_terit_nosauk)]
setorder(tab_adm_terit, -n)
tab_adm_terit[, small := .I > 30]

setorder(tab_adm_terit, reg_stat_kods, -n)
tab_adm_terit[, merge := small | (any(small) & n == last(n[!small])),
              by = .(reg_stat_kods)]

tab_adm_terit[, .(reg_stat_nosauk, adm_terit_nosauk, n, small, merge)]
tab_adm_terit[, .(reg_stat_kods, adm_terit_kods)]

tab_adm_terit[, reg_label := paste(sub(" .*$", "", reg_stat_nosauk), "reg.")]

tab_adm_terit[, AREAVAR2 := cumsum(!small)]
tab_adm_terit[, AREAVAR2_label := ifelse(
  test = !merge,
  yes = paste(reg_label, adm_terit_nosauk),
  no = paste(reg_label, "other")
)]

tab_adm_terit_min <- tab_adm_terit[, .(adm_terit_kods, AREAVAR2)]
tab_AREAVAR2 <- tab_adm_terit[, .(AREAVAR2, AREAVAR2_label)] |> unique()
openxlsx::write.xlsx(
  x = tab_AREAVAR2,
  file = "tables/tab_AREAVAR2.xlsx",
  colWidths = "auto"
)

# dat[, AREAVAR2 := as.integer(adm_terit_kods)]

dat <- merge(dat, tab_adm_terit_min, by = "adm_terit_kods", all.x = T, sort = F)
dat[, .N, keyby = .(AREAVAR2)]

dat_sdif <- merge(dat_sdif, dat[, .(CASEID, AREAVAR2)],
                  by = "CASEID", all.x = T)

# URBRUR
dat_sdif[, .(URBRUR)]
dat_sdif[, AREAVAR3 := URBRUR]
dat_sdif[, .N, keyby = .(AREAVAR3)]

# DEGURBA
# https://ec.europa.eu/eurostat/web/degree-of-urbanisation/methodology
# Cities
# Towns and suburbs
# Rural areas
# 
# https://ec.europa.eu/eurostat/web/nuts/local-administrative-units
# Local Administrative Units (LAU)
# Correspondence table LAU – NUTS 2021, EU-27 and EFTA / available Candidate Countries
# https://ec.europa.eu/eurostat/documents/345175/501971/EU-27-LAU-2022-NUTS-2021.xlsx
dat_degurba <- openxlsx::read.xlsx(
  xlsxFile = "eurostat/EU-27-LAU-2022-NUTS-2021.xlsx",
  sheet = "LV"
)
setDT(dat_degurba)

dat_degurba[, adm_terit_kods := substr(LAU.CODE, 1, 4)]
dat_degurba[, .(adm_terit_kods, DEGURBA)]

dat <- merge(dat, dat_degurba[, .(adm_terit_kods, DEGURBA)],
             by = "adm_terit_kods", all.x = T, sort = F)
rm(dat_degurba)

dat[, AREAVAR4 := DEGURBA]
dat[, .(CASEID, AREAVAR4)]
dat[, .N, keyby = .(AREAVAR4)]

dat[, .N, keyby = .(DEGURBA, adm_terit_kods, adm_terit_nosauk)]


# Unemployment level in age group 15-64 by JURISDICTION
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__EMP__NBB__NBA/EKA011/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/EMP/NBB/NBA/EKA011"

# Disable SSL verification if needed
# SSL certificate was not available on 2023-02-26
if (inherits(x = try(pxweb_get(url = api_url), silent = T),
             what = "try-error")) {
  httr::set_config(httr::config(ssl_verifypeer = 0L))
}

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
grep("LV[0-9]{7}", api_meta$variables[[1]]$values, value = T)

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

api_meta$variables[[5]]$code
api_meta$variables[[5]]$values
grep(pattern = paste0("Y", seq(15, 60, 5), collapse = "|"),
     x = api_meta$variables[[5]]$values,
     value = T)

api_meta$variables[[6]]$code
api_meta$variables[[6]]$values

api_meta$variables[[7]]$code
api_meta$variables[[7]]$values

dat_AREAVAR5 <- pxweb_get_data(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "LV[0-9]{7}",
                x = api_meta$variables[[1]]$values,
                value = T),
    SEX = "T",
    LABOUR_STATUS = c("ACT", "UNE"),
    ETHNICITY = "TOTAL",
    AgeGroup = grep(pattern = paste0("Y", seq(15, 60, 5), collapse = "|"),
                    x = api_meta$variables[[5]]$values,
                    value = T),
    ContentsCode = "EKA011",
    TIME = "2022")),
  column.name.type = "code",
  variable.value.type = "code"
)

setDT(dat_AREAVAR5)
setnames(dat_AREAVAR5, old = length(dat_AREAVAR5), new = "value")
setnames(dat_AREAVAR5, tolower(names(dat_AREAVAR5)))
dat_AREAVAR5

dat_AREAVAR5[, .N, keyby = .(agegroup)]

dat_AREAVAR5[substr(area, 7, 8) == "00", adm_terit_kods := substr(area, 3, 6)]
dat_AREAVAR5[substr(area, 7, 8) != "00", atvk           := substr(area, 3, 9)]

dat_AREAVAR5 <- dat_AREAVAR5[, .(value = sum(value)),
                             keyby = .(adm_terit_kods, atvk, labour_status)]

dat_AREAVAR5 <- dcast.data.table(
  data = dat_AREAVAR5,
  formula = adm_terit_kods + atvk ~ labour_status,
  value.var = "value"
)

dat_AREAVAR5[, AREAVAR5 := UNE / ACT]
dat_AREAVAR5[, summary(AREAVAR5)]
dat_AREAVAR5[, c("ACT", "UNE") := NULL]

dat <- merge(
  x = dat,
  y = dat_AREAVAR5[!is.na(adm_terit_kods),
                   .(adm_terit_kods, AREAVAR5_adm = AREAVAR5)],
  by = "adm_terit_kods",
  all.x = T,
  sort = F
)

dat <- merge(
  x = dat,
  y = dat_AREAVAR5[!is.na(atvk), .(atvk, AREAVAR5_ter = AREAVAR5)],
  by = "atvk",
  all.x = T,
  sort = F
)

rm(dat_AREAVAR5)

dat[!is.na(AREAVAR5_ter)]
dat[, AREAVAR5_val := fifelse(is.na(AREAVAR5_ter), AREAVAR5_adm, AREAVAR5_ter)]
dat[!is.na(AREAVAR5_ter)]

dat[, c("AREAVAR5_adm", "AREAVAR5_ter") := NULL]


# Function to categorise or recode to N2 format
categorise <- function(x, n = 100L) {
  cut(x = x, breaks = n, labels = FALSE, right = FALSE) - 1L
}

# Pretty range
prettyRange <- function(x, fmt = "%.3f") {
  paste0("[", paste(sprintf(fmt = fmt, range(x)), collapse = ";"), "]")
}

# Copy table
copy.table <- function(x) {
  tab <- dat[, .(label = prettyRange(get(paste0(x, "_val")))), keyby = x]
  print(tab)
  clipr::write_clip(content = tab, col.names = FALSE)
}

dat[, AREAVAR5 := categorise(AREAVAR5_val)]
dat[, .N, keyby = .(AREAVAR5)][, P := round(N / sum(N), 2)][]
dat[, cor(AREAVAR5_val, AREAVAR5)]

# dat[, .(label = prettyRange(AREAVAR5)), keyby = "AREAVAR5"]
# dat[, .(label = prettyRange(get("AREAVAR5"))), keyby = "AREAVAR5"]

copy.table("AREAVAR5")



# Share of population aged 15 and over with upper secondary education or higher (ISCED 3-8) by cities, towns, rural territories and neighbourhoods
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__IZG__IZ__IZI/IZT041/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/IZG/IZ/IZI/IZT041"

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
grep("^(TOTAL|ED[345])", api_meta$variables[[1]]$values, value = T)

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
grep("LVDPA", api_meta$variables[[2]]$values, value = T, invert = T)
grep("^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
     api_meta$variables[[2]]$values, value = T)

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

dat_PERSVAR2 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    EDUCATION_LEVEL = "*",
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[2]]$values,
                value = T),
    ContentsCode = "IZT041",
    TIME = "2022"))
)

dat_PERSVAR2$columns
col_names <- purrr::map_chr(
  .x = dat_PERSVAR2$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_PERSVAR2 <- purrr::map(
  .x = dat_PERSVAR2$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_PERSVAR2)
setnames(dat_PERSVAR2, col_names)
dat_PERSVAR2

dat_PERSVAR2[, .N, keyby = .(value)]
# Non numeric values
dat_PERSVAR2[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_PERSVAR2[dat_PERSVAR2[grep("\\(.*\\)", value), .(area)], on = "area"]
dat_PERSVAR2[!grep("^[0-9]+$", value), value := NA]
dat_PERSVAR2[, value := as.integer(value)]

dat_PERSVAR2[, .N, keyby = .(education_level)]
dat_PERSVAR2[grep("^ED[02]",  education_level), education_level := "ED0_2"]
dat_PERSVAR2[grep("^ED[345]", education_level), education_level := "ED3_8"]
dat_PERSVAR2[, .N, keyby = .(education_level)]

dat_PERSVAR2 <- dcast.data.table(data = dat_PERSVAR2,
                                 formula = area ~ education_level,
                                 fun.aggregate = sum,
                                 value.var = "value")

dat_PERSVAR2[, PERSVAR2 := ED3_8 / TOTAL]
dat_PERSVAR2[is.na(PERSVAR2)]

dat_PERSVAR2[, as.list(summary(PERSVAR2))]

dat_PERSVAR2_ter <- dat_PERSVAR2[
  grep("LV[0-9]{7}", area),
  .(atvk_2022 = area, PERSVAR2_ter = PERSVAR2)
]

dat_PERSVAR2_apk <- dat_PERSVAR2[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, PERSVAR2_apk = PERSVAR2)
]

# dat[, .(atvk)]

dat <- merge(dat, dat_PERSVAR2_ter, by = "atvk_2022", all.x = T, sort = F)
dat <- merge(dat, dat_PERSVAR2_apk, by = "apk_kods",  all.x = T, sort = F)
rm(dat_PERSVAR2, dat_PERSVAR2_apk, dat_PERSVAR2_ter)

dat[, PERSVAR2_val := fifelse(is.na(PERSVAR2_apk), PERSVAR2_ter, PERSVAR2_apk)]
dat[, c("PERSVAR2_ter", "PERSVAR2_apk") := NULL]
dat[, as.list(summary(PERSVAR2_val))]
dat[is.na(PERSVAR2_val)]

dat[, PERSVAR2 := categorise(PERSVAR2_val)]
dat[, .N, keyby = .(PERSVAR2)][, P := round(prop.table(N), 3)][]
dat[, cor(PERSVAR2_val, PERSVAR2)]
copy.table("PERSVAR2")
# 33 + 82

# Share of owner occupied dwellings by JURISDICTION
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__MA__MAS/MAS040/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/POP/MA/MAS/MAS040"

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
# Administrative teritories according to 2021-01-01
grep("LV[0-9]{7}", api_meta$variables[[1]]$values, value = T) |> length()
grep("LV[A-Z]{3}[0-9]{2}", api_meta$variables[[1]]$values, value = T) |> length()

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
api_meta$variables[[2]]$valueTexts

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

dat_PERSVAR3 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[1]]$values,
                value = T),
    OWS = c("DW_OC", "DW_OWN"),
    ContentsCode = "MAS040",
    TIME = "2021"))
)

dat_PERSVAR3$columns
col_names <- purrr::map_chr(
  .x = dat_PERSVAR3$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_PERSVAR3 <- purrr::map(
  .x = dat_PERSVAR3$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_PERSVAR3)
setnames(dat_PERSVAR3, col_names)
dat_PERSVAR3

dat_PERSVAR3[, .N, keyby = .(value)]
# Non numeric values
dat_PERSVAR3[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_PERSVAR3[dat_PERSVAR3[grep("\\.", value), .(area)], on = "area"]
dat_PERSVAR3[!grep("^[0-9]+$", value), value := NA]
dat_PERSVAR3[, value := as.integer(value)]

dat_PERSVAR3 <- dcast.data.table(
  data = dat_PERSVAR3,
  formula = area ~ ows,
  value.var = "value"
)

dat_PERSVAR3[is.na(DW_OWN)]
dat_PERSVAR3[, lapply(.SD, sum, na.rm = T), .SDcols = patterns("DW"),
             keyby = .(nchar(area))]
dat_PERSVAR3[, PERSVAR3 := DW_OWN / DW_OC]
dat_PERSVAR3[, summary(PERSVAR3)]

dat_PERSVAR3_ter <- dat_PERSVAR3[
  grep("LV[0-9]{7}", area),
  .(adm_terit_kods_2021 = area, PERSVAR3_ter = PERSVAR3)
]

dat_PERSVAR3_apk <- dat_PERSVAR3[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, PERSVAR3_apk = PERSVAR3)
]

dat <- merge(dat, dat_PERSVAR3_ter, by = "adm_terit_kods_2021",
             all.x = T, sort = F)
dat <- merge(dat, dat_PERSVAR3_apk, by = "apk_kods", all.x = T, sort = F)
rm(dat_PERSVAR3, dat_PERSVAR3_apk, dat_PERSVAR3_ter)

dat[is.na(PERSVAR3_ter)]

dat[, PERSVAR3_val := fifelse(is.na(PERSVAR3_apk), PERSVAR3_ter, PERSVAR3_apk)]
dat[, c("PERSVAR3_ter", "PERSVAR3_apk") := NULL]
dat[, as.list(summary(PERSVAR3_val))]


dat[, .N, keyby = .(PERSVAR3_val)][, P := round(prop.table(N), 3)][]
dat[, PERSVAR3 := categorise(PERSVAR3_val)]
dat[, .N, keyby = .(PERSVAR3)][, P := round(prop.table(N), 3)][]
dat[, cor(PERSVAR3_val, PERSVAR3)]
copy.table("PERSVAR3")
# 115 + 60


# Share of occupied dwellings by JURISDICTION
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__MA__MAS/MAS070/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/POP/MA/MAS/MAS070"

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
# Administrative teritories according to 2021-01-01
grep("LV[0-9]{7}", api_meta$variables[[1]]$values, value = T) |> length()
grep("LV[A-Z]{3}[0-9]{2}", api_meta$variables[[1]]$values, value = T) |> length()

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
api_meta$variables[[2]]$valueTexts

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
api_meta$variables[[3]]$valueTexts

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values
api_meta$variables[[4]]$valueTexts

api_meta$variables[[5]]$code
api_meta$variables[[5]]$values
api_meta$variables[[5]]$valueTexts

api_meta$variables[[6]]$code
api_meta$variables[[6]]$values
api_meta$variables[[6]]$valueTexts

dat_PERSVAR4 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[1]]$values,
                value = T),
    TOB = "TOTAL",
    OCS = "*",
    POC = "TOTAL",
    ContentsCode = "MAS070",
    TIME = "2021"))
)

dat_PERSVAR4$columns
col_names <- purrr::map_chr(
  .x = dat_PERSVAR4$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_PERSVAR4 <- purrr::map(
  .x = dat_PERSVAR4$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_PERSVAR4)
setnames(dat_PERSVAR4, col_names)
dat_PERSVAR4

dat_PERSVAR4[, .N, keyby = .(value)]
# Non numeric values
dat_PERSVAR4[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_PERSVAR4[dat_PERSVAR4[grep("\\(.*\\)", value), .(area)], on = "area"]
dat_PERSVAR4[!grep("^[0-9]+$", value), value := NA]
dat_PERSVAR4[, value := as.integer(value)]

dat_PERSVAR4 <- dcast.data.table(
  data = dat_PERSVAR4,
  formula = area ~ ocs,
  value.var = "value"
)

dat_PERSVAR4[is.na(DW)]
dat_PERSVAR4[is.na(DW_OC)]
dat_PERSVAR4[, lapply(.SD, sum, na.rm = T), .SDcols = patterns("DW"),
             keyby = .(nchar(area))]
dat_PERSVAR4[, PERSVAR4 := DW_OC / DW]
dat_PERSVAR4[, summary(PERSVAR4)]

dat_PERSVAR4_ter <- dat_PERSVAR4[
  grep("LV[0-9]{7}", area),
  .(adm_terit_kods_2021 = area, PERSVAR4_ter = PERSVAR4)
]

dat_PERSVAR4_apk <- dat_PERSVAR4[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, PERSVAR4_apk = PERSVAR4)
]

dat <- merge(dat, dat_PERSVAR4_ter, by = "adm_terit_kods_2021",
             all.x = T, sort = F)
dat <- merge(dat, dat_PERSVAR4_apk, by = "apk_kods", all.x = T, sort = F)
rm(dat_PERSVAR4, dat_PERSVAR4_apk, dat_PERSVAR4_ter)

dat[is.na(PERSVAR4_ter)]

dat[, PERSVAR4_val := fifelse(is.na(PERSVAR4_apk), PERSVAR4_ter, PERSVAR4_apk)]
dat[, c("PERSVAR4_ter", "PERSVAR4_apk") := NULL]
dat[, as.list(summary(PERSVAR4_val))]

dat[, PERSVAR4 := categorise(PERSVAR4_val)]
dat[, .N, keyby = .(PERSVAR4)][, P := round(prop.table(N), 3)][]
dat[, cor(PERSVAR4_val, PERSVAR4)]
copy.table("PERSVAR4")
# 175 + 73


# Share of citizens of Latvia by cities, towns, rural territories and neighbourhoods
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__IR__IRV/IRV030/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/POP/IR/IRV/IRV030"

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
api_meta$variables[[1]]$valueTexts

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
api_meta$variables[[2]]$valueTexts

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
grep("LVDPA", api_meta$variables[[3]]$values, value = T, invert = T)
grep("^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
     api_meta$variables[[3]]$values, value = T)

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values
api_meta$variables[[4]]$valueTexts

api_meta$variables[[5]]$code
api_meta$variables[[5]]$values
api_meta$variables[[5]]$valueTexts

dat_PERSVAR5 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    SEX = "T",
    CITIZEN = c("TOTAL", "CITIZ_LV"),
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[3]]$values,
                value = T),
    ContentsCode = "IRV030",
    TIME = "2022"))
)

dat_PERSVAR5$columns
col_names <- purrr::map_chr(
  .x = dat_PERSVAR5$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_PERSVAR5 <- purrr::map(
  .x = dat_PERSVAR5$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_PERSVAR5)
setnames(dat_PERSVAR5, col_names)
dat_PERSVAR5

dat_PERSVAR5[, .N, keyby = .(value)]
# Non numeric values
dat_PERSVAR5[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_PERSVAR5[dat_PERSVAR5[!grep("^[0-9]+$", value), .(area)], on = "area"]
dat_PERSVAR5[!grep("^[0-9]+$", value), value := NA]
dat_PERSVAR5[, value := as.integer(value)]

dat_PERSVAR5 <- dcast.data.table(data = dat_PERSVAR5,
                                 formula = area ~ citizen,
                                 value.var = "value")

dat_PERSVAR5[, PERSVAR5 := CITIZ_LV / TOTAL]
dat_PERSVAR5[is.na(PERSVAR5)]

dat_PERSVAR5[, as.list(summary(PERSVAR5))]

dat_PERSVAR5_ter <- dat_PERSVAR5[
  grep("LV[0-9]{7}", area),
  .(atvk_2021 = area, PERSVAR5_ter = PERSVAR5)
]

dat_PERSVAR5_apk <- dat_PERSVAR5[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, PERSVAR5_apk = PERSVAR5)
]

dat <- merge(dat, dat_PERSVAR5_ter, by = "atvk_2021", all.x = T, sort = F)
dat <- merge(dat, dat_PERSVAR5_apk, by = "apk_kods",  all.x = T, sort = F)
rm(dat_PERSVAR5, dat_PERSVAR5_apk, dat_PERSVAR5_ter)

dat[, PERSVAR5_val := fifelse(is.na(PERSVAR5_apk), PERSVAR5_ter, PERSVAR5_apk)]
dat[, c("PERSVAR5_ter", "PERSVAR5_apk") := NULL]
dat[, as.list(summary(PERSVAR5_val))]

dat[, PERSVAR5 := categorise(PERSVAR5_val)]
dat[, .N, keyby = .(PERSVAR5)][, P := round(prop.table(N), 3)][]
dat[, cor(PERSVAR5_val, PERSVAR5)]
copy.table("PERSVAR5")
# 248 + 68

# Share of one person households by JURISDICTION
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__MV__MVS/MVS041/

api_url <- "https://data.stat.gov.lv:443/api/v1/en/OSP_PUB/START/POP/MV/MVS/MVS041"

api_meta <- pxweb_get(url = api_url)
api_meta$title
length(api_meta$variables)

api_meta$variables[[1]]$code
api_meta$variables[[1]]$values
grep("LV[0-9]{7}", api_meta$variables[[1]]$values, value = T)

api_meta$variables[[2]]$code
api_meta$variables[[2]]$values
api_meta$variables[[2]]$valueTexts

api_meta$variables[[3]]$code
api_meta$variables[[3]]$values
api_meta$variables[[3]]$valueTexts

api_meta$variables[[4]]$code
api_meta$variables[[4]]$values

dat_DUVAR_SCRRESP3 <- pxweb_get_data(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "LV[0-9]{7}",
                x = api_meta$variables[[1]]$values,
                value = T),
    SPH = c("TOTAL", "1"),
    ContentsCode = "MVS041",
    TIME = "2023")),
  column.name.type = "code",
  variable.value.type = "code"
)

setDT(dat_DUVAR_SCRRESP3)
setnames(dat_DUVAR_SCRRESP3, old = length(dat_DUVAR_SCRRESP3), new = "value")
setnames(dat_DUVAR_SCRRESP3, tolower(names(dat_DUVAR_SCRRESP3)))
dat_DUVAR_SCRRESP3

dat_DUVAR_SCRRESP3[substr(area, 7, 8) == "00", adm_terit_kods := substr(area, 3, 6)]
dat_DUVAR_SCRRESP3[substr(area, 7, 8) != "00", atvk           := substr(area, 3, 9)]

dat_DUVAR_SCRRESP3 <- dcast.data.table(data = dat_DUVAR_SCRRESP3,
                                  formula = adm_terit_kods + atvk ~ sph,
                                  value.var = "value")

dat_DUVAR_SCRRESP3[, DUVAR_SCRRESP3 := `1` / TOTAL]
dat_DUVAR_SCRRESP3[, summary(DUVAR_SCRRESP3)]
dat_DUVAR_SCRRESP3[, c("1", "TOTAL") := NULL]

dat <- merge(
  x = dat,
  y = dat_DUVAR_SCRRESP3[!is.na(adm_terit_kods),
                    .(adm_terit_kods, DUVAR_SCRRESP3_adm = DUVAR_SCRRESP3)],
  by = "adm_terit_kods",
  all.x = T,
  sort = F
)

dat <- merge(
  x = dat,
  y = dat_DUVAR_SCRRESP3[!is.na(atvk), .(atvk, DUVAR_SCRRESP3_ter = DUVAR_SCRRESP3)],
  by = "atvk",
  all.x = T,
  sort = F
)

rm(dat_DUVAR_SCRRESP3)

dat[!is.na(DUVAR_SCRRESP3_ter)]
dat[, DUVAR_SCRRESP3_val := fifelse(is.na(DUVAR_SCRRESP3_ter), DUVAR_SCRRESP3_adm, DUVAR_SCRRESP3_ter)]
dat[!is.na(DUVAR_SCRRESP3_ter)]

dat[, c("DUVAR_SCRRESP3_adm", "DUVAR_SCRRESP3_ter") := NULL]

dat[, DUVAR_SCRRESP3 := categorise(DUVAR_SCRRESP3_val)]
dat[, .N, keyby = .(DUVAR_SCRRESP3)][, P := round(prop.table(N), 3)][]
dat[, cor(DUVAR_SCRRESP3_val, DUVAR_SCRRESP3)]
copy.table("DUVAR_SCRRESP3")
# 319 + 31


# Add to SDIF

dat_sdif <- merge(
  x = dat_sdif,
  y = dat[, .(CASEID, AREAVAR4, AREAVAR5, PERSVAR2, PERSVAR3, PERSVAR4,
              PERSVAR5, DUVAR_SCRRESP3)],
  by = "CASEID",
  all.x = T
)

dat_sdif[, .N, keyby = .(AREAVAR3)]
dat_sdif[, .N, keyby = .(AREAVAR4)]
dat_sdif[, .N, keyby = .(AREAVAR3, AREAVAR4)]

dat_sdif[, paste(range(SORT_PSU), collapse = "-")] |> cat("\n")
dat_sdif[, paste(range(SORT_HH),  collapse = "-")] |> cat("\n")

dat_sdif[, paste(round(range(AREAVAR5),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(PERSVAR2),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(PERSVAR3),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(PERSVAR4),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(PERSVAR5),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(DUVAR_SCRRESP3), 3), collapse = "-")] |> cat("\n")

dat[, .N, keyby = .(reg_stat_kods, reg_stat_nosauk)]$reg_stat_nosauk |>
  paste(collapse = "\n") |> cat()


dat[order(AREAVAR2), .(AREAVAR2, adm_terit_nosauk)] |> unique() |>
  clipr::write_clip(col.names = FALSE)



# Extra variables

# PROB_OVERALL_HH
# Overall probability of selection of HH
# 14,12
dat_sdif[, PROB_OVERALL_HH := round(PROB_PSU * PROB_HH, 12)]
all.equal(dat_sdif[, sum(1 / PROB_OVERALL_HH)], dat_frame[, .N])

# THEOR_HBWT
# Theoretical base weight for selected HH
# (inverse overall selection probability of HH)
# 13,6
dat_sdif[, THEOR_HBWT := round(1 / PROB_OVERALL_HH, 6)]
all.equal(dat_sdif[, sum(THEOR_HBWT)], dat_frame[, .N])

# TRIMGRPS
# Trimming domains
# Required. Identifies the groups for computing the trimming factor. It should be consistent with oversampling domains or strata. Set to 1 if no oversampling.
dat_sdif[, TRIMGRPS := 1L]


# IFLG_PERSVAR[2-5]
# Imputation flag for PERSVAR[2-5]
# Required if any values of PERSVAR[2-5] were imputed for weighting; 1 = the value was imputed; 0 = not imputed. 
dat_sdif[, IFLG_PERSVAR2 := 0L]
dat_sdif[, IFLG_PERSVAR3 := 0L]
dat_sdif[, IFLG_PERSVAR4 := 0L]
dat_sdif[, IFLG_PERSVAR5 := 0L]

# IFLG_DUVAR_SCRRESP3
# Imputation flag for IFLG_DUVAR_SCRRESP3
# Required if any values of DUVAR_SCRRESP3 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif[, IFLG_DUVAR_SCRRESP3 := 0L]

# IFLG_DUVAR_ALL1
# Imputation flag for DUVAR_ALL1
# Required if any values of DUVAR_ALL1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif[, IFLG_DUVAR_ALL1 := 0L]
dat_sdif[, IFLG_DUVAR_ALL2 := 0L]

# IFLG_AREAVAR1
# Imputation flag for AREAVAR1
# Required if any values of AREAVAR1 were imputed for weighting; 1 = value was imputed; 0 = not imputed.
dat_sdif[, IFLG_AREAVAR1 := 0L]
dat_sdif[, IFLG_AREAVAR2 := 0L]
dat_sdif[, IFLG_AREAVAR3 := 0L]
dat_sdif[, IFLG_AREAVAR4 := 0L]
dat_sdif[, IFLG_AREAVAR5 := 0L]



# Save

fwrite(x = dat_sdif, file = "data2/sample_piaac_sdif.csv",  yaml = F)
fwrite(x = dat_sdif, file = "data2/sample_piaac_sdif.csvy", yaml = T)
openxlsx::write.xlsx(x = dat_sdif, file = "data2/sample_piaac_sdif.xlsx",
                     overwrite = T)
