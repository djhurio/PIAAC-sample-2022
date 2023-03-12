# Weighting and non-response analyses variables

# Reset
rm(list = ls())

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

dat[is.na(atvk_2021), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(atvk_2021), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]

dat[is.na(adm_terit_kods_2021), .(atvk_nosauk, paste(lat, lon, sep = ","))]
dat[is.na(adm_terit_kods_2021), .(adrese_ir, paste(lks92x, lks92y, sep = ","))]

# dat[, .N, keyby = .(adm_terit_kods, adm_terit_kods_2021)] |> View()
dat[, .N, keyby = .(adm_terit_kods)]
dat[, .N, keyby = .(adm_terit_kods_2021)]

# stop()

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
dat[, AREAVAR2 := as.integer(adm_terit_kods)]
dat_sdif <- merge(dat_sdif, dat[, .(CASEID, AREAVAR2)],
                  by = "CASEID", all.x = T)

# URBRUR
dat_sdif[, .(URBRUR)]
dat_sdif[, AREAVAR3 := URBRUR]

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
    TIME = "2021")),
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

dat_AREAVAR6 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    EDUCATION_LEVEL = "*",
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[2]]$values,
                value = T),
    ContentsCode = "IZT041",
    TIME = "2021"))
)

dat_AREAVAR6$columns
col_names <- purrr::map_chr(
  .x = dat_AREAVAR6$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_AREAVAR6 <- purrr::map(
  .x = dat_AREAVAR6$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_AREAVAR6)
setnames(dat_AREAVAR6, col_names)
dat_AREAVAR6

dat_AREAVAR6[, .N, keyby = .(value)]
# Non numeric values
dat_AREAVAR6[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_AREAVAR6[dat_AREAVAR6[grep("\\(.*\\)", value), .(area)], on = "area"]
dat_AREAVAR6[!grep("^[0-9]+$", value), value := NA]
dat_AREAVAR6[, value := as.integer(value)]

dat_AREAVAR6[, .N, keyby = .(education_level)]
dat_AREAVAR6[grep("^ED[02]",  education_level), education_level := "ED0_2"]
dat_AREAVAR6[grep("^ED[345]", education_level), education_level := "ED3_8"]
dat_AREAVAR6[, .N, keyby = .(education_level)]

dat_AREAVAR6 <- dcast.data.table(data = dat_AREAVAR6,
                                 formula = area ~ education_level,
                                 fun.aggregate = sum,
                                 value.var = "value")

dat_AREAVAR6[, AREAVAR6 := ED3_8 / TOTAL]
dat_AREAVAR6[is.na(AREAVAR6)]

dat_AREAVAR6[, as.list(summary(AREAVAR6))]

dat_AREAVAR6_ter <- dat_AREAVAR6[
  grep("LV[0-9]{7}", area),
  .(atvk = substr(area, 3, 9), AREAVAR6_ter = AREAVAR6)
]

dat_AREAVAR6_apk <- dat_AREAVAR6[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, AREAVAR6_apk = AREAVAR6)
]

dat <- merge(dat, dat_AREAVAR6_ter, by = "atvk",     all.x = T, sort = F)
dat <- merge(dat, dat_AREAVAR6_apk, by = "apk_kods", all.x = T, sort = F)
rm(dat_AREAVAR6, dat_AREAVAR6_apk, dat_AREAVAR6_ter)

dat[, AREAVAR6_val := fifelse(is.na(AREAVAR6_apk), AREAVAR6_ter, AREAVAR6_apk)]
dat[, c("AREAVAR6_ter", "AREAVAR6_apk") := NULL]
dat[, as.list(summary(AREAVAR6_val))]

dat[, AREAVAR6 := categorise(AREAVAR6_val)]
dat[, .N, keyby = .(AREAVAR6)][, P := round(prop.table(N), 3)][]
dat[, cor(AREAVAR6_val, AREAVAR6)]
copy.table("AREAVAR6")


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

dat_AREAVAR7 <- pxweb_get(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "^LV([0-9]{7}|[A-Z]{3}[0-9]{2})$",
                x = api_meta$variables[[1]]$values,
                value = T),
    OWS = c("DW_OC", "DW_OWN"),
    ContentsCode = "MAS040",
    TIME = "2021"))
)

dat_AREAVAR7$columns
col_names <- purrr::map_chr(
  .x = dat_AREAVAR7$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_AREAVAR7 <- purrr::map(
  .x = dat_AREAVAR7$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_AREAVAR7)
setnames(dat_AREAVAR7, col_names)
dat_AREAVAR7

dat_AREAVAR7[, .N, keyby = .(value)]
# Non numeric values
dat_AREAVAR7[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_AREAVAR7[dat_AREAVAR7[grep("\\.", value), .(area)], on = "area"]
dat_AREAVAR7[!grep("^[0-9]+$", value), value := NA]
dat_AREAVAR7[, value := as.integer(value)]

dat_AREAVAR7 <- dcast.data.table(
  data = dat_AREAVAR7,
  formula = area ~ ows,
  value.var = "value"
)

dat_AREAVAR7[is.na(DW_OWN)]
dat_AREAVAR7[, lapply(.SD, sum, na.rm = T), .SDcols = patterns("DW"),
             keyby = .(nchar(area))]
dat_AREAVAR7[, AREAVAR7 := DW_OWN / DW_OC]
dat_AREAVAR7[, summary(AREAVAR7)]

dat_AREAVAR7_ter <- dat_AREAVAR7[
  grep("LV[0-9]{7}", area),
  .(adm_terit_kods_2021 = area, AREAVAR7_ter = AREAVAR7)
]

dat_AREAVAR7_apk <- dat_AREAVAR7[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, AREAVAR7_apk = AREAVAR7)
]

dat <- merge(dat, dat_AREAVAR7_ter, by = "adm_terit_kods_2021", all.x = T, sort = F)
dat <- merge(dat, dat_AREAVAR7_apk, by = "apk_kods", all.x = T, sort = F)
rm(dat_AREAVAR7, dat_AREAVAR7_apk, dat_AREAVAR7_ter)

dat[is.na(AREAVAR7_ter)]

dat[, AREAVAR7_val := fifelse(is.na(AREAVAR7_apk), AREAVAR7_ter, AREAVAR7_apk)]
dat[, c("AREAVAR7_ter", "AREAVAR7_apk") := NULL]
dat[, as.list(summary(AREAVAR7_val))]


dat[, .N, keyby = .(AREAVAR7_val)][, P := round(prop.table(N), 3)][]
dat[, AREAVAR7 := categorise(AREAVAR7_val)]
dat[, .N, keyby = .(AREAVAR7)][, P := round(prop.table(N), 3)][]
dat[, cor(AREAVAR7_val, AREAVAR7)]
copy.table("AREAVAR7")


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

dat_AREAVAR8 <- pxweb_get(
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

dat_AREAVAR8$columns
col_names <- purrr::map_chr(
  .x = dat_AREAVAR8$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_AREAVAR8 <- purrr::map(
  .x = dat_AREAVAR8$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_AREAVAR8)
setnames(dat_AREAVAR8, col_names)
dat_AREAVAR8

dat_AREAVAR8[, .N, keyby = .(value)]
# Non numeric values
dat_AREAVAR8[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_AREAVAR8[dat_AREAVAR8[grep("\\(.*\\)", value), .(area)], on = "area"]
dat_AREAVAR8[!grep("^[0-9]+$", value), value := NA]
dat_AREAVAR8[, value := as.integer(value)]

dat_AREAVAR8 <- dcast.data.table(
  data = dat_AREAVAR8,
  formula = area ~ ocs,
  value.var = "value"
)

dat_AREAVAR8[is.na(DW)]
dat_AREAVAR8[is.na(DW_OC)]
dat_AREAVAR8[, lapply(.SD, sum, na.rm = T), .SDcols = patterns("DW"),
             keyby = .(nchar(area))]
dat_AREAVAR8[, AREAVAR8 := DW_OC / DW]
dat_AREAVAR8[, summary(AREAVAR8)]

dat_AREAVAR8_ter <- dat_AREAVAR8[
  grep("LV[0-9]{7}", area),
  .(adm_terit_kods_2021 = area, AREAVAR8_ter = AREAVAR8)
]

dat_AREAVAR8_apk <- dat_AREAVAR8[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, AREAVAR8_apk = AREAVAR8)
]

dat <- merge(dat, dat_AREAVAR8_ter, by = "adm_terit_kods_2021", all.x = T, sort = F)
dat <- merge(dat, dat_AREAVAR8_apk, by = "apk_kods", all.x = T, sort = F)
rm(dat_AREAVAR8, dat_AREAVAR8_apk, dat_AREAVAR8_ter)

dat[is.na(AREAVAR8_ter)]

dat[, AREAVAR8_val := fifelse(is.na(AREAVAR8_apk), AREAVAR8_ter, AREAVAR8_apk)]
dat[, c("AREAVAR8_ter", "AREAVAR8_apk") := NULL]
dat[, as.list(summary(AREAVAR8_val))]

dat[, AREAVAR8 := categorise(AREAVAR8_val)]
dat[, .N, keyby = .(AREAVAR8)][, P := round(prop.table(N), 3)][]
dat[, cor(AREAVAR8_val, AREAVAR8)]
copy.table("AREAVAR8")



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

dat_AREAVAR9 <- pxweb_get(
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

dat_AREAVAR9$columns
col_names <- purrr::map_chr(
  .x = dat_AREAVAR9$columns,
  .f = \(x) x$code
) |> tolower()
col_names[length(col_names)] <- "value"
col_names

dat_AREAVAR9 <- purrr::map(
  .x = dat_AREAVAR9$data,
  .f = \(x) c(x$key, x$values)
) |> rbindlist()
setDT(dat_AREAVAR9)
setnames(dat_AREAVAR9, col_names)
dat_AREAVAR9

dat_AREAVAR9[, .N, keyby = .(value)]
# Non numeric values
dat_AREAVAR9[!grep("^[0-9]+$", value), .N, keyby = .(value)]
dat_AREAVAR9[dat_AREAVAR9[!grep("^[0-9]+$", value), .(area)], on = "area"]
dat_AREAVAR9[!grep("^[0-9]+$", value), value := NA]
dat_AREAVAR9[, value := as.integer(value)]

dat_AREAVAR9 <- dcast.data.table(data = dat_AREAVAR9,
                                 formula = area ~ citizen,
                                 value.var = "value")

dat_AREAVAR9[, AREAVAR9 := CITIZ_LV / TOTAL]
dat_AREAVAR9[is.na(AREAVAR9)]

dat_AREAVAR9[, as.list(summary(AREAVAR9))]

dat_AREAVAR9_ter <- dat_AREAVAR9[
  grep("LV[0-9]{7}", area),
  .(atvk_2021 = area, AREAVAR9_ter = AREAVAR9)
]

dat_AREAVAR9_apk <- dat_AREAVAR9[
  grep("LV[A-Z]{3}[0-9]{2}", area),
  .(apk_kods = area, AREAVAR9_apk = AREAVAR9)
]

dat <- merge(dat, dat_AREAVAR9_ter, by = "atvk_2021", all.x = T, sort = F)
dat <- merge(dat, dat_AREAVAR9_apk, by = "apk_kods",  all.x = T, sort = F)
rm(dat_AREAVAR9, dat_AREAVAR9_apk, dat_AREAVAR9_ter)

dat[, AREAVAR9_val := fifelse(is.na(AREAVAR9_apk), AREAVAR9_ter, AREAVAR9_apk)]
dat[, c("AREAVAR9_ter", "AREAVAR9_apk") := NULL]
dat[, as.list(summary(AREAVAR9_val))]

dat[, AREAVAR9 := categorise(AREAVAR9_val)]
dat[, .N, keyby = .(AREAVAR9)][, P := round(prop.table(N), 3)][]
dat[, cor(AREAVAR9_val, AREAVAR9)]
copy.table("AREAVAR9")


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

dat_AREAVAR10 <- pxweb_get_data(
  url = api_url,
  query = pxweb_query(x = list(
    AREA = grep(pattern = "LV[0-9]{7}",
                x = api_meta$variables[[1]]$values,
                value = T),
    SPH = c("TOTAL", "1"),
    ContentsCode = "MVS041",
    TIME = "2022")),
  column.name.type = "code",
  variable.value.type = "code"
)

setDT(dat_AREAVAR10)
setnames(dat_AREAVAR10, old = length(dat_AREAVAR10), new = "value")
setnames(dat_AREAVAR10, tolower(names(dat_AREAVAR10)))
dat_AREAVAR10

dat_AREAVAR10[substr(area, 7, 8) == "00", adm_terit_kods := substr(area, 3, 6)]
dat_AREAVAR10[substr(area, 7, 8) != "00", atvk           := substr(area, 3, 9)]

dat_AREAVAR10 <- dcast.data.table(data = dat_AREAVAR10,
                                  formula = adm_terit_kods + atvk ~ sph,
                                  value.var = "value")

dat_AREAVAR10[, AREAVAR10 := `1` / TOTAL]
dat_AREAVAR10[, summary(AREAVAR10)]
dat_AREAVAR10[, c("1", "TOTAL") := NULL]

dat <- merge(
  x = dat,
  y = dat_AREAVAR10[!is.na(adm_terit_kods),
                    .(adm_terit_kods, AREAVAR10_adm = AREAVAR10)],
  by = "adm_terit_kods",
  all.x = T,
  sort = F
)

dat <- merge(
  x = dat,
  y = dat_AREAVAR10[!is.na(atvk), .(atvk, AREAVAR10_ter = AREAVAR10)],
  by = "atvk",
  all.x = T,
  sort = F
)

rm(dat_AREAVAR10)

dat[!is.na(AREAVAR10_ter)]
dat[, AREAVAR10_val := fifelse(is.na(AREAVAR10_ter), AREAVAR10_adm, AREAVAR10_ter)]
dat[!is.na(AREAVAR10_ter)]

dat[, c("AREAVAR10_adm", "AREAVAR10_ter") := NULL]

dat[, AREAVAR10 := categorise(AREAVAR10_val)]
dat[, .N, keyby = .(AREAVAR10)][, P := round(prop.table(N), 3)][]
dat[, cor(AREAVAR10_val, AREAVAR10)]
copy.table("AREAVAR10")


# Add to SDIF

dat_sdif <- merge(
  x = dat_sdif,
  y = dat[, .(CASEID, AREAVAR4, AREAVAR5, AREAVAR6, AREAVAR7, AREAVAR8,
              AREAVAR9, AREAVAR10)],
  by = "CASEID",
  all.x = T
)

dat_sdif[, .N, keyby = .(AREAVAR3)]
dat_sdif[, .N, keyby = .(AREAVAR4)]
dat_sdif[, .N, keyby = .(AREAVAR3, AREAVAR4)]

dat_sdif[, paste(range(SORT_PSU), collapse = "-")] |> cat("\n")
dat_sdif[, paste(range(SORT_HH),  collapse = "-")] |> cat("\n")

dat_sdif[, paste(round(range(AREAVAR5),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(AREAVAR6),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(AREAVAR7),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(AREAVAR8),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(AREAVAR9),  3), collapse = "-")] |> cat("\n")
dat_sdif[, paste(round(range(AREAVAR10), 3), collapse = "-")] |> cat("\n")

dat[, .N, keyby = .(reg_stat_kods, reg_stat_nosauk)]$reg_stat_nosauk |>
  paste(collapse = "\n") |> cat()


dat[order(AREAVAR2), .(AREAVAR2, adm_terit_nosauk)] |> unique() |>
  clipr::write_clip(col.names = FALSE)


# Save

fwrite(x = dat_sdif, file = "data2/sample_piaac_sdif.csv",  yaml = F)
fwrite(x = dat_sdif, file = "data2/sample_piaac_sdif.csvy", yaml = T)
openxlsx::write.xlsx(x = dat_sdif, file = "data2/sample_piaac_sdif.xlsx",
                     overwrite = T)
