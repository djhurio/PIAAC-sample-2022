# Sample Monitoring Parameter (SMP) file
library(data.table)
library(pxweb)
library(openxlsx)

# Reset
rm(list = ls())

# Rounding by preserving sum of values after rounding
source(file = "code/round_preserve_sum.R")

# PIAAC frame
frame <- fread("data/frame_piaac.csvy.gz", yaml = T)
sampl <- fread("data/sample_piaac.csvy", yaml = T)

# Read template

# CSV
tab_smp1 <- fread("../Deliverables/CY2_MS_SMP_File_LVA.csv")

# XLSX
tab_smp2 <- openxlsx::read.xlsx(
  xlsxFile = "../Deliverables/CY2_MS_SMP_File_LVA.xlsx",
  skipEmptyRows = F
)
setDT(tab_smp2)

all.equal(tab_smp1, tab_smp2)

tab_smp <- copy(tab_smp1)
rm(tab_smp1, tab_smp2)

tab_smp
tab_smp <- rbind(tab_smp, as.list(rep(NA_real_, ncol(tab_smp))))
# str(tab_smp)

# CNTRYID
# Country ID
# Required; ISO 3166 3-digit numeric codes.
# https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
tab_smp[, CNTRYID := 428L]

# DSGN
# Sample design
# Required; 1: Population registry, 2: Screener
tab_smp[, DSGN := 2L]

# SCR_EXP
# Expected number of completed screeners
# Initial person sample size: 14394
tab_smp[, SCR_EXP := 14394L]

# BQ_EXP
# Expected number of completed BQs (full BQ or doorstep interview)
# Expected number of BQs plus doorstep interviews: 9602
tab_smp[, BQ_EXP := 9602L]

# CC_EXP
# Target number of completed cases
# Expected number of completed cases: 7692
tab_smp[, CC_EXP := 7692L]

# MAIN_EXP
# Target number of completed assessments
# PIAAC Standard minimum number of completed assessments: 5000
tab_smp[, MAIN_EXP := 5000L]


# SCRRR_EXP
# Expected screener response rate
# Screener response rate = 75%
# Between 0 and 100.
tab_smp[, SCRRR_EXP := 75]

# BQRR_EXP
# Expected BQ response rate
# BQ response rate among those without a language barrier = 66.7%
tab_smp[, BQRR_EXP := 66.7]

# MAINRR_EXP
# Expected assessment response rate
# Assessment response rate = 80%
tab_smp[, MAINRR_EXP := 80]

# DSPCT_EXP
# Expected percentage of doorstep interviews
# Doorstep Interview response rate = 75%
tab_smp[, DSPCT_EXP := 75]

# SPSCR_EXP
# Expected number of sampled persons per screener complete
# Randomly select 1 person for household sizes up to 3 persons (including 3),
# otherwise 2 persons, international CMS will be used
frame[, .N, keyby = .(pers_sk_16_65)][, sum(N * (1 + (pers_sk_16_65 > 3))) / sum(N)]
frame[, mean(1 + (pers_sk_16_65 > 3))]
tab_smp[, SPSCR_EXP := frame[, round(mean(1 + (pers_sk_16_65 > 3)), 3)]]
tab_smp[, .(SPSCR_EXP)]

# REGION1_EXP
# Expected proportion in Region 1
# 6.4
# N
# Required. REGION1_EXP through REGION25_EXP must sum to 1.
# Region numbers must correspond to those in the SCF.

tab_reg <- frame[, .(reg_stat_kods, reg_nuts3_kods, reg_nuts3_nosauk)] |>
  unique()
setorder(tab_reg)
tab_reg

# PX-WEB
# https://ropengov.github.io/pxweb/articles/pxweb.html

# Divas tabulas no CSP
# 
# Iedzīvotāji pēc dzimuma un vecuma reģionos un republikas pilsētās gada sākumā
# 1971 - 2022
# https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__POP__IR__IRD/IRD040/
# 
# Iedzīvotāji pēc dzimuma un vecuma reģionos, valstspilsētās un novados
# gada sākumā (pēc administratīvi teritoriālās reformas 2021. gadā) –
# Teritoriālā vienība, Vecums, Laika periods un Dzimums
# https://data.stat.gov.lv/pxweb/lv/OSP_PUB/START__POP__IR__IRD/IRD041/

px_url <- c(
  "https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/POP/IR/IRD/IRD040",
  "https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/POP/IR/IRD/IRD041"
)

px_meta <- lapply(px_url, pxweb_get)

px_q <- pxweb_query(x = list(
  AREA = tab_reg$reg_nuts3_kods,
  AGE = paste0("Y", 16:65),
  SEX = c("M", "F"),
  ContentsCode = c("*"),
  TIME = "2022"
))

# pxweb_validate_query_with_metadata(pxq = px_q, pxmd = px_meta[[1]])
# pxweb_validate_query_with_metadata(pxq = px_q, pxmd = px_meta[[2]])
lapply(px_meta, pxweb_validate_query_with_metadata, pxq = px_q)

stat_iedz_IRD040 <- as.data.frame(pxweb_get(url = px_url[1], query = px_q),
                                  column.name.type = "code",
                                  variable.value.type = "code")
stat_iedz_IRD041 <- as.data.frame(pxweb_get(url = px_url[2], query = px_q),
                                  column.name.type = "code",
                                  variable.value.type = "code")

setDT(stat_iedz_IRD040)
setDT(stat_iedz_IRD041)

setnames(stat_iedz_IRD040, ncol(stat_iedz_IRD040), "value")
setnames(stat_iedz_IRD041, ncol(stat_iedz_IRD041), "value")

setnames(stat_iedz_IRD040, tolower(names(stat_iedz_IRD040)))
setnames(stat_iedz_IRD041, tolower(names(stat_iedz_IRD041)))

all.equal(stat_iedz_IRD040, stat_iedz_IRD041)
# Abas tabulas ir vienādas

stat_iedz <- copy(stat_iedz_IRD040)
rm(stat_iedz_IRD040, stat_iedz_IRD041)

# str(stat_iedz)

stat_iedz[, area := factor(area, tab_reg$reg_nuts3_kods, tab_reg$reg_nuts3_kods)]
stat_iedz[, .N, keyby = .(area)]
tab_reg

tab_stat_reg <- stat_iedz[, .(n = sum(value)), keyby = .(area)]
tab_stat_reg[, p := prop.table(n)]
tab_stat_reg

varnames <- grep("^REGION[0-9]+_EXP$", names(tab_smp), value = T)

x <- c(tab_stat_reg$p, rep(0, length(varnames) - nrow(tab_stat_reg))) |>
  round_preserve_sum(4)
x
sum(x)

tab_smp[, c(varnames) := as.list(x)]
tab_smp[, ..varnames]

rm(varnames, x)


# URBRUR1_EXP
# Expected proportion in urban areas
# 6.4
# N
# Required. URBRUR1_EXP and URBRUR2_EXP must sum to 1.

px_url <- c(
  "https://data.stat.gov.lv:443/api/v1/lv/OSP_PUB/START/POP/IR/IRD/IRD081"
)

px_meta <- pxweb_get(px_url)

px_q <- pxweb_query(x = list(
  SEX = "T",
  AgeGroup = "Y15-64",
  AREA = "*",
  ContentsCode = "*",
  TIME = "2022"
))

pxweb_validate_query_with_metadata(pxq = px_q, pxmd = px_meta)

stat_iedz_IRD081 <- as.data.frame(pxweb_get(url = px_url, query = px_q),
                                  column.name.type = "code",
                                  variable.value.type = "code")
setDT(stat_iedz_IRD081)

names(stat_iedz_IRD081)
setnames(stat_iedz_IRD081, ncol(stat_iedz_IRD081), "value")
setnames(stat_iedz_IRD081, tolower(names(stat_iedz_IRD081)))

stat_iedz_IRD081 <- stat_iedz_IRD081[grep("^LV(|[0-9]{7})$", area)]

stat_iedz_IRD081[, atvk1 := substr(area, 3, 4)]
stat_iedz_IRD081[, atvk2 := substr(area, 5, 6)]
stat_iedz_IRD081[, atvk3 := substr(area, 7, 8)]
stat_iedz_IRD081[, atvk4 := substr(area, 9, 9)]

stat_iedz_IRD081[, .N, keyby = .(atvk1)]
stat_iedz_IRD081[, .N, keyby = .(atvk2)]
stat_iedz_IRD081[, .N, keyby = .(atvk3)]
stat_iedz_IRD081[, .N, keyby = .(atvk4)]

# Novadi
stat_iedz_IRD081[atvk2 >= "20" & atvk3 == "00"]
# Izfiltrē novadus
stat_iedz_IRD081 <- stat_iedz_IRD081[atvk2 < "20" | atvk3 > "00"]

# Total LV sakrīt ar summu
stat_iedz_IRD081[, sum(value), keyby = .(area != "LV")]

stat_iedz_IRD081 <- stat_iedz_IRD081[area != "LV"]
stat_iedz_IRD081[, .N, keyby = .(atvk3)]

stat_iedz_IRD081[, urbrur := 1L + (atvk3 >= "40")]

tab_iedz_urbrur <- stat_iedz_IRD081[, .(n = sum(value)), keyby = .(urbrur)]
tab_iedz_urbrur[, p := round_preserve_sum(prop.table(n), 4)]
tab_iedz_urbrur

tmp <- frame[, .(n = sum(pers_sk_16_65)), keyby = .(terit_tips_kods)]
tmp[, p := round_preserve_sum(prop.table(n), 4)]
tmp

tab_smp[, c(sprintf("URBRUR%s_EXP", 1:2)) := as.list(tab_iedz_urbrur$p)]


# AGE0_EXP
# Expected proportion age <16
# 6.4
# N
# Required. Should be equal to 0 if there is no supplemental sample of ages < 16.
# AGE0_EXP through AGE6_EXP must sum to 1.

stat_iedz
stat_iedz[, age := as.integer(sub("Y", "", age))]

stat_iedz[between(age, 16, 25), age_group := 1L]
stat_iedz[between(age, 26, 35), age_group := 2L]
stat_iedz[between(age, 36, 45), age_group := 3L]
stat_iedz[between(age, 46, 55), age_group := 4L]
stat_iedz[between(age, 56, 65), age_group := 5L]
stat_iedz[, .N, keyby = .(age_group)]

tab_iedz_age <- stat_iedz[, .(n = sum(value)), keyby = .(age_group)]
tab_iedz_age[, p := prop.table(n)]

varnames <- grep("^AGE[0-9]+_EXP$", names(tab_smp), value = T)

x <- c(0, tab_iedz_age$p, 0) |> round_preserve_sum(4)
x
sum(x)

tab_smp[, c(varnames) := as.list(x)]
tab_smp[, ..varnames]

rm(varnames, x)


# GENDER1_EXP
# Expected proportion male
# 6.4
# N
# Required. GENDER1_EXP and GENDER2_EXP must sum to 1.

stat_iedz[, sex := factor(sex, c("M", "F"))]

tab_iedz_sex <- stat_iedz[, .(n = sum(value)), keyby = .(sex)]
tab_iedz_sex[, p := round_preserve_sum(prop.table(n), 4)]
tab_iedz_sex
tab_iedz_sex[, sum(p)]

tab_smp[, c(sprintf("GENDER%s_EXP", 1:2)) := as.list(tab_iedz_sex$p)]


# LANG1_EXP
# Expected proportion for assessment language 1
# 6.4
# N
# Required. LANG1_EXP through LANG5_EXP must sum to 1.

# Ārējās migrācijas apsekojuma rezultāti (t.sk. dati par valodām)
# https://stat.gov.lv/lv/statistikas-temas/iedzivotaji/migracija/cits/
# 1590-arejas-migracijas-apsekojuma-rezultati
# csp/AMA_2019_0.xlsx

tab_ama <- openxlsx::read.xlsx(
  xlsxFile = file.path("https://admin.stat.gov.lv/system/files/other_format",
                       "2020-03/AMA_2019_0.xlsx"),
  sheet = "5",
  startRow = 12
)
setDT(tab_ama)
setnames(tab_ama, tolower(names(tab_ama)))

tab_ama <- melt.data.table(data = tab_ama, id.vars = "indicators",
                           na.rm = T, variable.factor = F)
tab_ama <- tab_ama[grep("^[0-9]{2}-[0-9]{2}.y.o.$", variable)]
tab_ama <- tab_ama[indicators != "Other"]

tab_ama[, age_group := as.integer(factor(variable))]
tab_ama[, lang_prop := value / sum(value), by = .(age_group)]

# Viena gada nobīde vecuma grupās
tab_ama_iedz <- merge(tab_iedz_age, tab_ama, by = "age_group")
tab_ama_iedz[, n_lang := n * lang_prop]

tab_lang <- tab_ama_iedz[, .(n = sum(n_lang)), keyby = .(indicators)]
tab_lang[, p := round_preserve_sum(prop.table(n), 4)]
tab_lang
tab_lang[, sum(p)]

varnames <- grep("^LANG[0-9]+_EXP$", names(tab_smp), value = T)

x <- c(tab_lang$p, rep(0, length(varnames) - nrow(tab_lang)))
x
sum(x)

tab_smp[, c(varnames) := as.list(x)]
tab_smp[, ..varnames]

rm(varnames, x)

# LANG1_NAME
# Name of language 1
# 10
# C
# Required; ISO language codes.

varnames <- grep("^LANG[0-9]+_NAME$", names(tab_smp), value = T)
x <- c("lav", "rus", rep("", length(varnames) - 2))

tab_smp[, c(varnames[1]), with = F]

for (i in seq_along(varnames)) {
  set(x = tab_smp, i = NULL, j = varnames[i], value = x[i])
}


# F1 variables - keep default values
tab_smp[, F1_NH := .608]
tab_smp[, F1_CB := .602]
tab_smp[, F1_RB := .397]
tab_smp[, F1_AV := .397]
tab_smp[, F1_IL := .265]
tab_smp[, F1_LO := .265]
tab_smp[, F1_OT := .265]
tab_smp[, F1_AP := .744]
tab_smp[, F1_NW := .650]
tab_smp[, F1_NW_OCC := .858]

# F2 variable - multiply default with .8375
# For example F2_CB = .53 * .8375 = .4444

tab_smp[, F2_NH := round(.443 * .8375, 3)]
tab_smp[, F2_CB := round(.530 * .8375, 3)]
tab_smp[, F2_RB := round(.105 * .8375, 3)]
tab_smp[, F2_AV := round(.105 * .8375, 3)]
tab_smp[, F2_LO := round(.235 * .8375, 3)]
tab_smp[, F2_OT := round(.235 * .8375, 3)]
tab_smp[, F2_AP := round(.848 * .8375, 3)]
tab_smp[, F2_IL := round(.100 * .8375, 3)]
tab_smp[, F2_NW := round(.800 * .8375, 3)]
tab_smp[, F2_SP_SCR := round(1.060 * .8375, 3)]
tab_smp[, F2_BQ_EX := round(.980 * .8375, 3)]


# FT
tab_smp_ft <- copy(tab_smp)

varnames <- c(
  "SCR_EXP",
  "BQ_EXP",
  "CC_EXP",
  "MAIN_EXP"
)

tab_smp_ft[, ..varnames]

sampl[sample_du == 1 & between(SUBSAMP, 1, 5), .N]
sampl[sample_ft == 1 & between(SUBSAMP, 1, 1), .N]

r <- sampl[sample_ft == 1 & between(SUBSAMP, 1, 1), .N] /
  sampl[sample_du == 1 & between(SUBSAMP, 1, 5), .N]

tab_smp_ft[, ..varnames]
tab_smp_ft[, lapply(.SD, \(x) round(x * r)), .SDcols = varnames]

tab_smp_ft[, c(varnames) := lapply(.SD, \(x) round(x * r)), .SDcols = varnames]
tab_smp_ft[, ..varnames]


# Save

fwrite(    x = tab_smp, file = "data/sample_piaac_smp.csv")
write.xlsx(x = tab_smp, file = "data/sample_piaac_smp.xlsx", overwrite = T)

fwrite(    x = tab_smp_ft, file = "data/sample_piaac_smp_ft.csv")
write.xlsx(x = tab_smp_ft, file = "data/sample_piaac_smp_ft.xlsx", overwrite = T)
