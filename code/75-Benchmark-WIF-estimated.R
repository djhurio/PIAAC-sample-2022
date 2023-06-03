# Calibration totals
# Benchmark_WIF
# 
# CY2_Prelim_MS_Benchmark_WIF_LVA.xlsx
# CY2_Prelim_MS_Benchmark_WIF_Codebook_LVA.xlsx
# 
# CY2_Final_MS_Weighting_and_Variance_Estimation_Summary_LVA.docx
# Example_Benchmark_WIF_Codebook.xlsx
# 
# BQ: CALIBRATION
# 
# Potential Calibration Variables
# Age and gender cross classification
# Ethnicity
# Country of birth

# Reset
rm(list = ls())
gc()

# Libs
library(data.table)
library(openxlsx)

# Functions
source("code/round_preserve_sum.R")

sex_labels <- c("Males", "Females")

# Total population (2022-01-01)  - my estimation
# 
# The core PIAAC target population consists of all non-institutionalised adults
# between the ages of 16 and 65 (inclusive) residing in the country at the time
# of data collection.

# Total population by age
# https://data.stat.gov.lv:443/sq/15996
tab_pop_total_age <- fread("https://data.stat.gov.lv:443/sq/15997")
setDT(tab_pop_total_age)
setnames(tab_pop_total_age, length(tab_pop_total_age), "pop1665")
setnames(tab_pop_total_age, tolower)
tab_pop_total_age[, sex := factor(sex, sex_labels, sex_labels)]
setkey(tab_pop_total_age)
tab_pop_total_age

# Total population - private and institutional
# https://data.stat.gov.lv:443/sq/15998
tab_pop_total_pri <- fread("https://data.stat.gov.lv:443/sq/15999")
setDT(tab_pop_total_pri)
setnames(tab_pop_total_pri, length(tab_pop_total_pri), "pop_pri")
setnames(tab_pop_total_pri, tolower)
tab_pop_total_pri[, sex := factor(sex, sex_labels, sex_labels)]
setkey(tab_pop_total_pri)
tab_pop_total_pri

tab_pop_total <- merge(tab_pop_total_age, tab_pop_total_pri)
tab_pop_total

# Estimation
tab_pop_total[, prop_age := pop1665 / total]
tab_pop_total[, prop_pri := pop_pri / total]
tab_pop_total[, pop_trg := round(total * prop_age * prop_pri)]

# Target population by sex
tab_pop_trg_sex <- tab_pop_total[, .(`territorial unit`, `time period`, sex, pop_trg)]
setkey(tab_pop_trg_sex)

# Target population - total
tab_pop_trg_tot <- tab_pop_trg_sex[, .(pop_trg = sum(pop_trg)),
                                   keyby = .(`territorial unit`, `time period`)]

tab_pop_trg_sex
tab_pop_trg_tot
# 1171552

rm(tab_pop_total, tab_pop_total_age, tab_pop_total_pri)


# Population by age and sex
# Age and gender cross classification
# Population statistics (IRD041), 2022
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__IR__IRD/IRD041/
tab_pop_sex_age <- fread("https://data.stat.gov.lv:443/sq/15995")
setDT(tab_pop_sex_age)
setnames(tab_pop_sex_age, length(tab_pop_sex_age), "value")
setnames(tab_pop_sex_age, tolower)
tab_pop_sex_age[, sex := factor(sex, sex_labels, sex_labels)]

x <- seq(16, 61, by = 5)
tab_pop_sex_age[, age_grp := cut(
  x = age, breaks = 10, labels = paste(x, x + 4, sep = "-")
)]
rm(x)
tab_pop_sex_age[, .(n = .N, min = min(age), max = max(age)), keyby = .(age_grp)]

tab_pop_sex_age
tab_pop_sex_age <- tab_pop_sex_age[
  ,
  .(value = sum(value)),
  keyby = .(`territorial unit`, `time period`, sex, age_grp)
]
tab_pop_sex_age

key(tab_pop_sex_age)
key(tab_pop_trg_sex)

tab_pop_sex_age <- merge(tab_pop_sex_age, tab_pop_trg_sex)

tab_pop_sex_age[, RAKEDIM1 := .I]
tab_pop_sex_age[, TOTAL := round_preserve_sum(value * pop_trg / sum(value)),
                by = .(sex)]
tab_pop_sex_age[, Gender := sex]
tab_pop_sex_age[, Age := age_grp]


# Population by Ethnicity
# Population statistics (IRE040), 2022
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__IR__IRE/IRE040/
# https://data.stat.gov.lv:443/sq/16001

tab_pop_eth <- fread("https://data.stat.gov.lv:443/sq/16002")
setDT(tab_pop_eth)
setnames(tab_pop_eth, length(tab_pop_eth), "value")
setnames(tab_pop_eth, trimws)
setnames(tab_pop_eth, tolower)

unique(tab_pop_eth$ethnicity)
eth_labels <- c(
  "Latvians",
  "Russians",
  "Ukrainians",
  "Belarusians",
  "Estonians",
  "Lithuanians",
  "Poles",
  "Jews",
  "Roma",
  "Other ethnicities, including not selected and not indicated ethnicity"
)

tab_pop_eth[, ethnicity := factor(ethnicity, eth_labels, eth_labels)]

tab_pop_eth <- tab_pop_eth[, .(value = sum(value)),
                           keyby = .(`time period`, ethnicity)]

tab_pop_eth <- merge(tab_pop_eth, tab_pop_trg_tot)
setcolorder(tab_pop_eth, c("territorial unit"))

tab_pop_eth[, RAKEDIM2 := .I]
tab_pop_eth[, TOTAL := round_preserve_sum(value * pop_trg / sum(value))]
tab_pop_eth[, Ethnicity := ethnicity]



# Country of birth
# Population statistics (IRV050), 2022
# https://data.stat.gov.lv/pxweb/en/OSP_PUB/START__POP__IR__IRV/IRV050/
# https://data.stat.gov.lv:443/sq/16076

tab_pop_cob <- fread("https://data.stat.gov.lv:443/sq/16077", na.strings = "…")
setDT(tab_pop_cob)
setnames(tab_pop_cob, length(tab_pop_cob), "value")
setnames(tab_pop_cob, tolower)
setorderv(tab_pop_cob, cols = "value", order = -1, na.last = TRUE)
str(tab_pop_cob)
tab_pop_cob

tab_pop_cob[`country of birth` == "TOTAL"]
tab_pop_cob[`country of birth` != "TOTAL"]

tab_pop_cob[, sum(value, na.rm = T), keyby = .(`country of birth` == "TOTAL")]
tab_pop_cob <- tab_pop_cob[!is.na(value)]
tab_pop_cob[, sum(value), keyby = .(`country of birth` == "TOTAL")]

tab_pop_cob <- tab_pop_cob[`country of birth` != "TOTAL"]

tab_pop_cob[`country of birth` != "Latvia"][1:20]

# <00> 0 Latvija
# <01> 1 Krievija
# <02> 2 Baltkrievija
# <03> 3 Ukraina
# <06> 4 Lietuva
# <07> 5 Kazahstāna
# <05> 8 Igaunija
# <08> 9 Vācija
# <09> Citā valstī

# <04> Polija - par maz

pat <- "Latvia|Russia|Belarus|Ukraine|Lithuania|Kazakhstan|Estonia|Germany"
tab_pop_cob[, cob := fifelse(
  grepl(pattern = pat, x = `country of birth`), `country of birth`, "Other"
)]
rm(pat)

tab_pop_cob <- tab_pop_cob[, .(value = sum(value)),
                           keyby = .(`time period`, cob)]

tab_pop_cob[, flag := cob != "Other"]
setorderv(tab_pop_cob, c("flag", "value"), -1)
tab_pop_cob[, flag := NULL]
tab_pop_cob

tab_pop_cob <- merge(tab_pop_cob, tab_pop_trg_tot)
setcolorder(tab_pop_cob, c("territorial unit"))

tab_pop_cob[, RAKEDIM3 := .I]
tab_pop_cob[, TOTAL := round_preserve_sum(value * pop_trg / sum(value))]
tab_pop_cob[, Cntry_of_Birth := cob]


# Excel files

tab_rakedim1_total <- tab_pop_sex_age[, .(RAKEDIM1, TOTAL)]
tab_rakedim1_codes <- tab_pop_sex_age[, .(RAKEDIM1, Gender, Age)]

tab_rakedim2_total <- tab_pop_eth[, .(RAKEDIM2, TOTAL)]
tab_rakedim2_codes <- tab_pop_eth[, .(RAKEDIM2, Ethnicity)]

tab_rakedim3_total <- tab_pop_cob[, .(RAKEDIM3, TOTAL)]
tab_rakedim3_codes <- tab_pop_cob[, .(RAKEDIM3, Cntry_of_Birth)]

write.xlsx(
  x = list(RAKEDIM1 = tab_rakedim1_total,
           RAKEDIM2 = tab_rakedim2_total,
           RAKEDIM3 = tab_rakedim3_total),
  file = "../Deliverables/Deliverables_2023/CY2_Prelim_MS_Benchmark_WIF_LVA.xlsx",
  overwrite = TRUE,
  colWidths = 20
)

write.xlsx(
  x = list(RAKEDIM1 = tab_rakedim1_codes,
           RAKEDIM2 = tab_rakedim2_codes,
           RAKEDIM3 = tab_rakedim3_codes),
  file = "../Deliverables/Deliverables_2023/CY2_Prelim_MS_Benchmark_WIF_Codebook_LVA.xlsx",
  overwrite = TRUE,
  colWidths = 20
)
