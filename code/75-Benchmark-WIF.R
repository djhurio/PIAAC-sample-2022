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


# Read file from CSP

fname <- file.path(
  "../Deliverables/Deliverables_2023",
  "CY2_Prelim_MS_Benchmark_WIF_Codebook_LVA_CSP_2023-06-03.xlsx"
)

tab_rakedim1 <- read.xlsx(xlsxFile = fname, sheet = "RAKEDIM1") |> setDT()
tab_rakedim2 <- read.xlsx(xlsxFile = fname, sheet = "RAKEDIM2") |> setDT()
tab_rakedim3 <- read.xlsx(xlsxFile = fname, sheet = "RAKEDIM3") |> setDT()

setnames(tab_rakedim1, ncol(tab_rakedim1), "TOTAL")
setnames(tab_rakedim2, ncol(tab_rakedim2), "TOTAL")
setnames(tab_rakedim3, ncol(tab_rakedim3), "TOTAL")

tab_rakedim1[, sum(TOTAL)] |> print()
tab_rakedim2[, sum(TOTAL)] |> print()
tab_rakedim3[, sum(TOTAL)] |> print()

if (tab_rakedim1[, sum(TOTAL)] != tab_rakedim2[, sum(TOTAL)] |
    tab_rakedim2[, sum(TOTAL)] != tab_rakedim3[, sum(TOTAL)]) stop()

# Excel files

tab_rakedim1_total <- tab_rakedim1[, .(RAKEDIM1, TOTAL)]
tab_rakedim1_codes <- tab_rakedim1[, .(RAKEDIM1, Gender, Age)]

tab_rakedim2_total <- tab_rakedim2[, .(RAKEDIM2, TOTAL)]
tab_rakedim2_codes <- tab_rakedim2[, .(RAKEDIM2, Ethnicity)]

tab_rakedim3_total <- tab_rakedim3[, .(RAKEDIM3, TOTAL)]
tab_rakedim3_codes <- tab_rakedim3[, .(RAKEDIM3, Country_of_Birth)]

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
