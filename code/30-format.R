# Format sample file
# According to "PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.docx"

# Reset
rm(list = ls())

# Packages
library(data.table)
library(openxlsx)
library(sampling)

# Load variable names

# Survey Control File (SCF)
tab_vars_scf <- read.xlsx(xlsxFile = "../info/survey-control-file.xlsx")
setDT(tab_vars_scf)
tab_vars_scf[, .(Variable.name, Format, Type)]

# Sample Design International File (SDIF)
tab_vars_sdif <- read.xlsx(
  xlsxFile = "../info/sample-design-international-file.xlsx"
)
setDT(tab_vars_sdif)
tab_vars_sdif[, .(Variable.name, Format, Type)]


# Load sample file
dat <- fread(file = "data/sample_piaac.csvy", yaml = T)

# Remove double quotes
dat[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]

# PIAAC variables

# CNTRYID
# Country ID
# Required; ISO 3166 3-digit numeric codes.
# https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes
dat[, CNTRYID := 428L]

# CASEID
# Household operational ID
# Required if household sampling;
# subset of PERSID that will be assigned in screening;
# Does not include CNTRYID;
# Matches to other PIAAC databases when combined with CNTRYID;
# Blank if persons are selected from registries.
dat[, .(CASEID)]
dat[sample_du == 1, .(CASEID)]
dat[sample_ft == 1, .(CASEID)]

# ID_PSU
# Sampling ID: Primary sampling unit (PSU) identification number
# Required if area PSUs are selected; Blank if no PSU selection.
dat[sample_du == 1, .(ID_PSU)]
dat[sample_du == 1, .N, keyby = .(ID_PSU)]

dat[sample_ft == 1, .(ID_PSU_FT)]
dat[sample_ft == 1, .N, keyby = .(ID_PSU_FT)]

dat[sample_ft == 1, ID_PSU := ID_PSU_FT]
dat[, ID_PSU_FT := NULL]

dat[, .(ID_PSU)]
dat[, .N, keyby = .(ID_PSU)]


# ID_HH
# Sampling ID: Household (HH) identification number
# Required if HHs are selected;
# Blank if no household selection,
# otherwise a sequential number is assigned within geographic clusters.
dat[sample_du == 1, .(ID_PSU, ID_HH)]
dat[sample_du == 1, .(ID_PSU, ID_HH)] |> anyDuplicated()

dat[sample_ft == 1, .(ID_PSU, ID_HH_FT)]
dat[sample_ft == 1, .(ID_PSU, ID_HH_FT)] |> anyDuplicated()

dat[sample_ft == 1, ID_HH := ID_HH_FT]
dat[, ID_HH_FT := NULL]

dat[, .(ID_PSU, ID_HH)]
dat[, .(ID_PSU, ID_HH)] |> anyDuplicated()


# PROB_PSU
# First-stage sampling unit probability of selection
# Required if area PSUs are selected; blank if no PSU selection.
dat[sample_du == 1, .(PROB_PSU)]
dat[sample_ft == 1, .(PROB_PSU_FT)]

dat[sample_ft == 1, PROB_PSU := PROB_PSU_FT]
dat[, PROB_PSU_FT := NULL]


# PROB_HH
# HH probability of selection (within prior-stage clusters, if applicable)
# Required if household sampling; blank for one-stage designs.
dat[sample_du == 1, .(PROB_HH)]
dat[sample_ft == 1, .(PROB_HH_FT)]

dat[sample_ft == 1, PROB_HH := PROB_HH_FT]
dat[, PROB_HH_FT := NULL]


# Different values of PROB_HH dependeing on release of reserve sample
dat[, .N, keyby = .(SUBSAMP)]

# SUBSAMP correction
dat[SUBSAMP < 3, SUBSAMP := 1L]
dat[SUBSAMP > 2, SUBSAMP := SUBSAMP - 2L]
dat[, .N, keyby = .(SUBSAMP)]

val <- dat[, sort(unique(SUBSAMP))]

dat[sample_du == 1, paste0("PROB_HH", val) := lapply(
  X = val,
  FUN = \(x) round(((SUBSAMP <= x) * sum(SUBSAMP <= x) / psu_mos), digits = 12)
), by = .(STRAT_PSU, ID_PSU)]

dat[sample_du == 1, .SD, .SDcols = patterns("PROB_HH")]
dat[sample_du == 1][ID_PSU == first(ID_PSU), .SD, .SDcols = patterns("PROB_HH")]

dat[sample_du == 1, all.equal(PROB_HH, PROB_HH8)]

dat[sample_du == 1, lapply(.SD, \(x) sum((1 / PROB_PSU / x)[x > 0])),
    .SDcols = patterns("PROB_HH")]
dat[sample_du == 1, lapply(.SD, \(x) sum((1 / PROB_PSU / x)[x > 0])),
    .SDcols = patterns("PROB_HH"), keyby = .(STRAT_PSU)]


# PROB_SMPFLG1
# Probability that the within-household sampling flag SMPFLG1 was set to 1
# Required for screener countries.
# Indicates the rate at which the sampling flag was set to 1.
# Equal to 1 if no stratification for within household selection.
# 
# PROB_SMPFLG1:
# Please set to 1 since you do not have stratification within household.
dat[, PROB_SMPFLG1 := 1L]


# STRAT_PSU
# Explicit strata used for stratifying PSUs
# Required if stratification is used for PSUs; blank otherwise.
dat[sample_du == 1, .(STRAT_PSU)]
dat[sample_du == 1, .N, keyby = .(STRAT_PSU)]

dat[sample_ft == 1, .(STRAT_PSU_FT)]
dat[sample_ft == 1, .N, keyby = .(STRAT_PSU_FT)]

dat[sample_ft == 1, STRAT_PSU := STRAT_PSU_FT]
dat[, STRAT_PSU_FT := NULL]


# SORT_PSU
# Sort order for PSU selection
# Required if systematic sampling of PSUs; blank otherwise.
dat[sample_du == 1, .(ID_PSU, SORT_PSU)]
dat[sample_du == 1, all.equal(ID_PSU, SORT_PSU)]

dat[sample_ft == 1, .(ID_PSU, SORT_PSU_FT)]
dat[sample_ft == 1, all.equal(ID_PSU, SORT_PSU_FT)]

dat[sample_ft == 1, SORT_PSU := SORT_PSU_FT]
dat[, SORT_PSU_FT := NULL]


# SORT_HH
# Sort order for HH selection
# Required if systematic sampling of households; blank otherwise.
dat[sample_du == 1, .(ID_HH, SORT_HH)]
dat[sample_du == 1, all.equal(ID_HH, SORT_HH)]

dat[sample_ft == 1, .(ID_HH, SORT_HH_FT)]
dat[sample_ft == 1, all.equal(ID_HH, SORT_HH_FT)]

dat[sample_ft == 1, SORT_HH := SORT_HH_FT]
dat[, SORT_HH_FT := NULL]

names(dat)


# ADDRESS1	Address line 1	Required
# ADDRESS2	Address line 2	Optional
# ADDRESS3	Address line 3	Optional
# CITY	City Name	Optional
# JURISDICTION	Name of state, province, etc.	Required
# ADDRESS4	Address postal area number 	Optional; For example, zip code.

# Create empty fields in correct order
dat[, c("ADDRESS1", "ADDRESS2", "ADDRESS3",
        "CITY", "JURISDICTION", "ADDRESS4") := ""]

dat[, .(adrese_ir)]

# Number of delimiters in address
dat[, n := stringr::str_count(adrese_ir, ", ")]

dat[, .N, keyby = .(terit_tips_kods, terit_tips_nosauk)]
dat[, .N, keyby = .(terit_tips_kods, terit_tips_nosauk, n)]

# Administratīvās valstspilsētas
dat[terit_tips_kods %in% 1:2,
    tstrsplit(adrese_ir, ", ")] # 3 parts
dat[terit_tips_kods %in% 1:2,
    c("ADDRESS1", "JURISDICTION", "ADDRESS4") :=
      tstrsplit(adrese_ir, ", ")]
dat[terit_tips_kods %in% 1:2]

# Novada valstspilsētas & pilsētas
dat[terit_tips_kods %in% 3:4,
    tstrsplit(adrese_ir, ", ")] # 4 parts
dat[terit_tips_kods %in% 3:4,
    c("ADDRESS1", "CITY", "JURISDICTION", "ADDRESS4") :=
      tstrsplit(adrese_ir, ", ")]
dat[terit_tips_kods %in% 3:4]

# Pagasti
dat[terit_tips_kods == 5 & n == 3,
    tstrsplit(adrese_ir, ", ")] # 4 parts
dat[terit_tips_kods == 5 & n == 3,
    c("ADDRESS1", "ADDRESS2", "JURISDICTION", "ADDRESS4") :=
      tstrsplit(adrese_ir, ", ")]
dat[terit_tips_kods == 5 & n == 3]

dat[terit_tips_kods == 5 & n == 4,
    tstrsplit(adrese_ir, ", ")] # 5 parts
dat[terit_tips_kods == 5 & n == 4,
    c("ADDRESS1", "ADDRESS2", "ADDRESS3", "JURISDICTION", "ADDRESS4") :=
      tstrsplit(adrese_ir, ", ")]
dat[terit_tips_kods == 5 & n == 4]

# Test
dat[order(ADDRESS1), .(ADDRESS1)]
dat[, all(ADDRESS1 != "")]

# Pagasts
dat[terit_tips_kods == 5 & n == 3, .N, keyby = .(ADDRESS2)]
dat[terit_tips_kods == 5 & n == 3, all.equal(ADDRESS2, atvk_nosauk)]

# Ciems
dat[terit_tips_kods == 5 & n == 4, .N, keyby = .(ADDRESS2)]

# Pilsēta
dat[terit_tips_kods %in% 3:4, .N, keyby = .(CITY)]
dat[terit_tips_kods %in% 3:4, all.equal(CITY, atvk_nosauk)]

# Admin ter
dat[, .N, keyby = .(JURISDICTION)]
dat[, all.equal(JURISDICTION, adm_terit_nosauk)]

# Postal index
dat[, .N, keyby = .(ADDRESS4)]
dat[ADDRESS4 == "LV-9999", .(adr_kods_maja, adr_kods_dziv, adrese_ir)]

dat[, n := NULL]


# ADD_DU
# Added dwelling unit (DU) Flag
# Required for countries with screeners;
# Flag to indicate whether the DU was added during data collection
# through a coverage enhancement procedure, such as the hidden DU procedure;
# 0=false, 1=true. Equal to 0 for the initial sample.
# 
# ADD_DU: Please set to 0 in the SCF
dat[, ADD_DU := 0L]


# QCFLAG_ASSIGN
# Quality control flag 
# Required; 0=not selected, 1=selected. At least 10% cases need to have value 1.
# 
# QCFLAG_ASSIGN: At least 10% cases need to have value 1.

X <- model.matrix(object = PROB_SMPFLG1 ~ factor(STRAT_PSU) + factor(SUBSAMP),
                  data = dat[sample_du == 1])
dim(X)
head(X)

set.seed(224946)
dat[sample_du == 1,
    QCFLAG_ASSIGN := sampling::samplecube(X = X,
                                          pik = rep(.1, .N),
                                          order = 2,
                                          method = 2)]
rm(X)

dat[sample_du == 1, .N]
dat[sample_du == 1, .N, keyby = .(QCFLAG_ASSIGN)][, P := prop.table(N)][]
# dat[, .(STRAT_PSU, ID_PSU, ID_HH, QCFLAG_ASSIGN)] |> View()

dat[sample_du == 1, round(mean(QCFLAG_ASSIGN), 2)]
dat[sample_du == 1, round(mean(QCFLAG_ASSIGN), 2), keyby = .(SUBSAMP)]
dat[sample_du == 1, round(mean(QCFLAG_ASSIGN), 2), keyby = .(STRAT_PSU)]

set.seed(152537)
dat[sample_ft == 1,
    QCFLAG_ASSIGN := sampling::srswor(n = ceiling(.N * .1), N = .N),
    by = .(STRAT_PSU, ID_PSU)]

dat[sample_ft == 1, .(STRAT_PSU, ID_PSU, QCFLAG_ASSIGN)]
dat[sample_ft == 1, round(mean(QCFLAG_ASSIGN), 2), keyby = .(STRAT_PSU)]
dat[sample_ft == 1, round(mean(QCFLAG_ASSIGN), 2), keyby = .(ID_PSU)]


# SUBSAMP
# Subsample flag
# Required;
# 1 = initial sample (first release),
# 2 = released sample #2,
# 3 = released sample #3,
# …
# 9 = released sample #9.
dat[, .(SUBSAMP)]
dat[, .N, keyby = .(sample_ft, SUBSAMP)]


# SAMPTYPE
# Flag for incentive groups (FT) or supplemental sample (MS)
# For FT, required for international CMS countries that are doing an incentive
# experiment. Values 1-9 for the different incentive groups.
# For MS, required for all countries;
# 1: PIAAC sample
# 2: Supplemental sample #1,
# 3: Supplemental sample #2, …etc.
# 
# SAMPTYPE: Please set to 1 since you do not have supplemental sample
dat[, SAMPTYPE := 1L]


# SMPFLG1
# Flag to indicate whether any persons should be selected from STRATUM 1
# within the household
# Required for screener countries;
# If no stratification for within household selection, setto 1; 1=Yes,0=No.
# 
# SMPFLG1: Please set it to 1 since there is no stratification within household.
dat[, SMPFLG1 := 1L]


# EXCFRM_PROP
# Proportion in target population who are excluded from the sampling frame
# Required. Constant.
# 
# EXCFRM_PROP:
# Proportion in target population who are excluded from the sampling frame
# dat[sample_du == 1, EXCFRM_PROP := 0.0061]
dat[sample_du == 1, EXCFRM_PROP := 0.015] # from MS sample design summary
dat[sample_ft == 1, EXCFRM_PROP := 0]
# dat[, EXCFRM_PROP := 0.0061]


# REGION
# Region
# Required. For iCMS countries, up to 25 categories allowed.
dat[, REGION := as.integer(reg_stat_kods)]
dat[, .N, keyby = .(REGION)]
dat[, .N, keyby = .(REGION, reg_stat_nosauk)]

# URBRUR
# Urban/rural indicator
# Required. 1: Urban, 2: Rural.
dat[, .N, keyby = .(terit_tips_kods, terit_tips_nosauk)]
dat[, URBRUR := fifelse(terit_tips_kods < 5, 1L, 2L)]
dat[, .N, keyby = .(URBRUR)]
dat[, .N, keyby = .(URBRUR, terit_tips_kods, terit_tips_nosauk)]



# Strata VS territory type
dat[, .N, keyby = .(STRAT_PSU, terit_tips_kods)] |>
  dcast.data.table(formula = STRAT_PSU ~ terit_tips_kods,
                   value.var = "N", fill = 0L)


# Sort
# names(dat)
setkey(dat, STRAT_PSU, ID_PSU, ID_HH)
dat[, .(STRAT_PSU, ID_PSU, ID_HH)]



# Create SCF file
vars_scf <- intersect(tab_vars_scf$Variable.name, names(dat))
dat_scf_du <- dat[sample_du == 1, ..vars_scf]
dat_scf_ft <- dat[sample_ft == 1, ..vars_scf]

# Create SDIF file
vars_sdif <- intersect(tab_vars_sdif$Variable.name, names(dat))
dat_sdif_du <- dat[sample_du == 1, ..vars_sdif]
dat_sdif_ft <- dat[sample_ft == 1, ..vars_sdif]



# Metadata for SampleSelectionForm_SS_3_LVA.xlsx
vars_scf

sapply(dat_scf_du, class)
vars_scf_type <- substr(sapply(dat_scf_du, class), 1, 1) |> toupper() |> unname()
vars_scf_type[vars_scf_type == "I"] <- "N"
vars_scf_type

vars_scf_len <- sapply(dat_scf_du, \(x) max(nchar(x)), USE.NAMES = F)
vars_scf_len

dat_scf_meta <- data.table(var_name = vars_scf,
                           type = vars_scf_type,
                           length = vars_scf_len)

tab_vars_scf[, .(Variable.name)]

dat_scf_meta <- merge(x = dat_scf_meta,
                      y = tab_vars_scf[, .(Variable.name, Label)],
                      by.x = "var_name",
                      by.y = "Variable.name",
                      all.x = T,
                      sort = F)


# Save Survey Control File
fwrite(    x = dat_scf_du, file = "data/sample_piaac_scf.csv")
write.xlsx(x = dat_scf_du, file = "data/sample_piaac_scf.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)

fwrite(    x = dat_scf_ft, file = "data/sample_piaac_scf_ft.csv")
write.xlsx(x = dat_scf_ft, file = "data/sample_piaac_scf_ft.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)

# Save sample design international file (SDIF)
fwrite(    x = dat_sdif_du, file = "data/sample_piaac_sdif.csv")
write.xlsx(x = dat_sdif_du, file = "data/sample_piaac_sdif.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)

fwrite(    x = dat_sdif_ft, file = "data/sample_piaac_sdif_ft.csv")
write.xlsx(x = dat_sdif_ft, file = "data/sample_piaac_sdif_ft.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)

# Save SCF metadata
write.xlsx(x = dat_scf_meta, file = "data/sample_piaac_scf_meta.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2)

dat_scf_du[, .N]
dat_scf_ft[, .N]

# Save sample file for fieldwork
dat_fw <- dat[, .(sample_du, sample_ft,
                  CASEID, STRAT_PSU, ID_PSU, ID_HH, SUBSAMP, QCFLAG_ASSIGN,
                  reg_stat_kods, reg_stat_nosauk,
                  adm_terit_kods, adm_terit_nosauk,
                  atvk, atvk_nosauk,
                  terit_tips_kods, terit_tips_nosauk,
                  adr_kods, adrese_ir,
                  lks92x, lks92y, lon, lat)]

dat_fw_du <- dat_fw[sample_du == 1]
dat_fw_ft <- dat_fw[sample_ft == 1]

# Save sample file for fieldwork
fwrite(    x = dat_fw_du, file = "data/sample_piaac_fw.csv")
write.xlsx(x = dat_fw_du, file = "data/sample_piaac_fw.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)

fwrite(    x = dat_fw_ft, file = "data/sample_piaac_fw_ft.csv")
write.xlsx(x = dat_fw_ft, file = "data/sample_piaac_fw_ft.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)
