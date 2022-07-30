# Format sample file
# According to "PIAAC_CY2(2022_04)Sampling and Weighting File Layouts.docx"

# Reset
rm(list = ls())

# Packages
library(data.table)
library(openxlsx)

# Load variable names
tab_vars <- read.xlsx(xlsxFile = "../info/survey-control-file.xlsx")
setDT(tab_vars)
tab_vars[, .(Variable.name, Format, Type)]

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

# ID_PSU
# Sampling ID: Primary sampling unit (PSU) identification number
# Required if area PSUs are selected; Blank if no PSU selection.
dat[, .(ID_PSU)]

# ID_HH
# Sampling ID: Household (HH) identification number
# Required if HHs are selected;
# Blank if no household selection,
# otherwise a sequential number is assigned within geographic clusters.
dat[, .(ID_HH)]

# PROB_PSU
# First-stage sampling unit probability of selection
# Required if area PSUs are selected; blank if no PSU selection.
dat[, .(PROB_PSU)]

# PROB_HH
# HH probability of selection (within prior-stage clusters, if applicable)
# Required if household sampling; blank for one-stage designs.
dat[, .(PROB_HH)]

# Different values of PROB_HH dependeing on release of reserve sample
dat[, .N, keyby = .(SUBSAMP)]
val <- unique(dat$SUBSAMP)

dat[, paste0("PROB_HH", val) := lapply(
  X = val,
  FUN = \(x) round(((SUBSAMP <= x) * sum(SUBSAMP <= x) / psu_mos), digits = 12)
), by = .(STRAT_PSU, ID_PSU)]

dat[, .SD, .SDcols = patterns("PROB_HH")]
dat[ID_PSU == first(ID_PSU), .SD, .SDcols = patterns("PROB_HH")]

dat[, all.equal(PROB_HH, PROB_HH6)]

dat[, lapply(.SD, \(x) sum((1 / PROB_PSU / x)[x > 0])),
    .SDcols = patterns("PROB_HH")]
dat[, lapply(.SD, \(x) sum((1 / PROB_PSU / x)[x > 0])),
    .SDcols = patterns("PROB_HH"), keyby = .(STRAT_PSU)]


# STRAT_PSU
# Explicit strata used for stratifying PSUs
# Required if stratification is used for PSUs; blank otherwise.
dat[, .(STRAT_PSU)]
dat[, .N, keyby = .(STRAT_PSU)]

# SORT_PSU
# Sort order for PSU selection
# Required if systematic sampling of PSUs; blank otherwise.
dat[, .(SORT_PSU)]

# SORT_HH
# Sort order for HH selection
# Required if systematic sampling of households; blank otherwise.
dat[, .(SORT_HH)]

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


# SUBSAMP
# Subsample flag
# Required;
# 1 = initial sample (first release),
# 2 = released sample #2,
# 3 = released sample #3,
# …
# 9 = released sample #9.
dat[, .(SUBSAMP)]
dat[, .N, keyby = .(SUBSAMP)]

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



# Create file

vars_scf <- c(
  intersect(tab_vars$Variable.name, names(dat)),
  grep("PROB_HH[1-6]", names(dat), value = T)
)

dat_scf <- dat[, ..vars_scf]

# Save DU sample
fwrite(    x = dat_scf, file = "data/sample_piaac_scf.csvy", yaml = T)
write.xlsx(x = dat_scf, file = "data/sample_piaac_scf.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)
