# CY2_Prelim_MS_SDIF_Codebook_LVA.xlsx

library(data.table)

sample_piaac_sdif <- fread(file = "data/sample_piaac_sdif.csv")

sample_piaac_sdif[, .N, keyby = .(STRAT_PSU)]
sample_piaac_sdif[, .N, keyby = .(SORT_PSU)]
sample_piaac_sdif[, .N, keyby = .(SORT_HH)]

sample_piaac_sdif[, .N, keyby = .(REGION)]

sample_piaac_sdif[, .N, keyby = .(SUBSAMP)]


sample_piaac_scf <- fread(file = "data/sample_piaac_scf.csv")

sample_piaac_scf[, .N, keyby = .(REGION)]
sample_piaac_scf[, .N, keyby = .(JURISDICTION)]
sample_piaac_scf[, .N, keyby = .(URBRUR)]



sample_piaac <- fread(file = "data/sample_piaac.csvy", yaml = T)

sample_piaac[, .N, keyby = .(sample_du)]
sample_piaac <- sample_piaac[sample_du == 1L]

sample_piaac[, .N, keyby = .(pers_sk_16_65)]

tmp <- sample_piaac[, .N, keyby = .(adm_terit_kods, adm_terit_nosauk)]
tmp[, N := NULL]

clipr::write_clip(tmp, col.names = F)
