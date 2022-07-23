# Frame pārbaude

library(data.table)

dat <- fread(file = "data/frame.csvy.gz", yaml = T)

str(dat)

dat[, .N]
dat[, .N, keyby = .(pop_majo_kods, pop_majo_apraksts, pop_majo)]

dat[pop_majo == 1, .N]

dat[, summary(pers_sk)]
dat[, summary(pers_sk_16_65)]

dat[pop_majo == 1, .N, keyby = .(pop_piaac, pers_sk_16_65 > 0)]

dat[pop_piaac == 1, .N]
# dat[, .N, keyby = .(der_capi_kods, der_capi_apraksts)]
dat[pop_piaac == 1, .N,
    keyby = .(der_capi_kods, der_capi_apraksts, der_capi, der_piaac)]
dat[pop_piaac == 1, .N, keyby = .(der_piaac)]

dat[pop_piaac == 1 & der_piaac == 1, .N]


# Filtrs

dat <- dat[pop_piaac == 1 & der_piaac == 1]

fwrite(x = dat, file = "data/frame_piaac.csvy.gz", yaml = T)
