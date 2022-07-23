# Frame pārbaude

library(data.table)

dat <- fread(file = "data/frame.csvy.gz", yaml = T)

dat[grep("\"\"", adrese_ir), adrese_ir := gsub("\"\"", "\"", adrese_ir)]
dat[grep("\"\"", adrese_vzd), adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]

str(dat)

dat[, .N]
dat[, .N, keyby = .(pop_majo_kods, pop_majo_apraksts, pop_majo)]

dat[pop_majo == 1, .N]

dat[pop_majo == 1, summary(pers_sk)]
dat[pop_majo == 1, summary(pers_sk_16_65)]

dat[pop_majo == 1, .N, keyby = .(pop_piaac, pers_sk_16_65 > 0)]

dat[pop_piaac == 1, .N]
# dat[, .N, keyby = .(der_capi_kods, der_capi_apraksts)]

dat[pop_piaac == 1, .N,
    keyby = .(der_capi_kods, der_capi_apraksts, der_capi, der_piaac)]

dat[pop_piaac == 1, .N, keyby = .(der_piaac)][, P := prop.table(N)][]
dat[pop_piaac == 1, .N, keyby = .(der_piaac, der_iec2019)][, P := prop.table(N)][]

dat[pop_piaac == 1 & der_piaac == 1, .N]


# Filtrs

dat <- dat[pop_piaac == 1 & der_piaac == 1]

fwrite(x = dat, file = "data/frame_piaac.csvy.gz", yaml = T)
