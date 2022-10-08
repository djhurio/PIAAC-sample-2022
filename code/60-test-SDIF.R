# Test SDIF

library(data.table)
library(openxlsx)

dat1 <- fread("data/sample_piaac_sdif.csv",
              colClasses = c(CASEID = "character"))

dat2 <- read.xlsx("test/sample_piaac_sdif_5OKT2022.xlsx")
setDT(dat2)

all.equal(dat1, dat2)

names(dat1)
names(dat2)

setdiff(names(dat1), names(dat2))
setdiff(names(dat2), names(dat1))

dat2[, .N, keyby = .(PERSID)]

str(dat1[, .(CASEID, SUBSAMP)])
str(dat2[, .(CASEID, SUBSAMP)])

test <- merge(dat1[, .(CASEID, SUBSAMP)],
              dat2[, .(CASEID, SUBSAMP)],
              by = "CASEID")

test[, .N, keyby = .(SUBSAMP.x, SUBSAMP.y)]
