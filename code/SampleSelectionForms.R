# Info for Sample Selection Forms

# Reset
rm(list = ls())

# Packages
library(data.table)
library(ggplot2)
library(openxlsx)

probs <- seq(0, 1, by = 0.25)
probs

# Data
tab_strata <- fread(file = "tables/sample-alloc.csv")
frame_psu <- fread(file = "data/frame_psu.csvy", yaml = T)
frame_du  <- fread(file = "data/frame_piaac_sampled.csvy.gz", yaml = T)
dat <- fread("data/sample_piaac.csvy", yaml = T)
dat_scf <- fread("data/sample_piaac_scf.csv")


# dat_scf[, .N, keyby = .(REGION)]
# dat_scf[, .N, keyby = .(JURISDICTION)]
# dat_scf[, .N, keyby = .(URBRUR)]


# File: SampleSelectionForm_SS_2_PSU_LVA.xlsx

# Table 2.
tab_strata[, .(STRAT_PSU, strata_psu, psu_cert, strata2019, reg_stat_nosauk)]
tab_strata[, strata2019_f := factor(
  x = strata2019,
  levels = 1:4,
  labels = c("Rīga", "Cities", "Towns", "Rural")
)]

tab_strata[, reg_short := sub("s .*$", "", reg_stat_nosauk)]

tab_strata[, label := fifelse(
  test = strata2019 == 1L,
  yes  = reg_short,
  no   = paste(strata2019_f, reg_short, sep = ", ")
)]
tab_strata[(psu_cert), label := paste(label,
                                      stringr::str_sub(strata_psu, -5),
                                      sep = ", ")]
tab <- tab_strata[, .(paste(STRAT_PSU, label, sep = ": "))]

tab[, paste(V1, collapse = "; ")] |> cat()

tab_strata[, paste(label, collapse = "\n")] |> cat()

# Table 11.
tab_psu_cert <- frame_psu[(psu_cert), .(STRAT_PSU, ID_PSU, psu_mos)]
write.xlsx(x = tab_psu_cert, file = "tables/tab_psu_cert.xlsx")


# Table 12.
tab_psu_non_cert <- frame_psu[
  !(psu_cert),
  .(type = "main",
    tot_mos = sum(psu_mos),
    pop_psu = .N,
    n_psu = sum(sample_psu),
    wgh_mos = sum(sample_psu * psu_mos / PROB_PSU)),
  keyby = .(STRAT_PSU)
]
tab_psu_non_cert
write.xlsx(x = tab_psu_non_cert, file = "tables/tab_psu_non_cert.xlsx")

# Table 13a.
frame_psu[sample_psu == 1, .N, keyby = .(psu_cert)]

# Table 13b.
# frame_psu[sample_psu == 1, summary(PROB_PSU)]
tab_psu_13b <- frame_psu[
  sample_psu == 1,
  .(probs, value = quantile(PROB_PSU, probs = probs))
]
write.xlsx(x = tab_psu_13b, file = "tables/tab_psu_13b.xlsx")

# Table 14.
tab_psu_14 <- frame_psu[sample_psu == 1,
                        .(n_psu = .N, wgh_psu = sum(psu_mos / PROB_PSU)),
                        keyby = .(strata2019)]
write.xlsx(x = tab_psu_14, file = "tables/tab_psu_14.xlsx")

# Table 15.
tab_psu_15 <- frame_psu[,
  .(ID_PSU, SORT_PSU, psu_mos,
    samp_int = sum(psu_mos) / sum(sample_psu),
    PROB_PSU, sample_psu, psu_cert),
  by = .(STRAT_PSU)]
tab_psu_15

ggplot(data = tab_psu_15,
       mapping = aes(x = psu_mos, y = PROB_PSU)) +
  geom_point() +
  facet_wrap(facets = vars(STRAT_PSU)) +
  theme_bw()

tab_psu_15[, all(PROB_PSU <= 1)]

tab_psu_15[PROB_PSU == 1, .N, keyby = .(sample_psu)]

tab_psu_15[psu_mos >= samp_int, .N, keyby = .(sample_psu)]


# Listing of the first 200 records
frame_psu[, samp_int := sum(psu_mos) / sum(sample_psu), by = .(STRAT_PSU)]

tab_psu_15_sub <- frame_psu[
  1:200, .(ID_PSU, STRAT_PSU, SORT_PSU, psu_mos, samp_int, PROB_PSU,
           SUBSAMP = 1L, sample_psu)]
tab_psu_15_sub
write.xlsx(x = tab_psu_15_sub, file = "tables/tab_psu_15_sub.xlsx")


# SampleSelectionForm_SS_2_DU_LVA.xlsx

# Table 6.
dat[, .N, keyby = .(SUBSAMP > 5)]

# Table 7.
tab_du_7 <- dat[, .N, keyby = .(SUBSAMP)]
write.xlsx(x = tab_du_7, file = "tables/tab_du_7.xlsx")

# Table 8.
dat[
  SUBSAMP == 1, .N, keyby = .(STRAT_PSU, ID_PSU)
][, .(average = mean(N), min = min(N), max = max(N))]

# Table 9.
tab_du_9 <- frame_psu[sample_psu == 1,
                      .(ID_PSU, ID_SSU = NA_integer_, psu_mos,
                        n_main = n_du * 2 / 3,
                        n_res = n_du / 3)][1:50]
write.xlsx(x = tab_du_9, file = "tables/tab_du_9.xlsx")

# Table 10.
tab_du_10a <- dat[, .(probs, value = quantile(PROB_HH, probs = probs))]
tab_du_10b <- dat[, .(probs, value = quantile(1 / PROB_PSU / PROB_HH,
                                              probs = probs))]

dat[, .(sum  =  sum(1 / PROB_PSU / PROB_HH),
        mean = mean(1 / PROB_PSU / PROB_HH))]
dat[, .(sum(1 / PROB_PSU / PROB_HH) / sum(SUBSAMP == 1))]

write.xlsx(x = list(tab_du_10a, tab_du_10b), file = "tables/tab_du_10.xlsx")


# Table 11.
tab_du_11 <- merge(
  frame_du[, .(pop_du = .N), keyby = .(STRAT_PSU)],
  dat[, .(wgt_du = sum(1 / PROB_PSU / PROB_HH)), keyby = .(STRAT_PSU)]
)
tab_du_11[, all.equal(pop_du, wgt_du)]
write.xlsx(x = tab_du_11, file = "tables/tab_du_11.xlsx")


# Table 12.
tab_du_12 <- dat[, .(n_du = .N, wgt_du = sum(1 / PROB_PSU / PROB_HH)),
                 keyby = .(terit_tips_kods, terit_tips_nosauk)]
write.xlsx(x = tab_du_12, file = "tables/tab_du_12.xlsx")


# Table 13.
dat[, THEOR_HBWT := 1 / PROB_PSU / PROB_HH]
probs <- c(0, 0.5, 1)
tab_du_13 <- dat[, as.list(quantile(x = THEOR_HBWT, probs = probs)),
                 keyby = .(terit_tips_kods, terit_tips_nosauk)]
write.xlsx(x = tab_du_13, file = "tables/tab_du_13.xlsx")


# Table 14.
dat[, all(PROB_HH <= 1)]
dat[PROB_HH == 1]

tab <- dat[, .(samp_int = sum(1 / PROB_HH) / .N,
               inv_prob = 1 / PROB_HH[1]), keyby = .(STRAT_PSU, ID_PSU)]
tab[, all.equal(samp_int, inv_prob)]


frame_du <- merge(frame_du, dat[, .(ID_PSU, ID_HH, SUBSAMP)],
                  by = c("ID_PSU", "ID_HH"), all.x = T)

tab_du_14 <- frame_du[
  sample_psu == 1L,
  .(ID_PSU, sample_psu, ID_HH, SORT_HH, PROB_HH, SUBSAMP, sample_du)
][1:200]

tab_du_14
tab_du_14[sample_du == 1]

write.xlsx(x = tab_du_14, file = "tables/tab_du_14.xlsx")
