# Sampling

# Reset
rm(list = ls())

# Packages
library(data.table)
library(openxlsx)
library(sampling)
library(ggplot2)
library(leaflet)

options(openxlsx.numFmt = NULL)

# TSP sort and plot
source("code/TSP-sort-plot.R")

# Rounding by preserving sum of values after rounding
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}



# Parameters
sample_size_psu <- 1842L
sample_size_du_by_psu <- 15L # Original and reserve sample

sample_size_du <- sample_size_psu * sample_size_du_by_psu
sample_size_du # Original and reserve sample


# DU frame
dat <- fread("data/frame_piaac.csvy.gz", yaml = T)

# Remove double quotes
dat[, adrese_ir  := gsub("\"\"", "\"", adrese_ir)]
dat[, adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]


# Number of DU by PSU
dat[, .N, keyby = .(iec2019)][order(N)]
dat[, .N, keyby = .(iec2019)][, summary(N)]
# min = 61
# max = 545
# OK


# DUs with imputed PSU
dat[, .N, keyby = .(iec2019_imp)] # 1
# dat[iec2019_imp == 1]

# DUs with imputed coordinates
dat[, .N, keyby = .(lks92_imp)] # 220
dat[, .N, keyby = .(lks92_imp, lks92_imp_flag)]

# Old-Riga PSUs have been excluded as hard to survey area
dat[, .N, keyby = .(der_iec2019)]
dat[, der_iec2019 := NULL]


# Total number of PSUs un DUs
dat[, .(psu = length(unique(iec2019)), du = .N)]


# Number of PSUs by PSU-strata & statistical regions
dat[, .N, keyby = .(strata2019, iec2019)][, .N]
dat[, .N, keyby = .(reg_stat_kods, iec2019)][, .N]
dat[, .N, keyby = .(strata2019, reg_stat_kods, iec2019)][, .N]


# Strata (16) formed by statistical region and type of territory
dat[, .(psu = length(unique(iec2019)), du = .N),
    keyby = .(strata2019, reg_stat_kods)]

dat[, .N, keyby = .(reg_stat_kods, reg_stat_nosauk)]



# PSU frame
frame_psu <- dat[
  , .(psu_mos = .N, lat = mean(lat), lon = mean(lon)),
  keyby = .(ID_PSU, SORT_PSU, strata2019, reg_stat_kods, iec2019)
]

frame_psu[, sum(psu_mos)] == dat[, .N]


# Overall sampling fraction
f <- sample_size_du / dat[, .N]
f
1 / f

# Limit for certainty PSUs
psu_mos_lim <- sample_size_du_by_psu / f
psu_mos_lim

# Certainty PSUs
frame_psu[, psu_cert := psu_mos > psu_mos_lim]
frame_psu[(psu_cert), .N] # 12


# Sample allocation by certainty PSUs and other PSUs
tab_sample_alloc <- frame_psu[, .(pop_psu = .N, pop_du = sum(psu_mos)),
                              keyby = .(psu_cert)]
tab_sample_alloc


# SU sample size allocation
# Should be multiple of the planned DU sample size by PSU
tab_sample_alloc[
  , n_du_1 := round_preserve_sum(pop_du * f / sample_size_du_by_psu) *
    sample_size_du_by_psu
]
tab_sample_alloc

tab_sample_alloc[, .(f, f_real = n_du_1 / pop_du)]
tab_sample_alloc[, .(psu_cert, n_du_1)]



# PSU stratification
frame_psu[, strata_psu := fifelse(
  test = psu_cert,
  yes  = paste(0, strata2019, reg_stat_kods, iec2019, sep = "-"),
  no   = paste(1, strata2019, reg_stat_kods, sep = "-")
)]



# Sample allocation by strata
tab_strata <- frame_psu[, .(pop_psu = .N, pop_du = sum(psu_mos)),
                        keyby = .(psu_cert, strata_psu)]


# Explicit strata used for stratifying PSUs 
# Required if stratification is used for PSUs; blank otherwise.
tab_strata[, STRAT_PSU := .I]


# Add sample allocation (1)
tab_strata <- merge(tab_strata,
                    tab_sample_alloc[, .(psu_cert, n_du_1)],
                    by = "psu_cert")

# Rounding base
tab_strata[, round_base := fifelse(
  test = psu_cert,
  yes  = 3L, # 2 for initial sample and 1 for reserve
  no   = sample_size_du_by_psu
)]

# Allocate DU sample size proportionaly to number of DUs
tab_strata[
  ,
  n_du := round_preserve_sum(n_du_1 * pop_du / sum(pop_du) / round_base) *
    round_base,
  by = .(psu_cert)
]

tab_strata[, sum(n_du), keyby = .(psu_cert)]

if (tab_strata[, sum(n_du)] != sample_size_du) stop("DU sample alloc.")

# tab_strata[order(n_du)]

tab_strata[, f := n_du / pop_du]
tab_strata[, dw := 1 / f]

tab_strata[, as.list(summary(f)),  keyby = .(psu_cert)]
tab_strata[, as.list(summary(dw)), keyby = .(psu_cert)]



# PSU sample size
tab_strata[, n_psu := fifelse(
  test = psu_cert,
  yes  = 1L,
  no   = n_du %/% sample_size_du_by_psu
)]

tab_strata[, sum(n_psu)] - sample_size_psu
# -2 sampled PSUs as originaly planned

# Reorder STRAT_PSU as the 1st col
setcolorder(tab_strata, "STRAT_PSU")

fwrite(    x = tab_strata, file = "tables/sample-alloc.csv")
write.xlsx(x = tab_strata, file = "tables/sample-alloc.xlsx",
           asTable = T, overwrite = T, zoom = 125, colWidths = "auto")


tab_strata[, .(strata_psu, STRAT_PSU, n_psu, n_du)]

# Add PSU sample size allocation
frame_psu <- merge(frame_psu,
                   tab_strata[, .(strata_psu, STRAT_PSU, n_psu, n_du)],
                   by = "strata_psu")

# Certainty PSUs
frame_psu[(psu_cert)]


# First-stage sampling unit probability of selection
# Required if area PSUs are selected; blank if no PSU selection.
# Format 14.12
frame_psu[, PROB_PSU := inclusionprobabilities(
    a = psu_mos, n = first(n_psu)
  ) |> round(digits = 12), by = .(strata_psu)]

frame_psu[, .(PROB_PSU)]
frame_psu[, .(PROB_PSU)] |> print(digits = 12)
frame_psu[, .(PROB_PSU)] |> print(digits = 14)

all.equal(frame_psu[, sum(PROB_PSU)], tab_strata[, sum(n_psu)])

frame_psu[, as.list(summary(PROB_PSU)), keyby = .(psu_cert)]


# PSU sampling

# Order by PSU strata and PSU ID
setorder(frame_psu, STRAT_PSU, SORT_PSU)

ggplot(data = frame_psu,
       mapping = aes(x = lon, y = lat, size = psu_mos)) +
  geom_point(alpha = .2) +
  coord_map() +
  theme_bw()

ggplot(data = frame_psu,
       mapping = aes(x = lon, y = lat, size = psu_mos)) +
  geom_point(alpha = .2) +
  geom_point(data = frame_psu[(psu_cert)], alpha = .2, colour = "red") +
  coord_map() +
  theme_bw()

ggplot(data = frame_psu,
       mapping = aes(x = lon, y = lat, size = psu_mos,
                     colour = factor(strata2019))) +
  geom_point(alpha = .2) +
  coord_map() +
  theme_bw()

ggplot(data = frame_psu,
       mapping = aes(x = lon, y = lat, size = psu_mos,
                     colour = factor(reg_stat_kods))) +
  geom_point(alpha = .2) +
  coord_map() +
  theme_bw()

ggplot(data = frame_psu[!(psu_cert)],
       mapping = aes(x = lon, y = lat)) +
  geom_path() +
  facet_wrap(facets = vars(STRAT_PSU), scales = "free") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


# Order by PSU strata and PSU ID
setorder(frame_psu, STRAT_PSU, SORT_PSU)

# Select PSU sample
set.seed(185248)
frame_psu[, sample_psu := UPsystematic(pik = PROB_PSU), by = STRAT_PSU]

frame_psu[, .N, keyby = .(psu_cert, sample_psu)]


# DU sample size
frame_psu[!(psu_cert), n_du := sample_size_du_by_psu * sample_psu]
frame_psu[, sum(n_du)] == sample_size_du
frame_psu[, sum(n_du), keyby = .(psu_cert)]


# DU conditional sampling probabilities (on condition PSU is sampled)
# HH probability of selection (within prior-stage clusters, if applicable) 
# Required if household sampling; blank for one-stage designs.
# Format: 14.12
frame_psu[, PROB_HH := (n_du / psu_mos) |> round(digits = 12)]
frame_psu[, .(PROB_HH)]
frame_psu[, .(PROB_HH)] |> print(digits = 12)
frame_psu[, .(PROB_HH)] |> print(digits = 14)

frame_psu[, summary(PROB_HH)]
frame_psu[sample_psu == 1, summary(PROB_HH)]
frame_psu[sample_psu == 1, as.list(summary(PROB_HH)), keyby = .(psu_cert)]
frame_psu[sample_psu == 1, as.list(summary(PROB_PSU * PROB_HH)),
          keyby = .(psu_cert)]
frame_psu[sample_psu == 1, as.list(summary(1 / PROB_PSU / PROB_HH)),
          keyby = .(psu_cert)]

# Sample size test
frame_psu[, sum(n_du)] == sample_size_du


frame_psu[, .(iec2019, STRAT_PSU, strata_psu, psu_cert,
              sample_psu, PROB_PSU, PROB_HH)]

# DU sampling
dat <- merge(
  dat,
  frame_psu[, .(iec2019, STRAT_PSU, psu_mos, psu_cert,
                sample_psu, PROB_PSU, PROB_HH)],
  by = "iec2019"
)


# Order by strata, psu, hh
setorder(dat, STRAT_PSU, SORT_PSU, SORT_HH)

dat[, .(STRAT_PSU, SORT_PSU, SORT_HH)]
dat[, .(STRAT_PSU, SORT_PSU, SORT_HH)] |> anyDuplicated()

set.seed(13852)
dat[, sample_du := UPsystematic(pik = PROB_HH), by = .(STRAT_PSU, ID_PSU)]

dat[, sum(sample_du)] == sample_size_du

dat[sample_du == 1]


# Address test
dat[adrese_ir != adrese_vzd & adrese_vzd != "", .N] # 294
# dat[adrese_ir != adrese_vzd & adrese_vzd != "",
#     .(adrese_ir, adrese_vzd, adr_kods)] |> View()

dat[tolower(adrese_ir) != tolower(adrese_vzd) & adrese_vzd != "", .N] # 12
# dat[tolower(adrese_ir) != tolower(adrese_vzd) & adrese_vzd != "",
#     .(adrese_ir, adrese_vzd, adr_kods)] |> View()


# Plot DU / HH
dat[, sample_du_f := factor(sample_du)]
plot.DT(DT = dat[ID_PSU == frame_psu[sample_psu == 1, sample(ID_PSU, 1)]],
        colour = "sample_du_f")
dat[, sample_du_f := NULL]


# Save DU frame
fwrite(x = dat, file = "data/frame_piaac_sampled.csvy.gz", yaml = T)




# Imputed coords
dat[sample_du == 1, .N, keyby = .(lks92_imp)] # 12
dat[sample_du == 1, .N, keyby = .(lks92_imp, lks92_imp_flag)]


# Sample
dat_sample <- dat[
  sample_du == 1,
  .(CASEID, ID_PSU, ID_HH,
    PROB_PSU, PROB_HH,
    STRAT_PSU, SORT_PSU, SORT_HH,
    psu_mos, psu_cert,
    reg_stat_kods, reg_stat_nosauk,
    adm_terit_kods, adm_terit_nosauk,
    atvk, atvk_nosauk,
    terit_tips_kods, terit_tips_nosauk,
    lks92x, lks92y, lon, lat,
    adr_kods_maja, adr_kods_dziv, adr_kods,
    adrese_ir, flat_num_ir, adrese_vzd,
    pers_sk_16_65)
]


# Overwrite IR address with VZD address (if equal) to make correct usage of case
dat_sample[tolower(adrese_ir) == tolower(adrese_vzd), adrese_ir := adrese_vzd]
# Clear VZD address if equal to IR
dat_sample[tolower(adrese_ir) == tolower(adrese_vzd), adrese_vzd := ""]

# Should manualy check address!!!
dat_sample[adrese_vzd != "", .(adrese_ir, adrese_vzd)]
dat_sample[, adrese_vzd := NULL]


# Subsample flag 
# Required;
# 1 = initial sample (first release),
# 2 = released sample #2,
# 3 = released sample #3,
# …
# 9 = released sample #9.
dat_sample[, id := 1:.N, by = .(STRAT_PSU, ID_PSU)]
dat_sample[, .(STRAT_PSU, ID_PSU, id)]
dat_sample[, .N, keyby = .(id)]

# %%  - modulo (atlikums)
# %/% - integer division (veselā daļa)
dat_sample[(id %% 3L) != 0L, SUBSAMP := 1L] # initial sample
dat_sample[(id %% 3L) == 0L, SUBSAMP := 1L + (id / 3L)] # reserve
dat_sample[SUBSAMP > 6L, SUBSAMP := 6L]

dat_sample[, .N, keyby = .(SUBSAMP)]
dat_sample[, .N, keyby = .(SUBSAMP)][, P := N / first(N)][]

# Remove temp id
dat_sample[, id := NULL]



# Plot sample
dat_sample[, SUBSAMP_f := factor(SUBSAMP)]
plot.DT(DT = dat_sample[ID_PSU == frame_psu[sample_psu == 1,
                                            sample(ID_PSU, 1)]],
        colour = "SUBSAMP_f")
dat_sample[, SUBSAMP_f := NULL]



# Save DU sample
fwrite(    x = dat_sample, file = "data/sample_piaac.csvy", yaml = T)
write.xlsx(x = dat_sample, file = "data/sample_piaac.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)


# Map
map_data <- dat_sample[, .(
  popup_label = paste0("ID_PSU = ", ID_PSU, "<br>",
                       "n = ", .N, "<br>",
                       paste(adrese_ir, collapse = "<br>"))
), keyby = .(ID_PSU, lon, lat)]
# map_data

m <- leaflet(data = map_data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   popup = ~popup_label,
                   stroke = T,
                   clusterOptions = markerClusterOptions())
m  # Print the map

htmlwidgets::saveWidget(widget = m, file = "maps/PIAAC-sample-total.html")
