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
source(file = "code/round_preserve_sum.R")

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
  keyby = .(ID_PSU, SORT_PSU,
            strata2019, reg_stat_kods, reg_stat_nosauk, iec2019)
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
                        keyby = .(psu_cert, strata_psu, strata2019,
                                  reg_stat_kods, reg_stat_nosauk)]


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


# Save PSU frame
fwrite(x = frame_psu, file = "data/frame_psu.csvy", yaml = T)

# Save DU frame
fwrite(x = dat, file = "data/frame_piaac_sampled.csvy.gz", yaml = T)




# Imputed coords
dat[sample_du == 1, .N, keyby = .(lks92_imp)] # 12
dat[sample_du == 1, .N, keyby = .(lks92_imp, lks92_imp_flag)]


# Field Trial sample
dat[, max(STRAT_PSU)]
dat[, max(ID_PSU)]

# dat[, .N, keyby = .(atvk, atvk_nosauk)]

dat[, STRAT_PSU_FT := 99L]

dat[atvk_nosauk == "Ventspils" &
      grepl("Sarkanmuižas dambis", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9901L]
dat[atvk_nosauk == "Rīga" &
      grepl("Vecmīlgrāvja 1. līnija|Kāvu iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9902L]
dat[atvk_nosauk == "Carnikavas pag." &
      grepl("Liepu iela|Tulpju iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9903L]
dat[atvk_nosauk == "Baldone" &
      grepl("Vanagkalnu iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9904L]
dat[adm_terit_nosauk == "Kuldīgas nov." & atvk_nosauk == "Kurmāles pag.",
    ID_PSU_FT := 9905L]
dat[atvk_nosauk == "Gulbene" &
      grepl("O. Kalpaka iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9906L]
dat[adm_terit_nosauk == "Valmieras nov." &
      (atvk_nosauk == "Skaņkalnes pag." | atvk_nosauk == "Mazsalacas pag."),
    ID_PSU_FT := 9907L]
dat[adm_terit_nosauk == "Tukuma nov." & atvk_nosauk == "Smārdes pag." &
      grepl("Milzkalne", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9908L]
dat[adm_terit_nosauk == "Jēkabpils nov." & atvk_nosauk == "Mežāres pag." &
      grepl("Rozessala", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9909L]
dat[(atvk_nosauk == "Līvāni" &
       grepl("Rīgas iela", adrese_ir, ignore.case = T)) |
      (adm_terit_nosauk == "Jēkabpils nov." & atvk_nosauk == "Kūku pag."),
    ID_PSU_FT := 9910L]
dat[atvk_nosauk == "Brocēni" & grepl("Skolas iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9911L]
dat[atvk_nosauk == "Rīga" & grepl("Tilta iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9912L]
dat[atvk_nosauk == "Krāslava" & grepl("Sporta iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9913L]
dat[atvk_nosauk == "Rīga" & grepl("Ģertrūdes iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9914L]
dat[atvk_nosauk == "Rīga" & grepl("Slokas iela", adrese_ir, ignore.case = T),
    ID_PSU_FT := 9915L]

dat[, SORT_PSU_FT := ID_PSU_FT]

dat[, .N, keyby = .(ID_PSU_FT)]
dat[, (sum(sample_du)), keyby = .(ID_PSU_FT)]
dat[, (.N - sum(sample_du)), keyby = .(ID_PSU_FT)]

# Frame for the FT
dat[, frame_ft := !is.na(ID_PSU_FT) & sample_du == 0L]
dat[(frame_ft), .N, keyby = .(ID_PSU_FT)]

# ID_HH_FT
dat[(frame_ft), ID_HH_FT := 1:.N, by = .(ID_PSU_FT)]
dat[, SORT_HH_FT := ID_HH_FT]

# Sampling probabilities for the FT
dat[, PROB_PSU_FT := as.numeric(frame_ft)]
dat[ (frame_ft), PROB_HH_FT := frame_ft * 10 / sum(frame_ft), by = .(ID_PSU_FT)]
dat[!(frame_ft), PROB_HH_FT := 0]
dat[(frame_ft)]
dat[, lapply(.SD, sum), .SDcols = c("frame_ft", "PROB_HH_FT"),
    keyby = .(ID_PSU_FT)]

# Sample FT
set.seed(230239)
dat[, sample_ft := sampling::UPsystematic(pik = PROB_HH_FT), by = .(ID_PSU_FT)]
dat[, .(N = sum(frame_ft), n = sum(sample_ft)), keyby = .(ID_PSU_FT)]

# dat[sample_ft == 1L, .(ID_PSU_FT, adrese_ir)][order(ID_PSU_FT)] |> View()

# Sample
dat_sample <- dat[
  sample_du == 1L | sample_ft == 1L,
  .(CASEID,
    sample_du, sample_ft,
    STRAT_PSU, STRAT_PSU_FT,
    ID_PSU, ID_PSU_FT,
    SORT_PSU, SORT_PSU_FT,
    PROB_PSU, PROB_PSU_FT,
    ID_HH, ID_HH_FT,
    SORT_HH, SORT_HH_FT,
    PROB_HH, PROB_HH_FT,
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
dat_sample[sample_du == 1L, id := 1:.N, by = .(STRAT_PSU, ID_PSU)]
dat_sample[sample_du == 1L, max_id := max(id), by = .(STRAT_PSU, ID_PSU)]
# dat_sample[, angle := 2 * pi * id / max_id]

foo <- function(max_id = 15L) {
  df <- data.table(id = 0:max_id, max_id)
  df[, x := id / max(id) * 2 * pi]
  df[, y := 1L]
  return(df[])
}

tab <- lapply(5:8 * 3, foo) |> rbindlist()
tab

ggplot(data = tab, mapping = aes(x = x, y = y)) +
  geom_line() +
  geom_label(mapping = aes(label = id)) +
  coord_polar() +
  scale_x_continuous(breaks = round(0:7 / 4 * pi, 2),
                     labels = paste0(0:7 / 4, "π")) +
  scale_y_continuous(breaks = NULL, limits = c(0, 1.3)) +
  facet_wrap(facets = vars(max_id), nrow = 2) +
  theme_bw()



dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU),
                            .(STRAT_PSU, ID_PSU, id, max_id)]
dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU),
                            .(STRAT_PSU, ID_PSU, id, max_id)]
dat_sample[sample_du == 1L, .N, keyby = .(id)]
dat_sample[sample_du == 1L, .N, keyby = .(max_id)]

# %%  - modulo (atlikums)
# %/% - integer division (veselā daļa)

set.seed(3544)
dat_sample[sample_du == 1L, rand := rep(sample(0:2, 1), max(id)),
           by = .(ID_PSU)]
dat_sample[sample_du == 1L, .N, keyby = .(rand)]

# Initial (main) sample
dat_sample[(sample_du == 1L) & (id %% 3L != rand),
           SUBSAMP := rep(1:(first(max_id) / 3), each = 2),
           by = .(ID_PSU)]
dat_sample[SUBSAMP > 5L, SUBSAMP := 5L]
dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU)]
dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU)]

# Reserve sample with random spatial distribution
set.seed(5108)
dat_sample[(sample_du == 1L) & (id %% 3L == rand),
           SUBSAMP := 5L + sample(1:(first(max_id) / 3)),
           by = .(ID_PSU)]
# dat_sample[SUBSAMP > 10L]
dat_sample[SUBSAMP > 10L, SUBSAMP := 10L]
dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU)]
dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU)]

dat_sample[sample_du == 1L, .N, keyby = .(SUBSAMP)]
dat_sample[sample_du == 1L, .N, keyby = .(SUBSAMP)][
  , P := N / sum(N[SUBSAMP <= 5])][]

# FT
dat_sample[sample_ft == 1L, SUBSAMP := 1L]

# Remove temp variables
dat_sample[, id := NULL]
dat_sample[, max_id := NULL]
# dat_sample[, angle := NULL]
dat_sample[, rand := NULL]

dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)]
dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)][SUBSAMP <= 5]
dat_sample[sample_du == 1L][ID_PSU == first(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)][SUBSAMP >= 6]

dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)]
dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)][SUBSAMP <= 5]
dat_sample[sample_du == 1L][ID_PSU == last(ID_PSU),
                            .(ID_PSU, ID_HH, SUBSAMP)][SUBSAMP >= 6]


# Plot sample
dat_sample[, SUBSAMP_f := factor(SUBSAMP)]
plot.DT(DT = dat_sample[sample_du == 1L][ID_PSU == frame_psu[sample_psu == 1,
                                                             sample(ID_PSU, 1)]],
        colour = "SUBSAMP_f", size = "SUBSAMP")
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
), keyby = .(sample_du, sample_ft, ID_PSU, lon, lat)]
# map_data

m <- leaflet(data = map_data[sample_du == 1L]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   popup = ~popup_label,
                   stroke = T,
                   clusterOptions = markerClusterOptions())
m  # Print the map

m_ft <- leaflet(data = map_data[sample_ft == 1L]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   popup = ~popup_label,
                   stroke = T,
                   clusterOptions = markerClusterOptions())
m_ft  # Print the map

htmlwidgets::saveWidget(widget = m, file = "maps/PIAAC-sample-total.html")
htmlwidgets::saveWidget(widget = m_ft, file = "maps/PIAAC-sample-FT.html")
