# Sampling

# Reset
rm(list = ls())

# Packages
library(data.table)
library(openxlsx)
library(sampling)
library(ggplot2)
library(TSP)
library(leaflet)

options(openxlsx.numFmt = NULL)

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
sample_size_psu <- 1842
sample_size_du_by_psu <- 15 # Original and reserve sample

sample_size_du <- sample_size_psu * sample_size_du_by_psu
sample_size_du # Original and reserve sample


# DU frame
dat <- fread("data/frame_piaac.csvy.gz", yaml = T)
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



# PSU frame
frame_psu <- dat[, .(psu_mos = .N, lat = mean(lat), lon = mean(lon)),
                 keyby = .(strata2019, reg_stat_kods, iec2019)]

frame_psu[, sum(psu_mos)] == dat[, .N]


# Overall sampling fraction
f <- sample_size_du / dat[, .N]
f

# Limit for certainty PSUs
psu_mos_lim <- sample_size_du_by_psu / f
psu_mos_lim

# Certainty PSUs
frame_psu[, psu_cert := psu_mos > psu_mos_lim]
frame_psu[(psu_cert)] # 12


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


# Add sample allocation (1)
tab_strata <- merge(tab_strata,
                    tab_sample_alloc[, .(psu_cert, n_du_1)],
                    by = "psu_cert")

# Rounding base
tab_strata[, round_base := fifelse(
  test = psu_cert,
  yes  = 3, # 2 for original sample and 1 for reserve sample
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
tab_strata[, summary(dw)]


# PSU sample size
tab_strata[, n_psu := fifelse(
  test = psu_cert,
  yes  = 1,
  no   = n_du / sample_size_du_by_psu
)]

tab_strata[, sum(n_psu)] - sample_size_psu
# -2 sampled PSUs as originaly planned

fwrite(    x = tab_strata, file = "tables/sample-alloc.csv")
write.xlsx(x = tab_strata, file = "tables/sample-alloc.xlsx",
           asTable = T, overwrite = T, zoom = 125, colWidths = "auto")



# Add PSU sample size allocation
frame_psu <- merge(frame_psu,
                   tab_strata[, .(strata_psu, n_psu, n_du)],
                   by = "strata_psu")

# Certainty PSUs
frame_psu[(psu_cert)]


# PSU sampling probabilities
frame_psu[, psu_prob := inclusionprobabilities(a = psu_mos, n = first(n_psu)),
          by = .(strata_psu)]

frame_psu[, sum(psu_prob)] == tab_strata[, sum(n_psu)]

frame_psu[, as.list(summary(psu_prob)), keyby = .(psu_cert)]


# PSU sampling

# Order by PSU strata and PSU ID
setorder(frame_psu, strata_psu, iec2019)

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
  facet_wrap(facets = vars(strata_psu), scales = "free") +
  theme_bw()


setorder(frame_psu, strata_psu, iec2019)
set.seed(185248)
frame_psu[, sample_psu := UPsystematic(pik = psu_prob), by = strata_psu]

frame_psu[, .N, keyby = .(psu_cert, sample_psu)]


# DU sample size
frame_psu[!(psu_cert), n_du := sample_size_du_by_psu * sample_psu]
frame_psu[, sum(n_du)]
frame_psu[, sum(n_du), keyby = .(psu_cert)]


# DU conditional sampling probabilities (on condition PSU is sampled)
frame_psu[, du_prob := n_du / psu_mos]
frame_psu[, summary(du_prob)]
frame_psu[, as.list(summary(du_prob)), keyby = .(sample_psu)]
frame_psu[, as.list(summary(psu_prob * du_prob)), keyby = .(sample_psu)]

# Sample size test
frame_psu[, sum(n_du)] == sample_size_du

frame_psu[, .(iec2019, strata_psu, psu_cert, sample_psu, psu_prob, du_prob)]


# DU sampling
dat <- merge(
  dat,
  frame_psu[, .(iec2019, strata_psu, psu_cert, sample_psu, psu_prob, du_prob)],
  by = "iec2019"
)

# Order buildings by TSP

dat_building <- dat[, .(iec2019, ind_maja, adr_kods_maja, lks92x, lks92y)] |>
  unique()
dat_building[, anyDuplicated(ind_maja)]
dat_building[adr_kods_maja != "", anyDuplicated(adr_kods_maja)]

# Functions ####
sort.DT <- function(DT, method = "arbitrary_insertion") {
  tour <- solve_TSP(ETSP(DT[, .(lks92x, lks92y)]), method = method)
  DT <- DT[tour]
  DT[, i := .I]
  DT[]
}

sort.DT.by <- function(DT, by, method = "arbitrary_insertion") {
  by.values <- unique(DT[, get(by)])
  rbindlist(lapply(by.values, function(x) sort.DT(DT[get(by) == x])))
}

plot.DT <- function(DT, group = NULL, colour = NULL, size = NULL,
                    title = NULL, subtitle = NULL) {
  ggplot(DT, aes_string(x = "lks92x", y = "lks92y", group = group)) +
    geom_point(aes_string(colour = colour, size = size)) +
    geom_path(linetype = "dashed") +
    coord_fixed() +
    ggtitle(label = title, subtitle = subtitle) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

plot.DT.by <- function(DT, by, group = NULL, colour = NULL, size = NULL,
                       title = NULL) {
  by.values <- unique(DT[, get(by)])
  lapply(by.values, function(x) plot.DT(DT = DT[get(by) == x],
                                        group = group, colour = colour,
                                        size = size,
                                        title = title,
                                        subtitle = paste(by, x, sep = ": ")))
}

tmp <- dat_building[iec2019 == min(iec2019)]
plot.DT(tmp)
sort.DT(DT = tmp) |> plot.DT()
rm(tmp)

set.seed(174754)
dat_building <- sort.DT.by(DT = dat_building, by = "iec2019")
setnames(dat_building, "i", "build_order")

plot.DT(dat_building[iec2019 == sample(iec2019, 1)])

dat_building[, .(iec2019, ind_maja, build_order)]



# Add building order to the DU frame
dat <- merge(dat,
             dat_building[, .(iec2019, ind_maja, build_order)],
             by = c("iec2019", "ind_maja"))


# Flat numbers
# dat[n_flats > 1, .(adrese_ir, flat_num_ir)] |> View()
dat[n_flats > 1, .N, keyby = .(flat_num_ir)]
dat[n_flats > 1, .N, keyby = .(flat_num_ir)][order(N)]

# Is any flat without flat number?
dat[n_flats > 1 & is.na(flat_num_ir)]

# Order by strata, PSU, building, flat
setorder(dat, strata_psu, iec2019, build_order, flat_num_ir)

set.seed(13852)
dat[, sample_du := UPsystematic(pik = du_prob), by = .(strata_psu, iec2019)]

dat[, sum(sample_du)] == sample_size_du

dat[sample_du == 1]


# Address test
dat[adrese_ir != adrese_vzd & adrese_vzd != "", .N]
# dat[adrese_ir != adrese_vzd & adrese_vzd != "",
#     .(adrese_ir, adrese_vzd, adr_kods)] |> View()

dat[tolower(adrese_ir) != tolower(adrese_vzd) & adrese_vzd != "", .N]
# dat[tolower(adrese_ir) != tolower(adrese_vzd) & adrese_vzd != "",
#     .(adrese_ir, adrese_vzd, adr_kods)] |> View()



# Sve DU frame
fwrite(x = dat, file = "data/frame_piaac_sampled.csvy.gz", yaml = T)




# Sample
dat_sample <- dat[
  sample_du == 1,
  .(strata_psu, iec2019, psu_cert, build_order,
    psu_prob, du_prob,
    reg_stat_kods, reg_stat_nosauk,
    adm_terit_kods, adm_terit_nosauk,
    atvk, atvk_nosauk,
    lks92x, lks92y, lon, lat,
    adr_kods_maja, adr_kods_dziv, adr_kods,
    adrese_ir, flat_num_ir, adrese_vzd,
    pers_sk_16_65)
]


# Overwrite IR address with VZD address to make correct usage of case
dat_sample[tolower(adrese_ir) == tolower(adrese_vzd), adrese_ir := adrese_vzd]

# Clear VZD address if equal to IR
dat_sample[tolower(adrese_ir) == tolower(adrese_vzd), adrese_vzd := ""]
dat_sample[adrese_vzd != "", .(adrese_ir, adrese_vzd)]
if (dat_sample[, all(adrese_vzd == "")]) dat_sample[, adrese_vzd := NULL]

dat_sample[, id := .I]

# Sample type: 0 - main sample, 1 - reserve sample
dat_sample[, sample_type := as.integer(id %% 3 == 0)]
dat_sample[, .N, keyby = .(sample_type)]


# Save DU sample
fwrite(    x = dat_sample, file = "data/sample_piaac.csvy", yaml = T)
write.xlsx(x = dat_sample, file = "data/sample_piaac.xlsx",
           overwrite = T, colWidths = "auto",
           firstActiveRow = 2, firstActiveCol = 3)


# Map
map_data <- dat_sample[, .(n = .N,
                           popup_label = paste(adrese_ir, collapse = "<br>")),
                       keyby = .(iec2019, build_order, lon, lat, sample_type)]
map_data

m <- leaflet(data = map_data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup_label,
                   radius = ~10 * n, stroke = T,
                   clusterOptions = markerClusterOptions())
m  # Print the map

m_original <- leaflet(data = map_data[sample_type == 0]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup_label,
                   radius = ~10 * n, stroke = T,
                   clusterOptions = markerClusterOptions())
m_original  # Print the map


m_reserve <- leaflet(data = map_data[sample_type == 1]) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup_label,
                   radius = ~10 * n, stroke = T,
                   clusterOptions = markerClusterOptions())
m_reserve  # Print the map
