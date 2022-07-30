# TSP sort and plot functions

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
