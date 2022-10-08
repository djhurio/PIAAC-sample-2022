# Sample map

quarto::quarto_render(input = "code/51-map.qmd")

file.copy(from = "code/51-map.html",
          to = "maps/PIAAC-2022-SUBSAMP1.html",
          overwrite = T)
file.remove("code/51-map.html")
