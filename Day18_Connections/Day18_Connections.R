# 30DayChartChallenge Day 18 - Connections
# tutorial: https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html
# book: https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html#customize-sector-labels

# load packages
library(tidyverse)
library(circlize)

# create adjacency matrix 
d = matrix(c(0, 10, 5, 10,
             10, 0, 0, 5,
             20, 10, 0, 40,
             30, 30, 60, 0), 4)
rownames(d) = c("litterbox", "balcony", "bed", "tree")
colnames(d) = c("litterbox", "balcony", "bed", "tree")

# plot chord diagram
# it's good this is made up data, because apparently some points are out of the plotting region - but for my purposes it doesn't really matter
d %>% 
  chordDiagram(transparency = c(0.1, 0.75, 0.75, 0.75), 
               grid.col = c(litterbox = "#828585", 
                            balcony = "#fd8f24", 
                            bed = "#919c4c", 
                            tree = "#e68c7c"),
               annotationTrack = "grid",
               preAllocateTracks = list(track.height = 0.25*max(strwidth(unlist(dimnames(d))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  if(abs(xplot[2] - xplot[1]) < 10) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5), col = "black")
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "bending.inside", 
                niceFacing = TRUE, adj = c(0.5, 0), col= "black")
  }
}, bg.border = NA)

title(main = "\n My Cat's journeys")

