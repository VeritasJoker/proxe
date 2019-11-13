library(shiny)
library(xlsx)
library(ggplot2)
library(DT)
library(beeswarm)
library(stringr)
library(pheatmap)
library(readxl)
library(gplots)
library(RColorBrewer)
library(plyr)
library(ComplexHeatmap)
library(tidyr)
library(dplyr)
library(forcats)
library(markdown)
library(shinyalert)

# for loading of objects to be used in both ui.R and server.R
# a la http://shiny.rstudio.com/articles/scoping.html

loadedGlobal <- load("pre-compiled.RData")

print(loadedGlobal)

# developer code for manually stepping through all loaded objects
if (F) {
  for (i in 1:length(loadedGlobal)) {
    print(loadedGlobal[i])
    print(str(get(loadedGlobal[i])))
    readline(prompt = "Press [enter] to continue")
  }
}

# # make lists of which variables to show as options in UI
numeric_cols_vis <- which(sapply(df[, 1:obInvisRet_ind], is.numeric))
factor_cols_vis <- which(sapply(df[, 1:obInvisRet_ind], is.factor))
solid_numeric_cols_vis <- which(sapply(solid, is.numeric))
solid_factor_cols_vis <- which(sapply(solid, is.factor))


for (u in list.files("ui", pattern = "*.R", recursive = TRUE, full.names = TRUE)) {
  source(u)
}

for (s in list.files("server", pattern = "*.R", recursive = TRUE, full.names = TRUE)) {
  source(s)
}

# note ./global.R is run before any of this.

op <- par(no.readonly = TRUE)

# warn if WHO_Classification is a factor -- this was a temporary fix in clean_liquid.R for production of a contingency table 2/2016.
if (class(df$`WHO Classification`) == "factor") warning("Reminder: WHO Classification is a factor even though it is highly variable.")
