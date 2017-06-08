# setup -------------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(feather)
library(sp)
library(rgdal)
library(fuzzyjoin)
library(ncdf4)
library(SearchTrees)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))


# Prepare data ------------------------------------------------------------

## Save CSV files to feather format, much faster
source("R/csv_to_feather.R") ## Takes ~4-5 minutes
source("R/process_primary_production.R") ## Takes ~10 minutes

# graphics ----------------------------------------------------------------

files <- list.files("graphs/", "pdf", full.names = TRUE)
map(files, embed_fonts)
