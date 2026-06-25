# source.R
# load all packages

# library ----
{
  library(sf)         # spatial data
  library(tigris)     # US Census shapefiles
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(tidycensus)
  library(forcats)
  library(broom)
  library(purrr)
  
  library(stringr)
  library(ggplot2)
  library(viridis)
  library(ggrastr)
  library(ggspatial)
  library(svglite)
  library(ggnewscale)
  library(patchwork)
  
  library(survival)
  library(car)
}

dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("data_fmt", showWarnings = FALSE, recursive = TRUE)
