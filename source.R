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
  
  library(spdep)
  
}

dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pop", showWarnings = FALSE, recursive = TRUE)
dir.create("output/rail_map", showWarnings = FALSE, recursive = TRUE)
dir.create("output/road_map", showWarnings = FALSE, recursive = TRUE)
dir.create("data_fmt", showWarnings = FALSE, recursive = TRUE)

ggsave_pdf_svg <- function(outdir = "output",
                           file_name, f_width = 3, f_height = 3){
  ggsave(file.path(outdir, paste0(file_name, ".pdf")),
         device = cairo_pdf,     family = "Arial",
         width = f_width,    height = f_height  )
  
  ggsave(file.path(outdir, paste0(file_name, ".svg")),
         device = svglite, fix_text_size = FALSE, 
         width = f_width, height = f_height, bg = "transparent")
}
