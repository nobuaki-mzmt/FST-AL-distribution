# library ----
{
  library(sf)         # spatial data
  library(tigris)     # US Census shapefiles
  library(dplyr)
  library(tidyr)
  library(tidycensus)
  
  library(stringr)
  library(ggplot2)
  library(viridis)
  library(ggrastr)
  library(ggspatial)
  
  library(survival)
  library(car)
}

dir.create("data_fmt", showWarnings = FALSE, recursive = TRUE)

# FST data ---- 
{
  df <- read.csv("FSTrecords.csv")
  
  plot_data <- df %>%
    count(FirstDetectedYear) %>%
    complete(FirstDetectedYear = (min(df$FirstDetectedYear)-5):max(df$FirstDetectedYear), 
             fill = list(n = 0)) %>%
    mutate(CumulativeTotal = cumsum(n))
  
  ggplot(plot_data, aes(x = FirstDetectedYear, y = CumulativeTotal)) +
    geom_area(fill = "#FFF176", alpha = 0.4) +
    geom_line(color = "#E65100", size = 1) +
    geom_point(data = filter(plot_data, n > 0), color = "#E65100", size = 2) +
    scale_x_continuous(limits = c(1980, 2025), 
                       breaks = seq(1985, 2025, 10)) +
    theme_classic() +
    labs(x = "Year", y = "Number of detected counties") +
    theme(aspect.ratio = 3/4)
  ggsave("output/time_development_counties.pdf", 
         device = cairo_pdf, family = "Arial",
         width = 3, height = 3)
}

# Alabama census data ----
if(!file.exists("data_fmt/alabama_census.rda")){
  df <- read.csv("FSTrecords.csv")
  
  options(tigris_use_cache = TRUE)
  
  # Alabama county was organized until 1903. So data of 2025 covers the whole C. formosanus records
  al_counties <- counties(state = "AL", cb = TRUE, year = 2025)
  al_counties <- st_transform(al_counties, 3857)
  al_map_data <- al_counties %>% left_join(df, by = c("NAME" = "County"))
  
  # road data
  # primary_secondary_roads() function is available 2011-2025
  {
    analyze_roads <- function(target_year, al_counties = al_counties){
      print(paste("Processing year:", target_year))
      
      al_roads <- primary_secondary_roads(state = "AL", year = target_year)
      al_roads <- st_transform(al_roads, 3857) # Using meters-based projection
      
      roads_intersected <- st_intersection(al_roads, al_counties) %>%
        st_collection_extract("LINESTRING")
      
      roads_data <- roads_intersected %>%
        mutate(length_km = as.numeric(st_length(.)) / 1000) %>%
        st_drop_geometry()
      
      df_road <- roads_data %>%
        group_by(NAME) %>%
        summarise(
          total_road_km = sum(length_km, na.rm = TRUE),
          total_IS_km   = sum(length_km[RTTYP == "I"], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(year = target_year)
      
      return(list(al_roads = al_roads, df_road = df_road))
    }
    
    years <- 2011:2025
    road_sensitivity_data <- years %>% 
      set_names() %>% 
      map(~analyze_roads(target_year = .x, al_counties = al_counties))
  }
  
  # rail data
  # rails() function is available 2011-2025
  {
    analyze_rails <- function(target_year, al_counties = al_counties){
      print(paste("Processing year:", target_year))
      
      us_rails <- rails(year = target_year)
      al_rails <- us_rails %>%
        st_transform(st_crs(al_counties)) %>%
        st_filter(al_counties)
      al_rails_main <- al_rails %>% filter(MTFCC == "R1011") # R1011 Railroad Feature (Main, Spur, or Yard) 
      al_rails_main <- st_transform(al_rails_main, 3857) # Using meters-based projection
      
      rails_by_county <- st_intersection(al_rails_main, al_counties)
      rails_by_county$rail_length_m <- as.numeric(st_length(rails_by_county))
      rails_cleaned <- st_collection_extract(rails_by_county, "LINESTRING")
      
      df_rail <- rails_cleaned %>%
        st_drop_geometry() %>%
        group_by(NAME) %>%
        summarise(
          rail_total_length_km = sum(rail_length_m)/1000,
          num_rail_companies = n_distinct(FULLNAME),
          .groups = "drop"
        )
      
      return(list(al_rails_main = al_rails_main, df_rail = df_rail))
    }
    
    years <- 2011:2025
    rail_sensitivity_data <- years %>% 
      set_names() %>% 
      map(~analyze_rails(target_year = .x, al_counties = al_counties))
  }
    
  # population
  {
    # download the csv data from NHGIS (renamed as needed)
    # NHGIS county-level census data (1980, 1990, 2000, 2010, 2020; nominal geographic integration).
    # Includes total population and urban/rural population counts for U.S. counties.
    df_pop <- read.csv("nhgis0002_ts_nominal_county.csv", header = T)
    df_pop <- df_pop |> filter(STATE == "Alabama") |>
      mutate(
        NAME = str_remove(COUNTY , " County")
      ) |>
      select(NAME, year = YEAR, Total_pop = AV0AA)
    
    # data of 2024 is obtained from get_estimates
    al_pop <- get_estimates(
      geography = "county",
      product = "population",
      state = "AL",
      year = 2024,
      geometry = TRUE) %>%
      mutate(
        NAME = str_remove(NAME, ", Alabama"),
        NAME = str_remove(NAME, " County")
      ) 
    al_pop_2024 <- al_pop
    al_pop <- al_pop %>% filter(variable == "POPESTIMATE") |> 
      select(-variable, -year, -value)
    
    al_pop <- al_pop  |> left_join(df_pop, by= "NAME")
    
    al_pop_2024 <- al_pop_2024 %>% filter(variable == "POPESTIMATE") |>
      rename(Total_pop = value) |>
      select(colnames(al_pop))
    
    al_pop <- rbind(al_pop, al_pop_2024)
    
    al_geo <- counties(state = "AL", cb = FALSE, year = 2025) %>%
      select(NAME, ALAND) 
    
    al_pop <- al_pop %>%
      left_join(st_drop_geometry(al_geo), by = "NAME") %>%
      mutate(
        area_sq_km = ALAND / 1000000,
        density = Total_pop / area_sq_km
      ) %>%
      select(NAME, year, Total_pop, area_sq_km, density)
  }
  
  save(al_map_data, road_sensitivity_data, rail_sensitivity_data, al_pop, file = "data_fmt/alabama_census.rda")
}

# plot maps ----
{
  load("data_fmt/alabama_census.rda")

  # FST detection year
  {
    p_FST <- ggplot(al_map_data) +
      geom_sf(aes(fill = FirstDetectedYear), color = "white", size = 0.2) +
      scale_fill_viridis_c(option = "viridis", name = "First detection year") +
      coord_sf(datum = st_crs(4326)) +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude") +
      theme(
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank()
      )
    ggsave(plot = p_FST, filename = "output/FST_map.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 3, height = 3)
  }
  
  # road map
  {
    # each year
    year_list <- names(road_sensitivity_data)
    for(i_year in 1:length(year_list)){
      al_roads <- road_sensitivity_data[[i_year]]$al_roads
      al_interstates <- al_roads %>% filter(RTTYP == "I")
      ggplot() +
        geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
        rasterise(geom_sf(data = al_roads, aes(color = "Secondary Roads"), size = 0.3), dpi = 300) +
        geom_sf(data = al_interstates, aes(color = "Interstates"), size = 0.5) +
        scale_color_manual(
          name = "Road",
          values = c("Secondary Roads" = "#333333", "Interstates" = "#EE6677")
        ) +
        theme_void() +
        ggtitle(year_list[i_year])
      ggsave(filename = sprintf("output/road_map/road_map_%s.pdf", year_list[i_year]), 
             device = cairo_pdf, family = "Arial",
             width = 3, height = 3)
    }
    
    # aggregate plot
    combined_roads <- purrr::imap_dfr(road_sensitivity_data, function(val, name) {
      val$al_roads %>% mutate(year = name)
    })
    combined_interstates <- combined_roads %>% filter(RTTYP == "I")
    
    p <- ggplot() +
      geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
      rasterise(geom_sf(data = combined_roads, aes(color = "Secondary Roads"), size = 0.3), dpi = 300) +
      geom_sf(data = combined_interstates, aes(color = "Interstates"), size = 0.5) +
      facet_wrap(~year, ncol = 5) +
      scale_color_manual(
        name = "Road", values = c("Secondary Roads" = "#333333", "Interstates" = "#EE6677")
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 12, face = "bold"), 
        legend.position = "none"
      )
    
    ggsave(filename = "output/road_maps_all_years.pdf", 
           plot = p,
           device = cairo_pdf, family = "Arial",
           width = 9, height = 9)
  }
  
  # rail
  {
    # each year
    year_list <- names(rail_sensitivity_data)
    for(i_year in 1:length(year_list)){
      al_rails_main <- rail_sensitivity_data[[i_year]]$al_rails_main
      ggplot() +
        geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
        geom_sf(data = al_rails_main, color = "#4477AA", size = 0.4) +
        theme_void() +
        ggtitle(year_list[i_year])
      ggsave(filename = sprintf("output/rail_map/rail_map_%s.pdf", year_list[i_year]), 
             device = cairo_pdf, family = "Arial",
             width = 3, height = 3)
    }
    
    # aggregate plot
    combined_rails <- purrr::imap_dfr(rail_sensitivity_data, function(val, name) {
      val$al_rails_main %>% mutate(year = name)
    })
  
    p <- ggplot() +
      geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
      rasterise(geom_sf(data = combined_rails,  color = "#4477AA", size = 0.4, dpi = 300)) +
      facet_wrap(~year, ncol = 5) +
      scale_color_manual(
        name = "Road", values = c("Secondary Roads" = "#333333", "Interstates" = "#EE6677")
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 12, face = "bold"), 
        legend.position = "none"
      )
    
    ggsave(filename = "output/rail_maps_all_years.pdf", 
           plot = p,
           device = cairo_pdf, family = "Arial",
           width = 9, height = 9)
  }
  
  # overlay
  {
    al_interstates <- road_sensitivity_data[["2024"]]$al_roads |> filter(RTTYP == "I")
    p_FST + geom_sf(data = al_interstates, color = "red", size = 0.3)  +
      labs(title = "Interstates (as of 2024) overlayed")
    ggsave("output/FST_interstate.pdf", 
           device = cairo_pdf, family = "Arial",
           width = 4, height = 4)
    
    al_rails_main <- rail_sensitivity_data[["2011"]]$al_rails_main
    p_FST + geom_sf(data = al_rails_main, color = "blue", size = 0.25) +
      labs(title = "Railroads (as of 2011) overlayed")
    ggsave("output/FST_railroad.pdf", , 
           device = cairo_pdf, family = "Arial",
           width = 4, height = 4)
  }
  
  # population
  year_list <- al_pop |> pull(year) |> unique()
  for( i_year in year_list){
    ggplot() +
      geom_sf(data = al_pop |> filter(year == i_year), 
              aes(fill = density), color = "white", size = 0.1) +
      scale_fill_viridis_c(
        option = "magma", 
        labels = scales::comma,
        name = "People per\nsq km"
      ) +
      theme_void() +
      labs(title = sprintf("Alabama Population Density %d", i_year))
    
    ggsave(sprintf("output/pop/AL_population_density_%d.pdf", i_year), 
           device = cairo_pdf, family = "Arial",
           width = 4, height = 4)
  }
  
  ggplot() +
    geom_sf(data = al_pop, 
            aes(fill = density), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      option = "magma", 
      labels = scales::comma,
      name = "People per\nsq km"
    ) +
    facet_wrap( ~ year, ncol = 5) +
    theme_void() +
    labs(title = sprintf("Alabama Population Density"))+
    theme(legend.position = "bottom")
  
  ggsave(sprintf("output/AL_population_density.pdf"), 
         device = cairo_pdf, family = "Arial",
         width = 8, height = 4)
  
}

# Temperature data ----
if(!file.exists("data_fmt/df_alabama_climate.rda")){
  if(!file.exists("climdiv-tmpccy.txt")){
    urls <- c(
      avg = "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpccy-v1.0.0-20260604",
      min = "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmincy-v1.0.0-20260604"
    )
    files <- c(avg = "climdiv-tmpccy.txt", min = "climdiv-tmincy.txt")
    walk2(urls, files, ~ download.file(.x, .y, quiet = TRUE))
  }
  
  parse_noaa_file <- function(file_path) {
    lines <- readLines(file_path)
    df_meta <- tibble(
      state_fips  = substr(lines, 1, 2),
      county_fips = substr(lines, 3, 5),
      element     = substr(lines, 6, 7),
      year        = as.integer(substr(lines, 8, 11))
    )
    
    monthly_text <- str_squish(substr(lines, 12, nchar(lines)))
    monthly_mat  <- do.call(rbind, str_split(monthly_text, "\\s+"))
    class(monthly_mat) <- "numeric"
    
    monthly_mat[monthly_mat < -99.9] <- NA
    
    colnames(monthly_mat) <- paste0("month_", 1:12)
    
    bind_cols(df_meta, as.data.frame(monthly_mat))
  }
    
  df_avg <- parse_noaa_file(files["avg"]) |> 
    filter(state_fips == "01", year >= 1980, year < 2026, element == "02") |> 
    rowwise() |> 
    mutate(
      temp_mean_F = mean(c_across(starts_with("month_")), na.rm = TRUE),
      temp_mean_C = (temp_mean_F - 32) * 5/9
    ) |> 
    ungroup() |> 
    dplyr::select(state_fips, county_fips, year, temp_mean_C)
  
  df_min <- parse_noaa_file(files["min"]) |> 
    filter(state_fips == "01", year >= 1980, year < 2026, element == "28") |> 
    rowwise() |> 
    mutate(
      temp_min_F = min(c_across(starts_with("month_")), na.rm = TRUE),
      temp_min_C = (temp_min_F - 32) * 5/9
    ) |> 
    ungroup() |> 
    dplyr::select(state_fips, county_fips, year, temp_min_C)
  
  df_alabama_climate <- left_join(
    df_avg, df_min, by = c("state_fips", "county_fips", "year")
  )
  
  al_counties <- counties(state = "AL", cb = TRUE, year = 2024)
  al_counties <- al_counties |> select(COUNTYFP, NAME) |> st_drop_geometry()
  
  df_alabama_climate <- df_alabama_climate |> 
    left_join(al_counties, by = join_by(county_fips == "COUNTYFP")) |>
    select(-state_fips, -county_fips)
  
  save(df_alabama_climate, file = "data_fmt/df_alabama_climate.rda")
}

# clean data ----
{
  load("data_fmt/alabama_census.rda")
  load("data_fmt/df_alabama_climate.rda")
  df_termite <- read.csv("FSTrecords.csv")
  
  df_pop <- al_pop |> 
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
      ) |>
    st_drop_geometry() %>%
    select(-centroid, -Total_pop) 
  
  df_pop_stat <- df_pop %>%
    pivot_wider(
      names_from = year,
      values_from = density,
      names_prefix = "popdens_"
    ) %>%
    mutate(
      popdens_change = popdens_2024 - popdens_1980
    ) %>%
    select(NAME, area_sq_km, lat, popdens_1980, popdens_2024, popdens_change)
  
  df_road <- road_sensitivity_data |> map_dfr(~ .x$df_road)
  df_rail <- rail_sensitivity_data |> map_dfr(~ .x$df_rail, .id = "year") |>
    mutate(year = as.numeric(year))
  df_traffic <- df_road |> left_join(df_rail, by = c("NAME", "year")) 
  df_traffic <- df_traffic |> 
    left_join(df_pop |> filter(year == 2020) |> select(area_sq_km, NAME), by = "NAME") |>
    mutate(rail_density = rail_total_length_km  / area_sq_km,
           IS_presence = total_IS_km > 0,
           road_density = total_road_km / area_sq_km
           ) |>
    select(NAME, year, rail_density, IS_presence, road_density)
  df_traffic <- df_traffic |>
    mutate(rail_density = ifelse(is.na(rail_density),
                                 0, rail_density))
   
  df_traffic_stat <- df_traffic %>%
    pivot_wider(
      names_from = year,
      values_from = c(rail_density, IS_presence, road_density)
    ) %>%
    select(NAME, rail_density_2011, rail_density_2024,
           IS_presence_2011, IS_presence_2024,
           road_density_2011, road_density_2024,)
  
  df_alabama_climate_stat <- df_alabama_climate |>
    group_by(NAME) |>
    summarise(
      mean_temp_mean = mean(temp_mean_C),
      mean_temp_min  = mean(temp_min_C),
      .groups = "drop"
    )
  
  #
  {
    df_pop_plot <- df_pop |> left_join(df_termite, by = join_by(NAME == "County")) %>%
      mutate(cens = !is.na(FirstDetectedYear),
             across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
             year_till_detect = FirstDetectedYear - 1985) 
    
    ggplot(df_pop_plot, aes(x = year, y = density)) +
      geom_line(aes(col = cens), linewidth = 0.7) +
      geom_vline(aes(xintercept = FirstDetectedYear), linetype = "dashed", color = "darkgray") +
      scale_color_manual(name = "detect", 
                         values = c("FALSE" = "#555555", "TRUE" = "#FF5555")) +
      facet_wrap(~ NAME, ncol = 10) +
      theme_bw(base_size = 9) +
      labs(x = "", y = "Population density") +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      )) + 
      theme(panel.grid = element_blank(),
            aspect.ratio = 3/4,
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 7), 
            axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
            axis.text.y = element_text(size = 6),
            legend.position = c(0.85, 0.03),
            legend.background = element_blank(), 
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8, face = "bold"))
    
    
    df_traffic_plot <- df_traffic |> left_join(df_termite, by = join_by(NAME == "County")) %>%
      mutate(cens = !is.na(FirstDetectedYear),
             across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
             year_till_detect = FirstDetectedYear - 1985) 
    
    library(ggnewscale)
    ggplot(df_traffic_plot, aes(x = year)) +
      geom_line(aes(y = road_density, col = cens), linewidth = 0.7) +
      scale_color_manual(name = "Detect Road", values = c("FALSE" = "#555555", "TRUE" = "#FF5555")) +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      ))+
      new_scale_color() + 
      geom_line(aes(y = rail_density, col = cens), linewidth = 0.7, linetype = 4) +
      scale_color_manual(name = "Detect Rail", values = c("FALSE" = "#555555", "TRUE" = "#5555FF")) +
      geom_vline(aes(xintercept = FirstDetectedYear), linetype = "dashed", color = "darkgray") +
      facet_wrap(~ NAME, ncol = 10) +
      theme_bw(base_size = 9) +
      labs(x = "", y = "Rord density (km / km2)") +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      )) +
      theme(panel.grid = element_blank(),
            aspect.ratio = 3/4,
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 7), 
            axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
            axis.text.y = element_text(size = 6),
            legend.position = c(0.85, 0.03),
            legend.background = element_blank(), 
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8, face = "bold"))
    
    
    ggplot(df_traffic_plot, aes(x = year)) +
      geom_path(aes(y = IS_presence * 1, col = cens), linewidth = 0.7) +
      scale_color_manual(name = "Detect", values = c("FALSE" = "#555555", "TRUE" = "#FF5555")) +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      ))+
      scale_y_continuous(breaks = c(0,1), limits = c(-0.05, 1.05)) +
      geom_vline(aes(xintercept = FirstDetectedYear), linetype = "dashed", color = "darkgray") +
      facet_wrap(~ NAME, ncol = 10) +
      theme_bw(base_size = 9) +
      labs(x = "", y = "IS presence") +
      theme(panel.grid = element_blank(),
            aspect.ratio = 3/4,
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 7), 
            axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
            axis.text.y = element_text(size = 6),
            legend.position = c(0.85, 0.03),
            legend.background = element_blank(), 
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8, face = "bold"))
    
    
    df_alabama_climate_plot <- df_alabama_climate |> left_join(df_termite, by = join_by(NAME == "County")) %>%
      mutate(cens = !is.na(FirstDetectedYear),
             across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
             year_till_detect = FirstDetectedYear - 1985) 
    
    ggplot(df_alabama_climate_plot, aes(x = year)) +
      geom_path(aes(y = temp_mean_C * 1, col = cens), linewidth = 0.7) +
      scale_color_manual(name = "Detect", values = c("FALSE" = "#555555", "TRUE" = "#FF5555")) +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      ))+
      geom_vline(aes(xintercept = FirstDetectedYear), linetype = "dashed", color = "darkgray") +
      new_scale_color() + 
      geom_line(aes(y = temp_min_C, col = cens), linewidth = 0.7) +
      scale_color_manual(name = "Detect", values = c("FALSE" = "#555555", "TRUE" = "#5555FF")) +
      guides(color = guide_legend(
        direction = "horizontal", 
        title.position = "left",
        title.vjust = 0.5   
      ))+
      facet_wrap(~ NAME, ncol = 10) +
      theme_bw(base_size = 9) +
      labs(x = "", y = "IS presence") +
      theme(panel.grid = element_blank(),
            aspect.ratio = 3/4,
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = 7), 
            axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
            axis.text.y = element_text(size = 6),
            legend.position = c(0.85, 0.03),
            legend.background = element_blank(), 
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 8, face = "bold"))
    }
  
  df_stat <- df_alabama_climate_stat |> left_join(df_traffic_stat, by = "NAME") |>
    left_join(df_pop_stat |> select(-area_sq_km), by = "NAME") |>
    rename(county = NAME) |>
    left_join(df_termite, by = c("county" = "County")) |>
    mutate(cens = !is.na(FirstDetectedYear),
           across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
           year_till_detect = FirstDetectedYear - 1985) 
    
}


# spatial autocorrelation
library(spdep)

coords <- df_pop |> filter(year == 2020) |> select(lon, lat)
nb <- knn2nb(knearneigh(coords, k = 5))
lw <- nb2listw(nb, style = "W")

df_sp <- df_pop |> filter(year == 2020) |> left_join(df_termite, 
                                                     by = join_by(NAME == "County")) %>%
  mutate(cens = !is.na(FirstDetectedYear),
         across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
         year_till_detect = FirstDetectedYear - 1985) 

moran.test(df_sp$density, lw)



# survival analysis
{
  
  df_surv <- df_stat %>%
    mutate(
      event = as.numeric(cens),
      across(
        where(is.numeric) & !c(FirstDetectedYear, year_till_detect, event),
        ~ as.numeric(scale(.x))
      )
    ) |>
    select(-event, -FirstDetectedYear)
  
  
  # correlation
  df_mat <- df_surv %>% select(-county, -cens)
  cor_mat <- cor(df_mat, method = "spearman", use = "complete.obs")
  
  cor_df <- as.data.frame(cor_mat) %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "correlation") %>%
    filter(!is.na(correlation)) 
  
  ggplot(cor_df, aes(x = Var2, y = Var1, fill = correlation)) +
    geom_tile(color = "white") +
    scale_fill_distiller(
      palette = "RdBu", 
      limit = c(-1, 1), 
      direction = 1,
      name = "Spearman\nCorrelation"
    ) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
      axis.text.y  = element_text(color = "black"),
      axis.title   = element_blank(),
      panel.grid   = element_blank(),
      aspect.ratio = 1
    )
  
  # surv
  colnames(df_surv)
  
  # entire variable (copy and paste purpose)
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      mean_temp_mean +
      mean_temp_min +
      rail_density_2011 +
      rail_density_2024 +
      IS_presence_2011 +
      IS_presence_2024 +
      road_density_2011 +
      road_density_2024 +
      lat +
      popdens_1980 +
      popdens_2024 +
      popdens_change,
    data = df_surv
  )

  # original model
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2024 +
      road_density_2024 +
      lat +
      popdens_2024,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use 2011 traffic
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2011 +
      road_density_2011 +
      lat +
      popdens_2024,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use newer rail (there was a change of rail category in ~2017)
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2024 +
      IS_presence_2024 +
      road_density_2024 +
      lat +
      popdens_2024,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use previous pop density
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2024 +
      road_density_2024 +
      lat +
      popdens_1980,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use pop density change as well
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2024 +
      road_density_2024 +
      lat +
      popdens_2024 +
      popdens_change,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use average temperature instead of latitude
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2024 +
      road_density_2024 +
      mean_temp_mean +
      popdens_2024,
    data = df_surv
  )
  Anova(cox_mod)
  
  # use min temperature instead of latitude
  cox_mod <- coxph(
    Surv(year_till_detect, cens) ~ 
      rail_density_2011 +
      IS_presence_2024 +
      road_density_2024 +
      mean_temp_min +
      popdens_2024,
    data = df_surv
  )
  Anova(cox_mod)
  
  
  
  summary(cox_mod)
  Anova(cox_mod)

  ph_test <- cox.zph(cox_mod)
  ph_test


  cox_df <- summary(cox_mod)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    mutate(
      HR = exp(coef),
      lower = exp(coef - 1.96 * `se(coef)`),
      upper = exp(coef + 1.96 * `se(coef)`)
    )

  cox_df$term <- factor(
    cox_df$term,
    levels = rev(c("lat_s",    "popdens_s",          
                   "raildens_s",   "roadden_s", "IS_presenceTRUE")),
    labels = rev(c("Latitude", "Population density", 
                   "Rail density", "Road density", "Interstate presence"
    ))
  )

  ggplot(cox_df, aes(x = HR, y = term)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    labs(
      x = "Hazard ratio (log scale)",
      y = NULL
    ) +
    theme_classic() +
    theme(
      panel.grid.minor = element_blank(),
      aspect.ratio = 1
    )

  ggsave("output/cox_hazard.pdf", , 
         device = cairo_pdf, family = "Arial",
         width = 4, height = 4)
  
  
  ## prediction
  risk_score_pred <- predict(cox_mod, type = "lp")
  hazard_ratio_pred <- exp(risk_score_pred)
  
  risk_map <- al_map_data %>% mutate(
    hazard_ratio = hazard_ratio_pred,
    risk_score =risk_score_pred,
    risk_wo_infect = if_else(is.na(FirstDetectedYear), risk_score, NA),
    hazard_wo_infect = if_else(is.na(FirstDetectedYear), hazard_ratio, NA))
  
  
  p_hazard <- ggplot(risk_map) +
    geom_sf(aes(fill = risk_wo_infect), color = "white", size = 0.2) +
    scale_fill_viridis(name = "Risk score", option = "inferno", direction = -1) +
    theme_void()
  p_hazard
  ggsave("output/AL_FST_risk.pdf", , 
         device = cairo_pdf, family = "Arial",
         width = 4, height = 4)
  
  sf <- survfit(cox_mod, newdata = df_surv)
  detect_prob_2025 <- summary(sf, times = 40)
  
  detect_prob_2025$newdata
  as.vector(detect_prob_2025$surv)
  
  risk_map %>% 
    mutate(prop_2025 = 1-as.vector(detect_prob_2025$surv)) %>%
    arrange(-risk_wo_infect) %>%
    select(NAME, risk_wo_infect, hazard_wo_infect, prop_2025)
  
  
  
  ggplot(risk_map) +
    geom_sf(fill = "gray", color = "white", size = 0.2) +
    theme_void()
}


key_vars <- c(
  "IS_presence_2011TRUE",
  "IS_presence_2024TRUE",
  "lat",
  "mean_temp_mean",
  "mean_temp_min",
  "popdens_2024",
  "popdens_1980",
  "popdens_change",
  "rail_density_2011",
  "rail_density_2024",
  "road_density_2024",
  "road_density_2011"
)
