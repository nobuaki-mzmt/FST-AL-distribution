# processing.R
# generate formatted data for output.R

source("source.R")

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
          total_road_km_wIS = sum(length_km, na.rm = TRUE),
          total_road_km_woIS = sum(length_km[RTTYP != "I"], na.rm = TRUE),
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
    
    al_pop <- al_pop |> 
      mutate(
        centroid = st_centroid(geometry),
        lon = st_coordinates(centroid)[, 1],
        lat = st_coordinates(centroid)[, 2]
      ) |>
      select(-centroid, -Total_pop) 
    
  }
  
  # traffic data summary
  {
    df_pop <- al_pop |> st_drop_geometry() 
    df_road <- road_sensitivity_data |> map_dfr(~ .x$df_road)
    df_rail <- rail_sensitivity_data |> map_dfr(~ .x$df_rail, .id = "year") |>
      mutate(year = as.numeric(year))
    df_traffic <- df_road |> left_join(df_rail, by = c("NAME", "year")) 
    df_traffic <- df_traffic |> 
      left_join(df_pop |> filter(year == 2020) |> select(area_sq_km, NAME), by = "NAME") |>
      mutate(rail_density      = rail_total_length_km  / area_sq_km,
             IS_presence       = total_IS_km > 0,
             road_density_woIS = total_road_km_woIS / area_sq_km,
             road_density_wIS  = total_road_km_wIS / area_sq_km
      ) |>
      select(NAME, year, rail_density, IS_presence, road_density_woIS, road_density_wIS)
    df_traffic <- df_traffic |>
      mutate(rail_density = ifelse(is.na(rail_density), 0, rail_density))
  }
  
  # traffic data 2000
  {
    fips_codes <- sprintf("%05d", seq(1001, 1133, by = 2))
    dir.create("data_2000", showWarnings = FALSE)
    
    results <- map_dfr(fips_codes, function(fips) {
      message(fips)
      zipfile <- file.path("data_2000", paste0(fips, ".zip"))
      
      if(!file.exists(zipfile)){
        url <- paste0("https://www2.census.gov/geo/tiger/tiger2k/al/tgr", fips, ".zip")
        download.file(url, zipfile, mode = "wb", quiet = TRUE)
      }
      outdir <- sub("\\.zip$", "", zipfile)
      if(!dir.exists(outdir)){
        unzip(zipfile, exdir = outdir)
      }
      rt1_file <- list.files(
        outdir,
        pattern = "\\.RT1$",
        full.names = TRUE
      )
      rt1 <- readLines(rt1_file)
      cfcc <- substr(rt1, 56, 58)
      tibble(
        FIPS = fips,
        A1 = sum(grepl("^A1", cfcc)),
        A2 = sum(grepl("^A2", cfcc)),
        A3 = sum(grepl("^A3", cfcc)),
        A4 = sum(grepl("^A4", cfcc)),
        A5 = sum(grepl("^A5", cfcc)),
        B1 = sum(grepl("^B1", cfcc)),
        B2 = sum(grepl("^B2", cfcc))
      )
    })
    
    al_counties <- counties(state = "AL", cb = TRUE, year = 2024)
    al_counties <- al_counties |> select(COUNTYFP, NAME) |> st_drop_geometry()
    
    df_traffic_2000 <- results |> mutate(FIPS = str_sub(FIPS, 3,5)) |> 
      left_join(al_counties, by = join_by("FIPS" == "COUNTYFP")) |>
      mutate(n_road_2000 = A1 + A2 + A3 +A4 + A5,
             n_rail_2000 = B1 + B2,
             Highway_presence_2000 = A1 > 0) |>
      select(-starts_with("A"), -starts_with("B"), -FIPS)
  }
  
  save(al_map_data, road_sensitivity_data, rail_sensitivity_data, df_traffic, 
       df_traffic_2000, al_pop, file = "data_fmt/alabama_census.rda")
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

# summary data ----
{
  load("data_fmt/alabama_census.rda")
  load("data_fmt/df_alabama_climate.rda")
  
  df_termite <- read.csv("FSTrecords.csv")
  
  df_pop_stat <-  al_pop |> st_drop_geometry()  %>%
    pivot_wider(
      names_from = year,
      values_from = density,
      names_prefix = "popdens_"
    ) %>%
    mutate(
      popdens_change = popdens_2024 - popdens_1980
    ) %>%
    select(NAME, area_sq_km, lat, popdens_1980, popdens_2024, popdens_change)
  
  df_traffic_stat <- df_traffic %>%
    pivot_wider(
      names_from = year,
      values_from = c(rail_density, IS_presence, road_density_wIS, road_density_woIS)
    ) %>%
    select(NAME, rail_density_2011, rail_density_2024,
           IS_presence_2011, IS_presence_2024,
           road_density_wIS_2011, road_density_wIS_2024,
           road_density_woIS_2011, road_density_woIS_2024)
  
  df_alabama_climate_stat <- df_alabama_climate |>
    group_by(NAME) |>
    summarise(
      mean_temp_mean = mean(temp_mean_C),
      mean_temp_min  = mean(temp_min_C),
      .groups = "drop"
    )
  
  df_stat <- df_alabama_climate_stat |> left_join(df_traffic_stat, by = "NAME") |>
    left_join(df_pop_stat |> select(-area_sq_km), by = "NAME") |>
    rename(county = NAME) |>
    left_join(df_termite, by = c("county" = "County")) |>
    mutate(cens = !is.na(FirstDetectedYear),
           across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
           year_till_detect = FirstDetectedYear - 1985) 
  
  df_stat <- df_stat |> left_join(df_traffic_2000, by = join_by("county" == "NAME"))
  
  df_surv <- df_stat %>%
    mutate(
      event = as.numeric(cens),
      across(
        where(is.numeric) & !c(FirstDetectedYear, year_till_detect, event),
        ~ as.numeric(scale(.x))
      )
    ) |>
    select(-event, -FirstDetectedYear)
  
  save(df_surv, file = "data_fmt/df_surv.rda")
}
