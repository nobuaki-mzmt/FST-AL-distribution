# library
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
  
  library(survival)
  library(car)
}

# FST data
{
  df <- read.csv("FSTrecords.csv")
  ggplot(df, aes(x = FirstDetectedYear))+
    geom_bar()
  
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
  
  
  analysis_df <- df %>%
    group_by(FirstDetectedYear) %>%
    summarise(new_detections = n()) %>%
    tidyr::complete(FirstDetectedYear = min(FirstDetectedYear):max(FirstDetectedYear), fill = list(new_detections = 0)) %>%
    mutate(time_index = FirstDetectedYear - min(FirstDetectedYear)) 
  
  model <- glm(new_detections ~ time_index, family = poisson, data = analysis_df)
  
  summary(model)
  
  # 3. Visualizing the "Intensity" Trend
  ggplot(analysis_df, aes(x = FirstDetectedYear, y = new_detections)) +
    geom_point(color = "#E65100", alpha = 0.6) +
    geom_smooth(method = "glm", method.args = list(family = "poisson"), 
                color = "#E65100", fill = "#FFF176") +
    theme_minimal() +
    labs(title = "Detection Intensity Modeling",
         subtitle = "Poisson regression showing the change in detection rate",
         y = "Number of New Counties", x = "Year")
}

# Alabama census data
{
  options(tigris_use_cache = TRUE)
  al_counties <- counties(state = "AL", cb = TRUE, year = 2024)
  al_counties <- st_transform(al_counties, 3857)
  al_map_data <- al_counties %>% left_join(df, by = c("NAME" = "County"))
  
  # road data
  al_roads <- primary_secondary_roads(state = "AL", year = 2024)
  al_roads <- st_transform(al_roads, 3857) # Using meters-based projection
  al_interstates <- al_roads %>% filter(RTTYP == "I")
  
  IS_by_county <- st_intersection(al_interstates, al_counties) %>%
    st_collection_extract("LINESTRING")
  IS_by_county$IS_length_km <- as.numeric(st_length(IS_by_county))/1000
  
  df_IS <- IS_by_county %>% st_drop_geometry() %>% group_by(NAME) %>%
    summarise(
      total_IS_km = sum(IS_length_km),
      .groups = "drop"
    )
  
  road_by_county <- st_intersection(al_roads, al_counties) %>%
    st_collection_extract("LINESTRING")
  road_by_county$road_length_km <- as.numeric(st_length(road_by_county))/1000
  
  df_road <- road_by_county %>% st_drop_geometry() %>% group_by(NAME) %>%
    summarise(
      total_road_km = sum(road_length_km),
      .groups = "drop"
    )
  
  # rail data
  us_rails <- rails(year = 2011)
  al_rails <- us_rails %>%
    st_transform(st_crs(al_counties)) %>%
    st_filter(al_counties)
  al_rails_main <- al_rails %>% filter(MTFCC == "R1011")
  
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
  
  # population
  al_pop <- get_estimates(
    geography = "county",
    product = "population",
    state = "AL",
    year = 2024,
    geometry = TRUE) %>%
    mutate(
      NAME = str_remove(NAME, ", Alabama"),
      NAME = str_remove(NAME, " County")
    ) %>% filter(variable == "POPESTIMATE") %>%
    rename(Population = value)
  
  
  al_geo <- counties(state = "AL", cb = FALSE, year = 2024) %>%
    select(GEOID, ALAND) 
  
  al_pop <- al_pop %>%
    left_join(st_drop_geometry(al_geo), by = "GEOID") %>%
    mutate(
      area_sq_km = ALAND / 1000000,
      density = Population / area_sq_km
    ) %>%
    select(NAME, Population, area_sq_km, density)
   
  df_pop <- al_pop %>% st_drop_geometry()
  
  
  # plot maps
  p_FST <- ggplot(al_map_data) +
    geom_sf(aes(fill = FirstDetectedYear), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "viridis", name = "First detection year") +
    theme_void()
  ggsave(plot = p_FST, filename = "output/FST_map.pdf", 
         device = cairo_pdf, family = "Arial",
         width = 3, height = 3)
  
  ggplot() +
    geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
    rasterise(geom_sf(data = al_roads, aes(color = "Secondary Roads"), size = 0.3), dpi = 300) +
    geom_sf(data = al_interstates, aes(color = "Interstates"), size = 0.5) +
    scale_color_manual(
      name = "Road",
      values = c("Secondary Roads" = "#333333", "Interstates" = "#EE6677")
    ) +
    theme_void()
  ggsave(filename = "output/road_map.pdf", 
         device = cairo_pdf, family = "Arial",
         width = 3, height = 3)
  
  
  
  ggplot() +
    geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
    geom_sf(data = al_rails_main, color = "#4477AA", size = 0.4) +
    theme_void()
  ggsave(filename = "output/rail_map.pdf", 
         device = cairo_pdf, family = "Arial",
         width = 3, height = 3)
  
  p_FST + geom_sf(data = al_interstates, color = "red", size = 0.3)  +
    labs(title = "Interstates (as of 2024) overlayed")
  ggsave("output/FST_interstate.pdf", 
         device = cairo_pdf, family = "Arial",
         width = 4, height = 4)
  
  p_FST + geom_sf(data = al_rails_main, color = "blue", size = 0.25) +
    labs(title = "Railroads (as of 2011) overlayed")
  ggsave("output/FST_railroad.pdf", , 
         device = cairo_pdf, family = "Arial",
         width = 4, height = 4)
  
  ggplot() +
    geom_sf(data = al_pop, aes(fill = density), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      option = "magma", 
      labels = scales::comma,
      name = "People per\nsq km"
    ) +
    theme_void() +
    labs(title = "Alabama Population Density")
  ggsave("output/AL_population_density.pdf", , 
         device = cairo_pdf, family = "Arial",
         width = 4, height = 4)
}

# clean data
{
  df_clean <- al_pop %>% 
    mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    ) %>%
    st_drop_geometry() %>%
    select(
      county = NAME,
      lat,
      lon,
      population = Population,
      pop_density = density,
      area_sq_km
    ) %>% 
    left_join(df_rail, by = join_by(county == "NAME")) %>%
    mutate(rail_density = rail_total_length_km  / area_sq_km) %>%
    left_join(df_IS, by = join_by(county == "NAME"))%>%
    mutate(IS_density = total_IS_km / area_sq_km,
           IS_presence = !is.na(total_IS_km)) %>%
    left_join(df_road, by = join_by(county == "NAME"))%>%
    mutate(road_density = total_road_km / area_sq_km) 
    
  df_stat <- df_clean %>%
    left_join(df, by = join_by(county == "County")) %>%
    mutate(cens = !is.na(FirstDetectedYear),
           across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
           year_till_detect = FirstDetectedYear - 1985) %>%
    mutate(across(c(total_IS_km, IS_density, num_rail_companies, rail_total_length_km), 
                  ~ tidyr::replace_na(.x, 0)))
 
}

# survival analysis
{
  colnames(df_stat)
  
  df_surv <- df_stat %>%
    mutate(
      event = as.numeric(cens),
      lat_s = scale(lat),
      popdens_s = scale(pop_density),
      raildens_s = scale(rail_density),
      #railcom_s = scale(num_rail_companies),
      #raillen_s = scale(rail_total_length_km),
      ISlen_s = scale(total_IS_km),
      ISden_s = scale(IS_density),
      road_s = scale(total_road_km),
      roadden_s = scale(road_density),
    )
  df_surv[is.na(df_surv)] <- 0
  
  # correlation
  df_mat <- df_surv %>%
    select(lat_s, popdens_s, raildens_s, roadden_s, IS_presence)
  cor_mat <- cor(df_mat, method = "spearman")
  cor_mat
  
  cox_mod <- coxph(
    Surv(year_till_detect, event) ~ 
      lat_s +
      popdens_s +
      raildens_s +
      IS_presence +
      roadden_s,
    data = df_surv
  )

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

