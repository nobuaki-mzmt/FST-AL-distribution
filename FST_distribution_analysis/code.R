library(sf)         # spatial data
library(tigris)     # US Census shapefiles
library(dplyr)
library(stringr)
library(ggplot2)

options(tigris_use_cache = TRUE)
al_counties <- counties(
  state = "AL", cb = TRUE, year = 2022
)

df <- read.csv("FSTrecords.csv")

al_map_data <- al_counties %>%
  left_join(df, by = c("NAME" = "County"))

p_FST <- ggplot(al_map_data) +
  geom_sf(aes(fill = FirstDetectedYear),
          color = "white",
          size = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "First detection year"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

al_roads <- primary_secondary_roads(state = "AL", year = 2024)
al_interstates <- al_roads %>% filter(RTTYP == "I")

ggplot() +
  geom_sf(data = al_counties, fill = "grey90", color = "white", size = 0.2) +
  geom_sf(data = al_roads, color = "black", size = 0.3) +
  geom_sf(data = al_interstates, color = "red", size = 0.5) +
  theme_void()

p_FST + geom_sf(data = al_interstates, color = "red", size = 0.3)  +
  labs(title = "Interstates (as of 2024) overlayed")
ggsave("output/interstate.pdf", , 
       device = cairo_pdf, family = "Arial",
       width = 4, height = 4)


#
us_rails <- rails(year = 2011)
al_rails <- us_rails %>%
  st_transform(st_crs(al_counties)) %>%
  st_filter(al_counties)
al_rails_main <- al_rails %>%filter(MTFCC == "R1011")


p_FST + geom_sf(data = al_rails_main, color = "blue", size = 0.25) +
  labs(title = "Railroads (as of 2011) overlayed")
ggsave("output/railroad.pdf", , 
       device = cairo_pdf, family = "Arial",
       width = 4, height = 4)


al_rails_main

# population
library(tidycensus)

al_pop <- get_estimates(
  geography = "county",
  product = "population",
  state = "AL",
  year = 2024,
  geometry = TRUE 
) %>%
  mutate(
    NAME = str_remove(NAME, ", Alabama"),
    NAME = str_remove(NAME, " County")
  )


al_pop_plot <- al_pop %>% filter(variable == "POPESTIMATE") %>%
  rename(Population = value)
ggplot() +
  geom_sf(data = al_pop_plot,
          aes(fill = Population), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma) +
  theme_void() +
  labs(title = "County population")


al_geo <- counties(state = "AL", cb = FALSE, year = 2023) %>%
  select(GEOID, ALAND) 

al_density <- al_pop_plot %>%
  left_join(st_drop_geometry(al_geo), by = "GEOID") %>%
  mutate(
    area_sq_km = ALAND / 1000000,
    density = Population / area_sq_km
  )

ggplot() +
  geom_sf(data = al_density, aes(fill = density), color = "white", size = 0.1) +
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



# clean data
df_clean <- al_density %>% 
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
  )


al_rails_main <- st_transform(al_rails_main, 3857) # Using meters-based projection
al_counties <- st_transform(al_counties, 3857)

rails_by_county <- st_intersection(al_rails_main, al_counties)
rails_by_county$rail_length_m <- st_length(rails_by_county)

county_rail_summary <- rails_by_county %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarise(
    rail_length_m = sum(rail_length_m),
    .groups = "drop"
  )
df_clean <- df_clean %>% 
  left_join(county_rail_summary, by = join_by(county == "NAME"))%>%
  mutate(rail_density = rail_length_m / area_sq_km) 

al_interstates <- st_transform(al_interstates, 3857) # Using meters-based projection
al_counties <- st_transform(al_counties, 3857)
IS_by_county <- st_intersection(al_interstates, al_counties)
IS_by_county$IS_length_m <- as.numeric(st_length(IS_by_county))

df_IS <- IS_by_county %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarise(
    total_IS_ms = sum(IS_length_m),
    .groups = "drop"
  )

df_clean <- df_clean %>% 
  left_join(df_IS, by = join_by(county == "NAME"))%>%
  mutate(IS_density = total_IS_ms / area_sq_km,
         IS_presence = !is.na(total_IS_ms))


df_clean <- df_clean %>%
  mutate(across(where(is.numeric), as.numeric)) %>% # Strip [m] or [miles] units
  mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0)))

# dis from mobile
if(F){
  mobile_coords <- df_clean %>% 
    filter(county == "Mobile") %>% 
    select(lon, lat)
  
  df_clean <-  df_clean %>%
    # Temporarily convert to sf to use spatial distance functions
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    mutate(
      dist_from_mobile_m = st_distance(
        geometry, 
        st_sfc(st_point(c(mobile_coords$lon, mobile_coords$lat)), crs = 4326)
      ) %>% as.numeric(),
      dist_from_mobile_km = dist_from_mobile_m / 1000
    ) %>% 
    st_drop_geometry()
}

df_stat <- df_clean %>%
  left_join(df, by = join_by(county == "County")) %>%
  mutate(cens = !is.na(FirstDetectedYear),
         across(FirstDetectedYear, ~ tidyr::replace_na(.x, 2025)),
         year_till_detect = FirstDetectedYear - 1985)

colnames(df_stat)

library(survival)
df_surv <- df_stat %>%
  mutate(
    event = as.numeric(cens),
    lat_s = scale(lat),
    popdens_s = scale(pop_density),
    raildens_s = scale(rail_density)
  )



cox_mod <- coxph(
  Surv(year_till_detect, event) ~ 
    lat_s +
    popdens_s +
    raildens_s +
    IS_presence ,
  data = df_surv
)

summary(cox_mod)

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
  levels = c("lat_s", "popdens_s", "raildens_s", "IS_presenceTRUE", "dist_mobile_s"),
  labels = c(
    "Latitude",
    "Population density",
    "Rail density",
    "Interstate presence",
    "Distance from Mobile"
  )
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

