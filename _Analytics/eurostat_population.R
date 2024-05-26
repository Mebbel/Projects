# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# Analyze population data from eurostat
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# Notes ------------------------------------------------------------------------

# TODO: Add copyright notice
# https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units



# Settings ---------------------------------------------------------------------
file_population = "data/raw/eurostat/population/estat_demo_r_pjangrp3.tsv/estat_demo_r_pjangrp3.tsv"



# Libraries ---------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)

library(sf)

library(ggplot2)

# Data ---------------------------------------------------------------------


## Transform population data ---------------------------------------------------
df_pop = read.table(file_population, sep = "\t", header = T)

# Separate first column
df_pop = df_pop %>%
  separate_wider_delim(
    cols = `freq.sex.unit.age.geo.TIME_PERIOD`, 
    delim = ",",
    names = c("freq", "sex", "unit", "age", "geo")
  )

# Correct column types
df_pop = df_pop %>%
  mutate(
    across(starts_with("X"), as.integer)
  ) 

## Prepare geo mapping - NUTS 3 ------------------------------------------------

df_shp_nuts = st_read("data/raw/eurostat/population/NUTS_RG_01M_2021_3035.shp/NUTS_RG_01M_2021_3035.shp")

## Prepare geo mapping - LAU ------------------------------------------------

df_shp_lau = st_read("data/raw/eurostat/population/ref-lau-2021-01m.shp/LAU_RG_01M_2021_3035.shp/LAU_RG_01M_2021_3035.shp")





# Visualize --------------------------------------------------------------------

# Ref: https://r-graphics.org/recipe-miscgraph-map-shapefile
df_shp_lau %>% 
  filter(CNTR_CODE == "FR") %>%
  ggplot(aes(fill = POP_2021)) +
  geom_sf(size = 0.1, color = "gray25")


# , xlim = c(3600000, 3700000), ylim = c(2500000, 3000000)


# Ref: https://bookdown.org/robinlovelace/geocompr/spatial-operations.html#topological-relations

df_shp_nuts = df_shp_nuts %>%
  filter(LEVL_CODE == 3)


current_crs <- st_crs(df_shp_nuts)
print(current_crs)

if (current_crs$epsg != 4326) {
  df_shp_nuts <- st_transform(df_shp_nuts, crs = 4326)
}



bbox <- st_bbox(c(xmin = -10.0, ymin = 20, xmax = 35.0, ymax = 71.0), crs = st_crs(df_shp_nuts))

# Convert the bounding box to an sf object
bbox_sfc <- st_as_sfc(bbox)

# Filter the data using the bounding box
filtered_sf_data <- df_shp_nuts[st_intersects(df_shp_nuts, bbox_sfc, sparse = FALSE), ]

f_coord <- function(x) {

  data.frame(
    xmax = max(st_coordinates(x)[,1]),
    xmin = min(st_coordinates(x)[,1]),
    ymax = max(st_coordinates(x)[,2]),
    ymin = min(st_coordinates(x)[,2])
  )
  
}

# Calculate coordinates
df = filtered_sf_data %>%
  group_by(NUTS_ID) %>% 
  mutate(coord = map_df(geometry, f_coord))





# Plot map of European Union

df %>%
  filter(coord$ymin > 20) %>% 
  # Add population data
  left_join(
    df_pop %>% filter(age == "TOTAL", sex == "T"), by = c("NUTS_ID" = "geo")
  ) %>%
  # filter(CNTR_CODE %in% c("DE", "FR", "IT")) %>%
  ggplot() +
  geom_sf(aes(fill = X2019), color = "gray95", size = 0.0005) +
  scale_fill_gradient(low = "white", high = "#003399", na.value = "white") +
  theme_bw()
  
  # coord_sf(crs = st_crs(4326))
  # coord_sf(crs = st_crs(3347))



# Create density plots! -> surface!
# -> within 100km of a point -> sum of population?!

range = 100 * 1000

df_dist = tibble()


stop("This iteration takes too long! 300 iterations take > 2h!")
# THere must be a smarter way. Do not need to check all possible connection
# -> Can we maybe determine the centers of each unit and estimate the dsitance?
# -> i.e. for each starting unit, determine possible candidates by estimating?
# -> Should reduce the dimensionality of the problem significantly.

for (i in 1:nrow(df)) {
  
  print(paste0(i , " / ", nrow(df)))
  
  unit <- df[i, ]
  other_units <- df[-i,]
  
  for (j in 1:nrow(other_units)) {
    
    dist = st_distance(unit, other_units[j,])
    
    # Check if distance between two points is below range
    if (as.numeric(dist[[1]]) <= range) {
      
      df_dist = bind_rows(
        df_dist, 
        tibble(ID_FROM = unit$NUTS_ID, ID_TO = other_units[j,]$NUTS_ID)
      )
      
      
    }
    
  }

}






df %>%
  filter(coord$ymin > 20) %>% 
  ggplot() +
  geom_sf(color = "gray95", size = 0.0005) +
  geom_sf(data = df %>% filter(NUTS_ID == "HR064"), fill = "red", size = 0.0005) +
  geom_sf(data = df %>% filter(NUTS_ID %in% (df_dist %>% filter(ID_FROM == "HR064") %>% pull(ID_TO))), fill = "green", size = 0.0005) +
  scale_fill_gradient(low = "white", high = "#003399", na.value = "white") +
  theme_bw()



