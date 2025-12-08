library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(knitr)
library(rnaturalearth)
library(tigris)
library(ggthemes)
library(cartogram)
library(scales)
library(viridis)
#format the data
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')
cheese_data <- cheeses %>%
  separate_rows(country, sep = ", ") %>%
  separate_rows(country, sep = " and ")
country_count <- cheese_data %>%
  group_by(country) %>%
  summarise(cheese_types = n_distinct(cheese)) %>%
  mutate(country = ifelse(country == "United States", "United States of America", country)) %>%
  mutate(country = ifelse(country == "England", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Great Britain", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Wales", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Scotland", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Czech Republic", "Czechia", country)) %>%
  mutate(country = ifelse(country == "Holland", "Netherlands", country)) %>%
  mutate(country = ifelse(country == "Macedonia", "North Macedonia", country)) %>%
  mutate(country = ifelse(country == "Serbia", "Republic of Serbia", country)) %>%
  mutate(country = ifelse(country == "Tibet", "China", country)) %>%
  rename(sovereignt = country)
borders <- ne_countries(scale = "small",
                        returnclass = "sf")
missing_countries <- setdiff(unique(country_count$sovereignt), unique(borders$sovereignt))
# Check if there are any missing countries
if (length(missing_countries) == 0) {
  print("All countries in the cheese dataframe are present in the borders dataframe.")
} else {
  print("The following countries are missing from the borders dataframe:")
  print(missing_countries)
}
#what countries are listed in the rnaturalearth package
borders %>%
  arrange(sovereignt) %>%
  pull(sovereignt) %>%
  print()
#now that countries are aligned...
combine <- left_join(borders, country_count, by = "sovereignt")
combine <- combine %>%
  mutate(cheese_types = replace_na(cheese_types, 0))
#this works
n_vals <- length(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 21, 41, 81, 101, 151, 201))
color_scale <- c("gray70", viridis(n_vals-1))
combine %>%
  ggplot() +
  geom_sf(aes(fill = cheese_types)) +
  scale_fill_viridis_b(
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 21, 41, 81, 101, 151, 201),
    labels = c("No Data Collected", "1", "2", "3", "4", "5", "6", "7", "8", "9",
               "10-20", "21-40", "41-80", "81-100", "101-150", "151-200", "<200")
  ) +
  labs(
    title = "Cheese Produced Around the World"
  )
#help figure out where to draw breaks by printing counts
combine %>%
  arrange(cheese_types) %>%
  pull(cheese_types) %>%
  print()
#ATTEMPT TO LAYER MAPS
#create us states
us_states <- tigris::regions(resolution = "5m")
us_states %>%
  ggplot() +
  geom_sf()
#create color scale
# number of legend categories
n_vals <- length(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 21, 41, 81, 101, 151, 201))
# first color gray, rest viridis
my_cols <- c("gray95", turbo(n_vals - 1, direction = -1))
combine %>%
  ggplot() +
  geom_sf(aes(fill = cheese_types)) +
  scale_fill_gradientn(
    colors = my_cols,
    breaks = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 301),
    labels = c(
      "No Data Collected", "1-3", "4-6", "7-10", "11-15", "16-25", "26-40", "41-60", "61-90", "91-150",
      "151-200", "201-250", "251-300", "<301"
    )
  ) +
  labs(title = "Cheese Produced Around the World",
       fill = "Different Types of Cheese Reported") +
  theme(
    legend.spacing.y = unit(1, 'cm')
  )
#this map does not have gray for no data collected countries. need to convert cheese_types to a factor to do this/not have a continuous set of values
cheese_map <- combine %>%
  ggplot() +
  geom_sf(aes(fill = cheese_types)) +
  scale_fill_binned(
    type = "viridis",
    breaks = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 301),
    labels = c(
      "No Data Collected", "1-3", "4-6", "7-10", "11-15", "16-25", "26-40", "41-60", "61-90", "91-150",
      "151-200", "201-250", "251-300", "<301"
    )
  ) +
  labs(title = "Cheese Produced Around the World",
       fill = "Different Types of Cheese Reported") +
  theme(
    legend.spacing.y = unit(4, 'cm')
  )
#adding in leaflet
# the ~ indicates that the following function should be performed in regards to the dataset provided to leaflet... where to pull columns from
cheese_map <- leaflet(data = combine) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorBin("viridis", cheese_types, bins = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 320))(cheese_types),
    fillOpacity = 0.7,
    color = "gray70",
    weight = 1,
    label = ~paste0(sovereignt, " ", "Cheese Types: ", cheese_types, "Most Common Milk Origin")
  ) %>%
  addLegend(
    pal = colorBin("viridis", domain = combine$cheese_types, bins = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 301)),
    values = ~cheese_types,
    title = "Different Types of Cheese Reported",
    labels = c(
      "No Data Collected", "1-3", "4-6", "7-10", "11-15", "16-25", "26-40", "41-60", "61-90", "91-150",
      "151-200", "201-250", "251-300", "<301"
    ),
    opacity = 0.7
  )
# Print the map
cheese_map



#check the CRS for the weird borders
#fix the legend overlap
#can try continuous instead of binning for color gradient 


