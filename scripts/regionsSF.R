library(tigris)
library(sf)
library(dplyr)
library(ggplot2)

states_sf <- states(cb = TRUE)
state_to_region <- c(
  "Connecticut" = "Northeast", "Maine" = "Northeast", "Massachusetts" = "Northeast",
  "New Hampshire" = "Northeast", "Rhode Island" = "Northeast", "Vermont" = "Northeast",
  "New Jersey" = "Northeast", "New York" = "Northeast", "Pennsylvania" = "Northeast",
  "Illinois" = "Midwest", "Indiana" = "Midwest", "Michigan" = "Midwest",
  "Ohio" = "Midwest", "Wisconsin" = "Midwest", "Iowa" = "Midwest", "Kansas" = "Midwest",
  "Minnesota" = "Midwest", "Missouri" = "Midwest", "Nebraska" = "Midwest", "North Dakota" = "Midwest",
  "South Dakota" = "Midwest", "Florida" = "South", "Georgia" = "South", "North Carolina" = "South",
  "South Carolina" = "South", "Virginia" = "South", "Washington, D.C." = "South",
  "Maryland" = "South", "Delaware" = "South", "West Virginia" = "South",
  "Alabama" = "South", "Kentucky" = "South", "Mississippi" = "South", "Tennessee" = "South",
  "Arkansas" = "South", "Louisiana" = "South", "Oklahoma" = "South", "Texas" = "South",
  "Arizona" = "West", "Colorado" = "West", "Idaho" = "West", "Montana" = "West", "Nevada" = "West",
  "New Mexico" = "West", "Utah" = "West", "Wyoming" = "West", "Alaska" = "West",
  "California" = "West", "Hawaii" = "West", "Oregon" = "West", "Washington" = "West"
)

states_sf$region <- state_to_region[states_sf$NAME]

non_continental_states <- c("Alaska", "Hawaii", "American Samoa", "Guam", "Puerto Rico", "United States Virgin Islands", "Northern Mariana Islands", "Wake Island", "Commonwealth of the Northern Mariana Islands")
states_sf <- states_sf %>%
  filter(!NAME %in% non_continental_states)

region_sf <- states_sf |>
  group_by(region) |>
  summarize()

st_write(region_sf, "dataset_for_shiny/region_shapefile.shp")