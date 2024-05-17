#Preparation

library("readr")
library("dplyr")

data <- read_delim("combined_data.csv", ",")
library("sf")

data_sf <- st_as_sf(data,
                    coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE
)|>
  filter(timestamp >= "2024-04-05", timestamp < "2024-04-06")

data_2056 <- data_sf |> st_transform(2056)
st_coordinates(data_2056)

# Segmentation
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

data_2056 <- data_2056 |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )

#Specify and apply threshold d
data_2056 <- data_2056 |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

data_2056 <- data_2056 |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

# Visualize segmented trajectories

library("ggplot2")
ggplot(data_2056, aes(x = longitude, y = latitude, colour = static)) +
  geom_point()+
  geom_path()+
  coord_equal()+
  theme(legend.position = "bottom")

#Segment-based analysis

