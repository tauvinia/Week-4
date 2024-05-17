library("readr")
library("dplyr")
library("sf")
library("ggplot2")

wildschwein <- read_delim("wildschwein_BE_2056.csv", ",")

# Careful! What Timezone is assumed?
sabi <- wildschwein |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE) |>
  filter(TierName == "Sabi", DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")

ggplot(sabi, aes(E,N, color = DatetimeUTC)) +
  geom_point()+
  geom_path()+
  coord_fixed()+
  scale_color_datetime(low= "blue", high = "red")+
  guides(color = guide_colorbar (title.position =  "top", title.hjust = .5, barwidth = unit(20, "lines"), barheight = unit (.5, "lines")))+
  theme (legend.position = "bottom")+
    geom_point(y= 1205120, x=2570470, size=20, pch = 21, color = "black", stroke =4)

#Step a/b
  
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

sabi <- sabi |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )
sabi <- sabi |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

sabi

sabi <- sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

sabi_filter <- sabi |>
  filter(!static)
library(ggplot2)

sabi_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")



