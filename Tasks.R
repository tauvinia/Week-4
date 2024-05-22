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
  mutate(static = stepMean < 5, na.rm = TRUE)
         #< mean(stepMean, na.rm = TRUE))

# Visualize segmented trajectories

library("ggplot2")
ggplot(data_2056, aes(x = longitude, y = latitude, colour = static)) +
  geom_point()+
  geom_path()+
  coord_equal()+
  theme(legend.position = "bottom")

#Segment-based analysis

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

data_2056 <- data_2056 |>
  mutate(segment_id = rle_id(static)) |>
  filter(static == FALSE)


#You can use the newly created function rle_id to assign unique IDs to subtrajectories 
#(as shown below). Visualize the moving segments by colourizing them by segment_ID. 
#Then use segment_ID as a grouping variable to determine the segments duration and 
#remove short segments (e.g. segments with a duration < 5 Minutes)
#mutate, filter out with stepmean


ggplot(data_2056, aes(x = longitude, y = latitude, colour = segment_id)) +
  geom_point()+
  geom_path()+
  coord_equal()+
  theme(legend.position = "none")


#Similarity measures
#explore the trajectories first and get an idea on how the pedestrians moved.
pedestrian <- read_delim("pedestrian.csv", ",")

library(patchwork)
pedestrian$TrajID <- as.factor(pedestrian$TrajID)

colors <- c(
  "1" = "#F8766D",  # Red (TrajID: 1)
  "2" = "#C49A00",  # Dark Yellow (TrajID: 2)
  "3" = "#53B400",  # Green (TrajID: 3)
  "4" = "#00C094",  # Cyan (TrajID: 4)
  "5" = "#00B6EB",  # Blue (TrajID: 5)
  "6" = "#FB61D7"   # Pink (TrajID: 6)
)
              

# Aggregate data for combined points
combined_data <- pedestrian %>%
  group_by(E, N) %>%
  summarise_all(mean)

ggplot(pedestrian, aes(E, N, color = TrajID)) +
  geom_point() +
  geom_point(data = combined_data, aes(E, N), color = "lightgray", size = 0.5) +
  geom_path() +
  ggtitle("Visual comparison of the 6 trajectories", subtitle = "Each subplot highlights a trajectory") +
  facet_wrap(~ TrajID, labeller = labeller(TrajID = function(x) paste("TrajID:", x)), scales = "free") +
  scale_color_manual(values = colors) +
  theme_minimal()

#I didnt manage to put the combined data on the same dataset. Could you share the solution?
 

#Calculate similarity

# I honestly tried for hours but I cannot get what is wrong with this code.
# I tried checking the help, asking chat gpt, googling and partly asking colleagues
# I hope it is still fine even if it doesnt work.
# Could you, please, share the correct code so that I can correct the exercise and use it for the project?



install.packages("SimilarityMeasures")
help(package = "SimilarityMeasures")

library(SimilarityMeasures) 

# Suppose you have functions to compute similarity measures for trajectories
compute_dtw <- function(traj1, traj2) {
  # Function to compute DTW distance between trajectories
}

compute_edit_distance <- function(traj1, traj2) {
  # Function to compute edit distance between trajectories
}

compute_frechet_distance <- function(traj1, traj2) {
  # Function to compute Frechet distance between trajectories
}

compute_lcss <- function(traj1, traj2) {
  # Function to compute LCSS similarity between trajectories
}

# Extract trajectories for TrajID 1 and other TrajIDs
#traj_1 <- subset(pedestrian, TrajID == 1)
#traj_others <- subset(pedestrian, TrajID != 1)


traj_1 <- pedestrian |> 
  filter(TrajID == 1) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

traj_2 <- pedestrian |> 
  filter(TrajID == 2) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

traj_3 <- pedestrian |> 
  filter(TrajID == 3) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

traj_4 <- pedestrian |> 
  filter(TrajID == 4) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

traj_5 <- pedestrian |> 
  filter(TrajID == 5) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

traj_6 <- pedestrian |> 
  filter(TrajID == 6) |> 
  subset(select = c(E,N)) |> 
  as.matrix()


other_trajectories <- pedestrian |> 
  filter(TrajID != 1) |> 
  subset(select = c(E,N)) |> 
  as.matrix()

DTW(traj_1, traj_2, traj_3, traj_4, traj_5, traj_6)

EditDist(traj_1, other_trajectories)



# Load required packages
library(SimilarityMeasures)
install.packages("TraMineR")
install.packages("dtw")
library(dtw) 
library(TraMineR)  # For Frechet distance

# Check dimensions of traj_1 and other_trajectories
print(dim(traj_1))
print(dim(other_trajectories))

# Convert traj_1 to a matrix if it's not already
if (!is.matrix(traj_1)) {
  traj_1 <- matrix(traj_1, ncol = length(traj_1))
}

# Convert other_trajectories to a list of matrices if it's not already
if (!is.list(other_trajectories)) {
  other_trajectories <- lapply(other_trajectories, function(traj) matrix(traj, ncol = length(traj)))
}

# Compute similarity measures for TrajID 1 with other trajectories
similarity_measures <- data.frame(
  DTW = sapply(other_trajectories, function(traj) dtw(traj_1, traj)$distance),
  EditDist = sapply(other_trajectories, function(traj) stringdist::stringdist(traj_1, as.character(traj), method = "lv")),
  Frechet = sapply(other_trajectories, function(traj) frechetdist(as.character(traj_1), as.character(traj))),
  LCSS = sapply(other_trajectories, function(traj) lcss(as.character(traj_1), as.character(traj))$length)
)

# Create plot to visualize similarity measures
library(ggplot2)
library(tidyr)

similarity_measures_plot <- similarity_measures %>%
  pivot_longer(cols = TrajID, names_to = "Similarity Measure", values_to = "Value")

ggplot(similarity_measures_plot, aes(x = factor(seq_along(Value)), y = Value, fill = `Similarity Measure`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Trajectory Index", y = "Similarity Measure Value") +
  ggtitle("Comparison of TrajID 1 with Other Trajectories") +
  scale_fill_manual(values = c("DTW" = "blue", "EditDist" = "green", "Frechet" = "red", "LCSS" = "orange")) +
  theme_minimal()
