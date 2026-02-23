library(tidyverse)
library(ratdat)

complete_old |>
  group_by(species_id, plot_type) |>
  summarize(hindfoot_mean = mean(hindfoot_length, na.rm = TRUE),
            hindfoot_max = max(hindfoot_length, na.rm = TRUE),
            count = n(),
            .by = c("species", "plot_type"))

my_var <- "test_var"

my_vector <- c(2,6,12,24,32)
my_vector[3]
my_vector[c(1,5)]


my_matrix <- matrix(data = 100, nrow = 3, ncol = 4)

my_matrix[2,3] <- my_matrix[2,3] / 10
my_matrix[ ,3]


my_array <- array(100, dim = c(2,3,4))
my_array[2, 1, 3] <- my_array[2, 1, 3]/10
