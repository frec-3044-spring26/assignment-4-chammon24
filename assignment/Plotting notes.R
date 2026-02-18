library(tidyverse)
library(ratdat)

ggplot(data = complete_old, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.2, color = "blue") + theme_classic()

ggplot(data = complete_old, aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.2) + scale_color_viridis_d() +
  theme_classic()

ggplot(data = complete_old, aes(x = weight, y = hindfoot_length, color = plot_type)) +
  geom_point(alpha = 0.2) + scale_x_log10() + scale_y_log10() +
  theme_classic()

ggplot(data = complete_old, aes(x = plot_type, y = hindfoot_length,
                                 fill = plot_type)) + geom_boxplot() + theme_classic()

ggplot(data = complete_old, aes(x = plot_type, y = hindfoot_length,
                                fill = plot_type)) + geom_boxplot() + theme_classic() +
  scale_x_discrete(labels = label_wrap_gen(width = 10))


ggplot(data = complete_old, aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme(axis.title = element_text(size = 14), legend.position = "none") +
  labs(title = "Rodent Size by Plot Type",
       x = "Plot Type",
       y = "Hindfoot Length (mm)") +
  facet_wrap(vars(sex), nrow = 1)

species_count <- complete_old|>
  group_by(species) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  slice(1:5)

ggplot(data = complete_old, aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme(axis.title = element_text(size = 14), legend.position = "none") +
  labs(title = "Rodent Size by Plot Type",
       x = "Plot Type",
       y = "Hindfoot Length (mm)") +
  facet_grid(rows = vars(sex), cols = vars(species))

complete_old |>
  filter(species %in% species_count$species) |>
  ggplot(aes(x = plot_type, y = hindfoot_length, fill = plot_type)) +
  geom_boxplot() +
  geom_jitter(aes(color = plot_type, alpha = 0.2)) +
  geom_point(alpha = 0.2) +
  theme_bw() +
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme(axis.title = element_text(size = 14), legend.position = "none") +
  labs(title = "Rodent Size by Plot Type",
       x = "Plot Type",
       y = "Hindfoot Length (mm)") +
  facet_grid(rows = vars(sex), cols = vars(species))
