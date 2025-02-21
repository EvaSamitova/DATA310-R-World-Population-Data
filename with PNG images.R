library("tidyverse")
library("ggforce")
library("maps")

# Get the data for cities of the world
cities <- as_tibble(world.cities)

# Convert the capital column to the factor type
cities <- cities %>% mutate(capital = as.factor(capital))

# Generate stats for the average population and standard deviation based on capital status
city_stats <- cities %>% group_by(capital) %>%
  summarize(MeanPop = mean(pop), SDPop = sd(pop))

# Generate and display bar plot
bar_plot <- ggplot(city_stats, aes(x = capital, y = MeanPop, fill = capital)) +
  geom_col() +
  geom_errorbar(aes(ymin = MeanPop - SDPop/2, ymax = MeanPop + SDPop/2), width = 0.25) +
  scale_fill_brewer(palette = "Dark2")

print(bar_plot) # Display plot in RStudio
ggsave("bar_plot.png", plot = bar_plot, width = 8, height = 6, dpi = 300)

# Generate and display scatter plot
scatter_plot <- ggplot(cities, aes(x = pop, y = lat, color = capital, shape = capital)) +
  geom_point(size = 2, alpha = .5)

print(scatter_plot)
ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 6, dpi = 300)

# Generate and display facet matrix plot
facet_matrix_plot <- ggplot(cities, aes(x = .panel_x, y = .panel_y, color = capital, shape = capital)) +
  geom_point(size = .5, alpha = .5) +
  facet_matrix(vars(pop, lat, long))

print(facet_matrix_plot)
ggsave("facet_matrix_plot.png", plot = facet_matrix_plot, width = 10, height = 6, dpi = 300)

# Generate and display world map
world_map_plot <- ggplot() + 
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(data = filter(cities, pop > 2000000), aes(x = long, y = lat, shape = capital, color = capital), size = 2) +
  coord_fixed()

print(world_map_plot)
ggsave("world_map_plot.png", plot = world_map_plot, width = 10, height = 6, dpi = 300)

# Generate and display Pacific Islands map
pacific_map_plot <- ggplot() + 
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(data = cities, aes(x = long, y = lat, shape = capital, color = capital, alpha = pop, size = pop)) +
  coord_cartesian(xlim = c(-175,-135), ylim = c(-25,-5)) +
  labs(title = "Pacific Islands", x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "lightblue"))

print(pacific_map_plot)
ggsave("pacific_map_plot.png", plot = pacific_map_plot, width = 10, height = 6, dpi = 300)
