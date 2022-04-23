library(tidyverse)
library(paletteer)

df <- ramen_ratings %>%
  group_by(brand) %>%
  summarise(rating = mean(stars)) %>%
  head(10)

brand = df$brand
count = df$n

plot <- ggplot(df,
               aes(
                 fct_reorder(brand, rating), rating, 
                fill =  fct_reorder(brand,rating)
               )) +
  geom_col(width = 1, color = "white") +
  scale_fill_paletteer_d("ggsci::purple_material")

# View the plot
plot

plot <- plot + coord_polar()
plot
# View the plot
plot <- plot + labs(
  x = "",
  y = "",
  title = "Top 10 Ramen Brands According to Ratings",
  caption = "Data Source : Ramen Ratings/#TidyTuesday/week 23/2019"
) 

# View the plot
plot

plot <- plot + 
  # Assign the ggplot2 minimal theme
  theme_minimal() +
  
  # Remove legend, axes, text, and tick marks
  
  
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.caption  = element_text(size = 12, hjust = 0.5)
  )

# View the plot
plot
