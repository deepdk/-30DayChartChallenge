library(tidyverse)
library(RColorBrewer)
library(showtext)

font_add_google("Playfair Display", "Playfair")  
showtext_auto()

options("device" = "windows")
options("device" = "RStudioGD")


coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

glimpse(coffee_ratings)

view(coffee_ratings)

coffee_ratings %>%
  count(species, sort = TRUE)

coffee_ratings %>%
  count(region, sort = TRUE)
 
coffee_ratings %>%
  count(variety, sort = TRUE)

df1 <- coffee_ratings %>%
  filter(species == 'Arabica')%>%
  count(country_of_origin, sort = TRUE)%>%
  na.omit()
view(df1)

df2 <- coffee_ratings %>%
  filter(species == 'Robusta') %>%
  count(country_of_origin , sort = TRUE)%>%
  na.omit()
view(df2)
  
library(treemapify)
library(paletteer)



p <- ggplot(df1, aes(area = n, fill = n, label = country_of_origin)) +
  geom_treemap(colour = "black", alpha = 1.2) +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 8,
                    family = "Playfair",
                    grow = TRUE) +
  scale_fill_paletteer_c("ggthemes::Brown") +
  labs(title = "Origin Countries of Coffee Beans", subtitle = "Coffee Type: Arebica", 
       caption = "Data Source : #TidyTuesday Week 28/2020 Coffee Ratings")+
  theme_minimal()+
  theme(plot.title = element_text(size = 25, hjust = 0.5,face = "bold", family = "Playfair"),
        plot.subtitle = element_text(size = 15, hjust = 0.5,face = "bold", family = "Playfair"),
        plot.caption = element_text(size = 10, hjust = 0.5,face = "bold"),
        legend.position = "None")
  
p
