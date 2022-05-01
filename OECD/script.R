library(tidyverse)
library(countrycode)
library(paletteer)
library(scales)
library(ggrepel)
library(reshape)
library(gridExtra)
library(showtext)
library(ggtext)
library(gghighlight)

font_add_google('Fira Sans', 'firasans')
showtext_auto()

deaths_per_inhab <- read.csv("../input/deaths-per-inhabitants/deaths_per_inhabitanats.csv")
country_codes <- readxl::read_xlsx("../input/country-codes/country_codes.xlsx")

names(deaths_per_inhab)[names(deaths_per_inhab) == "LOCATION"] <- "alpha-3"

data2 <- deaths_per_inhab %>%
  left_join(country_codes,  by = c("alpha-3"))
  
data2$name[1] <- "United Kingdom"

my_theme <- function() {
  
  # Colors
  color.background = "#CDC9C9"
  color.text = "#030303"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=17, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

p1 <- ggplot(data2, aes(fct_reorder(name, Value), Value, fill = fct_reorder(name, Value)))+
  geom_bar(stat = "identity", col = "gray10") +
  scale_fill_manual( values = c( "Georgia"="red","United States of America" = "red", "Russian Federation" = "red" ), guide = FALSE) +
  annotate("text", x = 10, y = 85, label = "Road accidents are measured in terms of the number of persons injured         \nand deaths due to road accidents,                 \nwhether immediate or within 30 days of the accident,            \nand excluding suicides involving the use of road motor vehicles.                   \nA road motor vehicle is a road vehicle fitted with an engine as the sole means of propulsion                  \nand one that is normally used to carry people or goods, or for towing, on the road.              \nThis includes buses, coaches, trolleys, tramways (streetcars) and road vehicles used to transport goods and to transport passengers.             \nRoad motor vehicles are attributed to the countries where they are registered,                \nwhile deaths are attributed to the countries in which they occur.", size = 6, family = 'firasans') +
  my_theme()+
  labs(title = "No. of Deaths in Road Accidents in 2020",
       subtitle = "Per 1000000 Inhabitants",
       caption = "Data Source : https://data.oecd.org/transport/road-accidents.htm")+
  coord_flip()
p1
