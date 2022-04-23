library(tidyverse)
library(paletteer)

temp <- read.csv("G:/Z by HP/temperature_change_data_11-29-2021.csv")

df1 <- temp %>%
  filter(Area %in% c("Japan","Germany","China","United States of America")) %>%
  group_by(Year, Area) %>%
  summarise(change = mean(Value))
  
my_theme <- function() {
  
  # Colors
  color.background = "#E8E8E8"
  color.text = "#22211d"
  
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
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    theme(legend.background = element_rect(fill="#E8E8E8", 
                                           size=0.5, linetype="solid"))+
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold", hjust = 0.5))+
    theme(plot.subtitle = element_text(color = color.text, size = 15, hjust = 0.5))+
    theme(plot.caption = element_text(color = color.text, size = 10, hjust = 0.5))+
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

p <- ggplot(df1, aes(Year, change, color = Area))+
  geom_line(size = 1)+
  geom_point()+
  scale_colour_paletteer_d("ggthemes::colorblind")+
  my_theme() +
  labs(x = "Year",
       y = "Change",
       title = "Temperature Change for Top 4 Economics",
       subtitle = "With respect to 1951-1980 baseline climatology",
       caption = "Data Source : Kaggle/Z by Hp Data Visualiztion Challenge")
p       
       
       
       
