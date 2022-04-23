library(tidyverse)
library(patchwork)
library(paletteer)
library(jpeg)
library(pacman)
library(grid)


members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

peak_n <- peaks %>%
  count(peak_name, sort = TRUE)

peaks %>%
  count(climbing_status, sort = TRUE)


#################################################

data <- left_join(peaks, members, by='peak_name')

my_theme <- function() {
  
  # Colors
  color.background = "#363636"
  color.text = "#FFFFFF"
  
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
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold", hjust = 0.5)) +
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


data %>%
  count(sex, sort = TRUE)%>%
  na.omit()
 

m_climber <- data %>%
  filter(sex == 'M')

f_climber <- data %>%
  filter(sex == 'F')

city_m <- m_climber %>%
  count(citizenship, sort = TRUE)%>%
  na.omit()%>%
  head(20)

city_f <- f_climber %>%
  count(citizenship, sort = TRUE)%>%
  na.omit()%>%
  head(20)

p1 <- ggplot(city_m, aes(fct_reorder(citizenship, n), n, fill = fct_reorder(citizenship, n)))+
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = 0, color = "white")+
  coord_flip()+
  scale_fill_paletteer_d("khroma::iridescent")+
  my_theme()+
  labs(title = "Male")
p1 

p2 <- ggplot(city_f, aes(fct_reorder(citizenship, n), n, fill = fct_reorder(citizenship,n)))+
  geom_col()+
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = 0, color = "white")+
  coord_flip()+
  scale_fill_paletteer_d("khroma::iridescent")+
  my_theme()+
  labs(title = "Female")
p2

P <- (p1 + p2) 

P + plot_annotation(title = 'Where are the climbers from?',
                    subtitle = "Himalayan Climbing Expeditions",
                    caption = "Data Source:Himalayan Climbing Expeditions/#TidyTuesday/Week39/2020",
                    theme = theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                                  plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                                  plot.caption = element_text(size = 10, face = "bold", hjust = 0.5)))
#################################################

unclimed <- data %>%
  filter(climbing_status == "Unclimbed") 

unclimed_peaks <- unclimed %>%
  count(peak_name, sort = TRUE) %>%
  na.omit() %>%
  head(10)

my_theme1 <- function() {
  
  # Colors
  color.background = "#363636"
  color.text = "#FFFFFF"
  
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
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold", hjust = 0.5))+
    theme(plot.subtitle =  element_text(color = color.text, size=15, face = "bold", hjust = 0.5))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=10, color = color.text)) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_blank()) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

p3 <- ggplot(unclimed_peaks, aes(fct_reorder(peak_name, n), n, fill = fct_reorder(peak_name,n)))+
  geom_col()+
  geom_bar(stat = "identity") +
  geom_text(aes(label = peak_name), hjust = 1.2, color = "white")+
  coord_flip()+
  scale_fill_paletteer_d("beyonce::X81")+
  my_theme1()+
  labs(title = "Top 10 Unclimbed Peaks of Himalaya",
       subtitle = "Count is number of ascents ")
p3 
