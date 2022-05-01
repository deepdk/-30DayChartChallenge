library(ggplot2) 
library(scales) 
library(patchwork) 
library(RColorBrewer)
library(corrplot)
library(ggthemes)
library(viridis)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

genres_to_include <- c('pop', 'rock')
df <- filter(spotify_songs, playlist_genre %in% genres_to_include)

d1 <- df %>% 
  select(playlist_genre, danceability,energy, loudness,speechiness, acousticness, liveness, valence) %>%
  na.omit()
  
my_theme <- function() {
  
  # Colors
  color.background = "#FFFFF0"
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
    theme(legend.position = "top") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=30, face = "bold", hjust = 0.5)) +
    theme(plot.subtitle    = element_text(color=color.text, size=25, face = "bold", hjust = 0.5)) +
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

g <- ggpairs(data = d1, columns = 2:8,
             aes(colour = playlist_genre),
            upper = "blank") +
scale_colour_manual(values = c("#CD6090", "#008B00")) +
scale_fill_manual(values = c("#CD6090", "#008B00"))+
labs(title = "Spotify Songs",
    subtitle = "Relation between variables for pop and rock songs")+
my_theme()

g
