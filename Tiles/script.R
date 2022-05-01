library(tidyverse)
library(RColorBrewer)
library(showtext)
library(treemapify)
library(paletteer)
library(waffle)
library(hrbrthemes)
library(ggalt)
library(GGally)
library(ggimage)

font_add_google("Playfair Display", "Playfair")  
font_add_google('Fira Sans', 'firasans')
showtext_auto()

df = read.csv("../input/world-happiness-report-2021/world-happiness-report-2021.csv")

df_cor <- df %>% 
    select(Corruption = Perceptions.of.corruption,
           Generosity = Generosity,
           Freedom = Freedom.to.make.life.choices, 
           Life_Expectancy = Healthy.life.expectancy, 
           Social_Support = Social.support,
           GDP_Per_Capita = Logged.GDP.per.capita, 
           Happiness = Ladder.score
           )

my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
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
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Playfair'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Playfair'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=17, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

p1 <- ggcorr(df_cor, 
       method = c("everything", "pearson"), 
       size = 10, hjust = 0.77,
       fontface = "bold",
       family = 'Playfair',
       color = 'white',      
       label = TRUE, label_size = 15,
       layout.exp = 1) +
       scale_fill_paletteer_c("grDevices::Mint")+
       my_theme()+
       annotate("text", x = 4, y = 6, label = "Happiness most strongly correlates with      \n(1) wealth(GDP)      \n(2) health,     \n(3) social support,    \n(4) freedom'"  , size = 15, family = 'lora', color = 'white')+
       labs(title = 'Which Factors Most Strongly Correlate With Happiness',
           subtitle = "World Happiness Report 2021",
           caption = "Data Source : https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2021")
p1

url <- "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse3.mm.bing.net%2Fth%3Fid%3DOIP.wbTIFSSicsbig448lcENyQHaEo%26pid%3DApi&f=1"
ggbackground(p1, url)

