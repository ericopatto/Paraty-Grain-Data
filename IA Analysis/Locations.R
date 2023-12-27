# Library
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggforce)

# Data
loc <- read_csv("../Data/Loc Data.csv") %>%
  mutate(Beach = substr(Site, 1, 1),
         Time = ymd_hms(paste0("2023-09-", Day, " ", as.character(Time))))

# ggsave("../Figures/R Maps/Locations/OverviewMap.png", width = 2654, height = 2054, units = "px")

# Overview
# top = -22.85, left = -44.8, bottom = -23.39, right = -44.1
# top = -23.17, left = -44.8, bottom = -23.39, right = -44.5
overview <- get_stadiamap(bbox = c(top = -23.17, left = -44.8, bottom = -23.415, right = -44.5), zoom = 12, maptype = "stamen_terrain")

ggmap(overview)+
  geom_point(mapping = aes(x = lon, y = lat, colour = Beach), data = loc,
             shape = "circle", size = 3)+
  annotate(geom = "spoke", x = -44.6, y = -23.395, angle = 125*pi/180, radius = 0.02,
           arrow=arrow(length = unit(0.4,"cm")), colour = "blue", linewidth = 1.2)+
  annotate(geom = "spoke", x = -44.6, y = -23.395, angle = c(125+68, 125-68)*pi/180, radius = 0.02,
           arrow=arrow(length = unit(0.4,"cm")), colour = "blue", linewidth = 0.5, linetype = 2)+
  theme_minimal()+
  theme(plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = 0.1, t = 0.2, l = 0.5, unit = "cm")),
        legend.key.size = grid::unit(1.8,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_colour_brewer(palette = "Set1", labels = c("Antigos", "Pontal", "Sono"))+
  scale_x_continuous(labels = function(x){paste0(round(x, 1), "º")})+
  scale_y_continuous(labels = function(x){paste0(round(x, 1), "º")})+
  labs(title = "Paraty Geography Overview",
       x = "", y = "")+
  coord_equal()

# Pontal
pontal <- get_stadiamap(bbox = c(top = -23.2135, left = -44.713, bottom = -23.220, right = -44.709), zoom = 18, maptype = "stamen_terrain")

ggmap(pontal)+
  geom_point(mapping = aes(x = lon, y = lat, colour = Site), data = loc[loc$Beach == "P",],
             shape = "circle", size = 6)+
  theme_void()+ 
  theme(plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = 0.1, t = 0.2, l = 2, unit = "cm")),
        legend.key.size = grid::unit(1.8,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))+
  scale_colour_brewer(palette = "Set2")+
  labs(title = "Praia do Pontal - Sites")

# Sono
sono <- get_stadiamap(bbox = c(top = -23.330, left = -44.640, bottom = -23.338, right = -44.627), zoom = 18, maptype = "stamen_terrain")

ggmap(sono)+
  geom_point(mapping = aes(x = lon, y = lat, colour = Site), data = loc[loc$Beach == "S",],
             shape = "circle", size = 6)+
  theme_void()+ 
  theme(plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = 0.1, t = 0.2, l = 2, unit = "cm")),
        legend.key.size = grid::unit(1.8,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))+
  scale_colour_brewer(palette = "Set2")+
  labs(title = "Praia do Sono - Sites")

# Antigos
antigos <- get_stadiamap(bbox = c(top = -23.3359, left = -44.6247, bottom = -23.3408, right = -44.620), zoom = 18, maptype = "stamen_terrain")

ggmap(antigos)+
  geom_point(mapping = aes(x = lon, y = lat, colour = Site), data = loc[loc$Beach == "A",],
             shape = "circle", size = 6)+
  theme_void()+ 
  theme(plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = 0.1, t = 0.2, l = 2, unit = "cm")),
        legend.key.size = grid::unit(1.8,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))+
  scale_colour_brewer(palette = "Set2")+
  labs(title = "Praia dos Antigos - Sites")

###
sonoantigos <- get_stadiamap(bbox = c(top = -23.330, left = -44.640, bottom = -23.355, right = -44.618), zoom = 16, maptype = "stamen_terrain")

ggmap(sonoantigos)+
  geom_point(mapping = aes(x = lon, y = lat, colour = Site), data = loc[loc$Beach != "P",],
             shape = "circle", size = 6)+
  annotate(geom = "spoke", x = -44.622, y = -23.353, angle = 125*pi/180, radius = 0.005,
           arrow=arrow(length = unit(0.4,"cm")), colour = "blue", linewidth = 1.5)+
  annotate(geom = "spoke", x = -44.622, y = -23.353, angle = c(125+68, 125-68)*pi/180, radius = 0.005,
           arrow=arrow(length = unit(0.4,"cm")), colour = "blue", linewidth = 0.5, linetype = 2)+
  theme_void()+ 
  theme(plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = 0.1, t = 0.2, l = 2, unit = "cm")),
        legend.key.size = grid::unit(1.8,"lines"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14))+
  scale_colour_brewer(palette = "Set1")+
  labs(title = "Antigos and Sono - Sites")
