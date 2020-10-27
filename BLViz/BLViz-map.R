library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)
library(gganimate)

bl2map <- read.csv("bl2map.csv")
bl2mapv <- as.character(bl2map$bl.name)
names(bl2mapv) <- bl2map$map.name

bl_map_data <- map_data("world") %>%
  mutate(
    region = case_when(
      subregion == "Macao" ~ "Macao",
      subregion == "Hong Kong" ~ "Hong Kong",
      TRUE ~ region
    )) %>%
  mutate(
    country = recode(region, !!!bl2mapv)
  )

map.plot <- read.csv("../BLData/BL2013_MF1599_v2.2.csv") %>%
  select(country,year,lh) %>%
  mutate(lh = lh / 100) %>%
  left_join(bl_map_data) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = lh)) +
  geom_polygon(colour = "black", size=0.1) +
  scale_fill_distiller(palette="Purples", na.value = "white", direction=1, labels = scales::percent, name ="") +
  theme_void() +
  theme_void() + theme(
    panel.grid.major.y = element_blank(),
    legend.position = "right",
    #plot.margin = margin(1, 2, 1, 4, "cm"),
    plot.title=element_text(size=25, hjust=0.5, vjust=1, face="bold", colour="grey"),
    plot.subtitle=element_text(size=18, hjust=0.5, vjust=1, face="italic", color="grey"),
    plot.caption=element_text(size=16, hjust=0.5, vjust=1, face="italic", color="grey")
  )

race.anim = map.plot + transition_states(year, transition_length = 10, state_length = 0, wrap=FALSE) +
  #view_follow(fixed_y = TRUE)  +
  enter_fade() + exit_fade() +
  labs(title = 'Tertiary education rate : {closest_state}',
       caption  = "Data source : Barro-Lee | Credit @JulienGossa") 

animate(race.anim, 36, fps = 2,  width = 1200, height = 675, start_pause = 0, end_pause=4,
        renderer = gifski_renderer("educmap.gif", loop = TRUE))

