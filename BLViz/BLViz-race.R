# BLViz Race
library(tidyverse)
library(ggthemes)
library(gganimate)
library(gifski)

pop_label <- function(pop) {
  paste(format(round(pop), big.mark=" "),"k")
}

pop_label_M <- function(pop) {
  paste(format(round(pop/1000), big.mark=" "),"M")
}

bl <- read.csv("../BLData/BL2013_MF1599_v2.2.csv") %>%
  mutate(year = as.factor(year)) %>%
  group_by(year) %>% 
  mutate(
    rank = rank(-(ls + lh)*pop),
    pop.ls = ls * pop /100,
    pop.lh = lh * pop /100,
    val = pop_label(pop.ls + pop.lh)
    )

maxval <- max(bl$pop.ls+bl$pop.lh)

race.plot <- bl %>%
  filter(rank <= 10) %>%
  #filter(year == 1950) %>%
  ggplot(aes(y=-rank, group=country)) +
    geom_tile(aes(x=pop.ls/2, width = pop.ls, fill=country, alpha="ls", height=0.9)) +
    geom_tile(aes(x=pop.ls+pop.lh/2, width = pop.lh, fill=country, alpha="lh", height=0.9)) +
    geom_text(aes(x = 0, label = paste(country, " "), color = country), hjust = 1, vjust = 0.5) +
    geom_text(aes(x=pop.ls+pop.lh, label = pop_label(pop.ls+pop.lh), color = country), hjust=0, vjust=0.5) +
    #coord_flip(clip = "off", expand = FALSE) +
    #geom_rect(xmin=-maxval, xmax=maxval, ymin=-2, ymax=0.5, fill="white", alpha = 1) +
    coord_cartesian(clip = "off", expand = FALSE) +
    scale_x_continuous(labels = pop_label_M) +
    scale_alpha_discrete(range=c(0.9,0.5), label=c("Secondary","Tertiary"), limits=c("ls","lh"), name="Education level") +
    ylab("Population") +
    guides(fill=FALSE, color=FALSE) +
    theme_void() + theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(size=0.1, color="grey"),
      axis.text.x = element_text(),
      legend.position = "bottom",
      plot.margin = margin(1, 2, 1, 4, "cm"),
      plot.title=element_text(size=25, hjust=0.5, vjust=1, face="bold", colour="grey"),
      plot.subtitle=element_text(size=18, hjust=0.5, vjust=1, face="italic", color="grey"),
      plot.caption=element_text(size=16, hjust=0.5, vjust=0, face="italic", color="grey")
      )

#raceplot

race.anim = race.plot + transition_states(year, transition_length = 10, state_length = 0, wrap=FALSE) +
  view_follow(fixed_y = TRUE)  +
  labs(title = 'Educated populations : {closest_state}',
       subtitle  =  "Top 10 Countries",
       caption  = "Data source : Barro-Lee | Credit @JulienGossa") 

animate(race.anim, 520, fps = 24,  width = 600, height = 700, start_pause = 40, end_pause=60,
        renderer = gifski_renderer("educrace.gif", loop = TRUE))

animate(race.anim, 65, fps = 5,  width = 600, height = 700,
        renderer = gifski_renderer("educrace.gif", loop = TRUE))

