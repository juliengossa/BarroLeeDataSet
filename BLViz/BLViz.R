library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(ggthemes)

bl <- read.csv("../BLData/BL2013_MF2599_v2.2.csv") %>%
  mutate(
    year = as.factor(year),
    lh.pop = lh * pop)

bl.sel <- filter(bl, country %in% 
                   c("Germany","France",
                     "USA","United Kingdom", "Russian Federation",
                     "Republic of Korea","China","Japan"))

bl.sel <- filter(bl, country %in% 
                   c(
                     "USA","United Kingdom", "Russian Federation",
                     "China"))


ggplot(bl, aes(x=year,y=lh.pop)) + geom_boxplot() +
  geom_line(data=bl.sel, aes(colour=country, group=country))
  
bl %>% 
  filter(year==2010) %>%
  mutate(
    rank.lh = rank(-lh),
    rank.lhc = rank(-lhc)) %>%
  arrange(rank.lhc) %>%
  select(country,rank.lh,lh,rank.lhc,lhc)




bl <- bind_rows(
  read.csv("BL2013_M2599_v2.2.csv") %>% mutate(sex = as.character(sex)),
  read.csv("BL2013_F2599_v2.2.csv") %>% mutate(sex = "F"),
  read.csv("BL2013_MF2599_v2.2.csv") %>% mutate(sex = as.character(sex)),
  read.csv("BL2013_M1599_v2.2.csv") %>% mutate(sex = as.character(sex)),
  read.csv("BL2013_F1599_v2.2.csv") %>% mutate(sex = "F"),
  read.csv("BL2013_MF1599_v2.2.csv") %>% mutate(sex = as.character(sex))) %>%
  mutate(
    year = as.factor(year),
    sex = as.factor(sex))




bl %>%
  filter(agefrom == 15, sex %in% c("M","F")) %>%
  group_by(year,sex) %>%
  summarise(
    pop.lhc = sum(lhc * pop),
    pop = sum(pop),
    lhc = pop.lhc / pop) %>%
  ggplot(aes(x=year,y=lhc, fill=sex)) + 
  # geom_col(, position="dodge") +
  geom_line(aes(colour=sex, group=sex), size=2) +
  theme_hc()

bl %>%
  filter(agefrom == 15, sex %in% c("M","F")) %>%
  ggplot(aes(x=year,y=lhc, fill=sex)) + 
  geom_smooth(aes(colour=sex, group=sex)) +
  theme_hc()

  
bl %>%
  filter(agefrom == 15, sex %in% c("M","F")) %>%
  group_by(year,sex,region_code) %>%
  summarise(lhc = sum(lhc)) %>%
  ggplot(aes(x=sex,y=lhc, fill=region_code)) + geom_col() +
  facet_wrap( ~ year) +
  theme_hc()


bl %>%
  filter(agefrom == 25, sex %in% c("M","F")) %>%
  ggplot(aes(x=year, y=lhc, colour=sex)) + geom_boxplot() + 
    geom_line(data=filter(bl, country %in% c("Germany")), 
              aes(linetype=country, colour=sex, group=interaction(country,sex))) +
    theme_hc()


bl.diff <- bl %>%
  filter(agefrom == 25, sex %in% c("M","F")) %>%
  select(BLcode,region_code,country,year,sex,lhc) %>%
  pivot_wider(names_from = sex, values_from = lhc) %>%
  mutate(diff = F-M) 

  ggplot(bl.diff,aes(x=year, y=diff)) + geom_boxplot() + theme_hc()

  bl.diff %>% 
    filter(year == max(levels(year))) %>%
    mutate(rank = rank(-diff)) %>%
    arrange(desc(rank))

map_countries <- unique(as.character(map_data("world")$region))
bl_countries <- unique(as.character(bl.diff$country))
setdiff(bl_countries,map_countries)




# Anime
p.global <- bl %>% 
  filter(agefrom==15, sex=="MF") %>%
  select(country,year,lhc) %>%
  #mutate(year=as.numeric(as.character(year))) %>%
  left_join(bl_map_data) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = lhc)) +
    geom_polygon(colour = "black", size=0.1) +
    scale_fill_distiller(palette="Purples", na.value = "white", direction=1, labels = scales::percent) +
    theme_void() +
    #labs(title = "Année = {round(frame_time,0)}")  +
    #transition_time(year)
    #transition_manual(year)
    labs(title = "Année = {closest_state}") 

animate(
  plot = p.global + 
    transition_states(year, transition_length = 3, state_length = 1) + 
    enter_fade() + 
    exit_fade(),
  width = 16,
  height = 8,
  units = "cm",
  res = 120,
  renderer = gifski_renderer(file = "lhc.gif")
)


p.global <- bl %>% 
  filter(agefrom==15, sex=="MF") %>%
  mutate(lhc.pop = lhc*pop) %>%
  select(country,year,lhc.pop) %>%
  left_join(bl_map_data) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = lhc.pop)) +
  geom_polygon(colour = "black", size=0.1) +
  scale_fill_distiller(palette="Purples", na.value = "white", direction=1, labels = scales::percent) +
  theme_void() +
  #labs(title = "Année = {round(frame_time,0)}")  +
  #transition_time(year)
  #transition_manual(year)
  labs(title = "Année = {closest_state}") 

animate(
  plot = p.global + 
    transition_states(year, transition_length = 3, state_length = 1) + 
    enter_fade() + 
    exit_fade(),
  width = 16,
  height = 8,
  units = "cm",
  res = 120,
  renderer = gifski_renderer(file = "lhc-pop.gif")
)

levelsPaletteP <- RColorBrewer::brewer.pal(8,"Paired")[c(2,1,4,3,8,7,5)]
levelsPalette <- rev(RColorBrewer::brewer.pal(8,"Purples"))

bl %>% 
  filter(agefrom==25, sex=="MF", country %in% c("USA","China","United Kingdom","Germany","France")) %>%
  mutate(
    lp = lp-lpc,
    ls = ls-lsc,
    lh = lh-lhc
  ) %>%
  mutate(country = fct_rev(country)) %>%
  select(country, year, pop, lu:lhc) %>%
  pivot_longer(cols = lu:lhc, names_to="level", values_to="value") %>%
  mutate(level = factor(level, levels=rev(c("lu","lp","lpc","ls","lsc","lh","lhc")))) %>%
  mutate(value.pop = value*pop / 1000) %>%
  ggplot(aes(x=year,y=value.pop,colour=level,fill=level,group=level)) + geom_area() +
    facet_grid(country ~ .) +
    scale_fill_manual(values=levelsPalette) +
    scale_colour_manual(values=levelsPalette) +
    #scale_y_continuous(labels = scales::percent) +
    theme_excel_new()



bl %>% 
  filter(agefrom==25, sex=="MF") %>%
  select(country,year,lhc) %>%
  filter(as.numeric(year) > 5) %>%
  pivot_wider(
    names_prefix = 'y',
    names_from = year,
    values_from = lhc
  ) %>%
  mutate(
    diff = y2010 - y2005
  ) %>%
  arrange(diff) -> tes


filter(select(bl,country,year), year == 1950) 

bl %>%
  group_by(year, country) %>%
  filter(country %in% c("France","Germany")) %>%
  summarise(population = sum(pop)) %>%
  ggplot(aes(x=year,y=population,group = country, colour = country)) + geom_line() +
  scale_color_brewer(palette="Paired")





