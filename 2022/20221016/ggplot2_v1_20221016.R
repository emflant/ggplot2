library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)
library(ggbump)

to_vector = function(a){
  str_extract(a, "'.+'") %>% 
    str_replace_all("'", "") %>% 
    str_split(", ") %>% unlist
}

# a = "['IPAD', 'IPHONE', 'ARKIT', 'AUGMENTED REALITY']"
# to_vector(a)

tuesdata <- tidytuesdayR::tt_load('2022-10-04')
tuesdata <- tidytuesdayR::tt_load(2022, week = 40)

product_hunt <- tuesdata$product_hunt
str(product_hunt)

tb1 = product_hunt %>% 
  mutate(release_year = year(release_date)) %>% 
  select(release_year, category_tags) %>% 
  mutate(tags = map(category_tags, to_vector)) %>% 
  unnest_longer(tags) %>% 
  select(tags, year = release_year)
  
tb1 %>% summarise(max_year = max(year))

tb2 = tb1 %>% group_by(tags, year) %>% 
  summarise(cnt = n(), .groups = "drop") %>% 
  group_by(year) %>% 
  arrange(year, desc(cnt)) %>% 
  mutate(ord = 1:n()) %>% 
  ungroup()


tb3 = tb2 %>% 
  filter(year == 2021, ord <= 5) %>% 
  select(tags)
v_ord = tb3 %>%  pull(tags)


tb4 = tb2 %>% inner_join(tb3)


ggplot(tb4, aes(year, ord, colour = tags)) +
  geom_line() +
  scale_y_reverse()

  

ggplot(tb4, aes(year, ord, colour = tags)) +
  geom_bump() + 
  geom_point() +
  scale_y_reverse()
  


ggplot(tb4, aes(year, ord, colour = factor(tags,v_ord))) +
  geom_bump() + 
  geom_point() +
  scale_y_reverse()


v_background_color = "#334960"

a = 0


a = a + 1

v_background_color = "gray100"


# v_color = c('#9e0142','#f46d43','#fee08b','#3288bd', '#5e4fa2')
v_color = c('#fec44f','#fe9929','#ec7014','#cc4c02','#993404')
# ['#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506']

v_color = c('#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8')
# ['#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58']


tb5 = tb4 %>% mutate(l = ifelse(year == 2014 | year == 2021, ord, NA)) %>% 
  mutate(l2 = ifelse(!is.na(l) & year == 2021, paste0(l, ".", tags), l)) %>% 
  mutate(l_pos = ifelse(year == 2014, 2013.5, 2021.3))



ggplot(data = tb5, aes(year, ord, colour = factor(tags,v_ord))) +
  geom_bump(size = 2) + 
  geom_point(size = 4) +
  geom_label(aes(l_pos, ord, label = l2),
             na.rm = T,hjust = 0) +
  scale_x_continuous(position = "top", breaks = 2014:2021,
                     labels = paste0("'", 14:21),
                     expand = expansion(c(0,0.3))) + 
  scale_y_reverse() +
  # scale_color_brewer(palette = "Blues") +
  scale_color_manual(values = v_color) +
  theme_void() +
  labs(title = "ggplot2 - Donut Chart",
       caption = "twitter @sourcebox7") +
  theme(legend.position = "none",
        plot.margin = margin(0.3,0.1,0.3,0.1,"in"),
        axis.text.x.top = element_text(family = "BMJUAOTF",
                                       size = 12),
        plot.title = element_text(color = "gray0", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.3,0,0.3,0,"in")),
        plot.caption = element_text(color = "gray0", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(0,0,0,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20221016/save_01.png", 
       width = 8, height = 8, dpi = 120, units = "in")





v_background_color = "#334960" 
v_color = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4')
# ['#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58']


v_color = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0')
# ['#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84']
ggplot(data = tb5, aes(year, ord, colour = factor(tags,v_ord))) +
  geom_bump(size = 2) + 
  geom_point(size = 4) +
  geom_label(aes(l_pos, ord, label = l2),
             na.rm = T,hjust = 0,
             fill = v_background_color,
             family = "BMJUAOTF",
             label.size = .5) +
  scale_x_continuous(position = "top", breaks = 2014:2021,
                     labels = paste0("'", 14:21),
                     expand = expansion(c(0,0.3))) + 
  scale_y_reverse() +
  # scale_color_brewer(palette = "Blues") +
  scale_color_manual(values = v_color) +
  theme_void() +
  labs(title = "ggplot2 - Bump Chart",
       caption = "twitter @sourcebox7") +
  theme(legend.position = "none",
        plot.margin = margin(0.3,0.3,0.5,0.5,"in"),
        axis.text.x.top = element_text(family = "BMJUAOTF",
                                       size = 12,
                                       colour = "gray100"),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0, 
                                  size = 20,
                                  margin = margin(0.2, 0, 0.4, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(-0.5,0,0,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20221016/save_02.png", 
       width = 8, height = 7, dpi = 240, units = "in")
