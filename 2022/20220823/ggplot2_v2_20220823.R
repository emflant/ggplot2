
library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


# set.seed(3943809)
# tb1 = tibble(x = 1:5, y = sample(30:100, 5), z = runif(5)) %>% 
#   mutate(ord = rank(y, ties.method = "min"))
# tb1 = tibble(x = 1:9, y = sample(30:100, 9), z = runif(9)) %>% 
#   mutate(ord = rank(y, ties.method = "min"))

tb1 = tibble(x = LETTERS[1:9], y = c(500, 40, 59, 29, 39, 98, 110, 5, 10)) %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  mutate(max_value = ord == 9) %>% 
  mutate(font_size = as.integer(ord * 4) ) %>% 
  mutate(rate = round(y / sum(y) * 100, 1)) %>% 
  mutate(rate_label = paste0(rate, "%"))
tb1
v_background_color = "#334960"
v_direction = 1
v_font_color = c("gray100", v_background_color, v_background_color, #abc
                 v_background_color, v_background_color, "gray100", #edf
                 "gray100", "gray20", "gray20") #ghi

# v_direction = -1
# v_font_color = c(v_background_color, v_background_color, v_background_color, #abc
#                  "gray100", "gray100", v_background_color, #edf
#                  v_background_color, "gray100", "gray100") #ghi


tb1 %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  ggplot(aes(area = y, fill = factor(ord))) +
  geom_treemap(start = "topleft", 
               # layout = "srow",
               colour = v_background_color, size = 10) +
  geom_treemap_text(aes(label = x), 
                    size = 20,
                    start = "topleft",
                    colour = v_font_color,
                    padding.x = unit(0.2, "in"),
                    padding.y = unit(0.2, "in"),
                    family = "BMJUAOTF", min.size = 10) +
  geom_treemap_text(aes(label = rate_label),
                    size = tb1$font_size,
                    start = "topleft",
                    colour = v_font_color, 
                    padding.x = unit(0.15, "in"),
                    padding.y = unit(0.2, "in"),
                    place = "bottomright", 
                    family = "BMJUAOTF",
                    min.size = 10) +
  scale_fill_brewer(palette = "YlGnBu", direction = v_direction) +
  labs(title = "ggplot2 - Treemap Chart",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.01, 
                                  size = 20,
                                  margin = margin(0.1,0,0.2,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.99, 
                                    size = 10,
                                    margin = margin(0.3,0,0,0,"cm")),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220823/save_ggplot_02.png", 
       width = 8, height = 5, dpi = 240, units = "in")

  
