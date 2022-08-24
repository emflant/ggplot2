
library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

rank(c(2,5,5,1,9,3), ties.method = "min")
order(c(2,5,1,9,3))
c(2,5,1)[order(c(2,5,1))]

tibble(x = 1:10, y = sample(30:100, 10), z = runif(10)) %>% 
  mutate(ord = rank(y, ties.method = "min"))

tibble(x = 1:5, y = sample(30:100, 5), z = runif(5)) %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  ggplot(aes(area = y, fill = factor(ord), label = y)) +
  geom_treemap(start = "topleft") +
  geom_treemap_text(start = "topleft") +
  scale_fill_brewer() 



set.seed(3943809)
tb1 = tibble(x = 1:5, y = sample(30:100, 5), z = runif(5)) %>% 
  mutate(ord = rank(y, ties.method = "min"))
tb1 = tibble(x = 1:9, y = sample(30:100, 9), z = runif(9)) %>% 
  mutate(ord = rank(y, ties.method = "min"))

tb1 = tibble(x = LETTERS[1:9], y = c(500, 40, 59, 29, 39, 98, 110, 5, 10)) %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  mutate(max_value = ord == 9) %>% 
  mutate(font_size = as.integer(ord * 4) ) %>% 
  mutate(rate = round(y / sum(y) * 100, 1)) %>% 
  mutate(rate_label = paste0(rate, "%"))
tb1
v_background_color = "#2E5E77"
v_background_color = "#2E5E77"
tb1 %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  ggplot(aes(area = y, fill = max_value)) +
  geom_treemap(start = "topleft", 
               # layout = "srow",
               colour = "#2E5E77", size = 10) +
  geom_treemap_text(aes(label = x), 
                    size = 20,
                    start = "topleft",
                    colour = "gray100",
                    padding.x = unit(0.2, "in"),
                    padding.y = unit(0.2, "in"),
                    family = "BMJUAOTF", min.size = 10) +
  geom_treemap_text(aes(label = rate_label),
                    size = tb1$font_size,
                    start = "topleft",
                    colour = "gray100", 
                    padding.x = unit(0.15, "in"),
                    padding.y = unit(0.2, "in"),
                    place = "bottomright", 
                    family = "BMJUAOTF",
                    min.size = 10) +
  scale_fill_manual(values = c("#A67EE2", "#7b59b3")) +  #594480 #815DB9
  labs(caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.99, 
                                    size = 10,
                                    margin = margin(0.2,0,0,0,"cm")),
        plot.margin = margin(0.5,0.5,0.3,0.4,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220823/save_ggplot_01.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")

tb1 %>% 
  mutate(ord = rank(y, ties.method = "min")) %>% 
  ggplot(aes(area = y, fill = max_value)) +
  geom_treemap(start = "topleft", 
               # layout = "srow",
               colour = "white", size = 5) +
  # geom_treemap(start = "topleft", layout = "scol") +
  # geom_treemap(start = "topleft", layout = "srow") +
  # geom_treemap(start = "topleft", layout = "fixed") +
  # geom_treemap_text(aes(label = x), 
  #                   start = "topleft",
  #                   place = "topleft",
  #                   family = "BMJUAOTF") +
  # geom_treemap_text(aes(label = rate), 
  #                   start = "topleft",
  #                   place = "bottomright",
  #                   family = "BMJUAOTF") +
  scale_fill_brewer(palette = "Purples") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none")
  
