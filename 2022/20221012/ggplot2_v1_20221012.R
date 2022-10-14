

library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


tibble(x = 1:10, y = rep(1, 10)) %>% 
  ggplot(aes(x,y)) +
  geom_col()


tibble(v = seq(1, 127)) %>% 
  mutate(x = v%%5)

rep(1:3, each = 10)
tibble(x = rep(1, 10), y = rep(1, 10)) %>% 
  ggplot(aes(x,y)) +
  geom_col(size = 1, colour = "gray100") +
  coord_fixed()


tibble(x = rep(1:3, each = 10), 
       y = rep(1,30), 
       z = rep(1:3, each = 10)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_col(color = "gray100", width = 1) +
  coord_fixed()



expand.grid(x = 1:3, y = 1:5)

v_x_size = 5
tb1 = tibble(v = 1:32) %>% 
  mutate(x1 = v %% v_x_size) %>% 
  mutate(x2 = ifelse(x1 == 0, v_x_size, x1)) %>% 
  mutate(y1 = (v-1) %/% v_x_size) %>% 
  mutate(y2 = y1 + 1) %>% 
  print(n = Inf)


ggplot(tb1, aes(x2, y2)) +
  geom_tile(fill = "blue", 
            alpha = 0.5,
            size = 1, 
            colour = "gray100") +
  coord_fixed()


rep(1,10)


v_x_size = 5
v_group_cnt = 5
tb1 = tibble(g = sample(1:100, v_group_cnt)) %>% 
  mutate(g1 = map2(1, g, rep)) %>% 
  unnest_longer(g1) %>% 
  group_by(g) %>% 
  mutate(v = 1:n()) %>% 
  mutate(x1 = v %% v_x_size) %>% 
  mutate(x2 = ifelse(x1 == 0, v_x_size, x1)) %>% 
  mutate(y1 = (v-1) %/% v_x_size) %>% 
  mutate(y2 = y1 + 1) %>% 
  ungroup() %>% 
  select(g, x = x2, y = y2) %>% 
  print(n = Inf)

v_ord = tb1 %>% distinct(g) %>% 
  pull(g)


ggplot(tb1, aes(x, y)) +
  geom_tile(size = 1, 
            colour = "gray100") +
  facet_wrap(~g, nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")


ggplot(tb1, aes(x, y)) +
  geom_tile(size = 1, 
            colour = "gray100") +
  facet_wrap(~factor(g, level = v_ord), nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")


ggplot(tb1, aes(x, y)) +
  geom_tile(aes(fill = factor(g)), 
            size = 1, 
            colour = "gray100") +
  # scale_fill_brewer(palette = "RdBu") +
  facet_wrap(~factor(g, v_ord), nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none")

tb2 = tb1 %>% 
  group_by(g) %>% 
  mutate(x2 = max(x)/2 + 0.5) %>% 
  mutate(y2 = max(y) + 2) %>% 
  mutate(cnt = 1:n()) %>% 
  mutate(x3 = ifelse(cnt == 1, x2, NA),
         y3 = ifelse(cnt == 1, y2, NA)) %>% 
  ungroup()
tb2
v_background_color = "#334960" #334960 #3D5773 #446180

ggplot(tb2, aes(x, y)) +
  geom_tile(aes(fill = factor(g)), 
            size = 2, 
            colour = v_background_color) +
  geom_text(aes(x3, y3, label = g), na.rm = T,
            family = "BMJUAOTF", colour = "gray100", size = 10) +
  scale_fill_brewer(palette = "BrBG") +
  # ylim(c(0,100)) +
  facet_wrap(~factor(g, v_ord), nrow = 1) +
  coord_fixed() +
  theme_void() +
  labs(title = "ggplot2 - Bar Chart",
       caption = "twitter @sourcebox7") +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        # strip.text = element_text(family = "BMJUAOTF", colour = "gray100", size = 20),
        plot.margin = margin(0.5,0.5,0.3,0.5, "in"),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0,0,0.3,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.2,0,0,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20221012/save_20221012_01.png", 
       width = 8, height = 6, dpi = 240, units = "in")
  