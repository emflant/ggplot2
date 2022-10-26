

library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


tb1 = tibble(x = c(1,2,3,1),
             y = c(1,1,1,2))

tb1
ggplot(tb1, aes(x, y, fill = factor(1))) +
  geom_tile(size = 2, colour = "gray100") +
  coord_fixed()


ggplot(tb1, aes(x, y, fill = factor(1))) +
  geom_tile(size = 2, colour = "gray100") +
  geom_text(aes(label = paste0("x = ", x, ", y = ", y))) +
  coord_fixed()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221014/images/20221014_11.png", 
       width = 6, height = 3.5, dpi = 120, units = "in")


tb1 = tibble(x = c(1,2,3,1,2),
             y = c(1,1,1,2,2),
             z = c(1,1,1,1,2))
tb1
ggplot(tb1, aes(x, y, fill = factor(z))) +
  geom_tile(size = 2, colour = "gray100") +
  geom_text(aes(label = paste0("x = ", x, ", y = ", y))) +
  coord_fixed()
ggsave("~/github/hugo/app/app-01/content/post/2022/20221014/images/20221014_12.png", 
       width = 6, height = 3.5, dpi = 120, units = "in")

# v_x_size = 5
tb1 = tibble(v = 1:18) %>% 
  mutate(x1 = v %% 5) %>% 
  mutate(x2 = ifelse(x1 == 0, 5, x1)) %>% 
  mutate(y1 = (v-1) %/% 5) %>% 
  mutate(y2 = y1 + 1) %>% 
  select(x2, y2)

tb1

ggplot(tb1, aes(x2, y2, fill = factor(1))) +
  geom_tile(size = 1, colour = "gray100") +
  geom_text(aes(label = paste0("(", x2, ", ", y2, ")"))) +
  coord_fixed()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221014/images/20221014_13.png", 
       width = 6, height = 4, dpi = 120, units = "in")


v_x_size = 5
v_group_cnt = 5
set.seed(38734)
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


ggplot(tb1, aes(x, y, fill = factor(g))) +
  geom_tile(size = 1, 
            colour = "gray100") +
  facet_wrap(~factor(g, v_ord), nrow = 1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/app-01/content/post/2022/20221014/images/20221014_21.png", 
       width = 6, height = 4, dpi = 120, units = "in")




v_x_size = 5
v_group_cnt = 5
set.seed(38734)
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
  scale_fill_brewer(palette = "RdYlGn") + #BrBG
  # ylim(c(0,100)) +
  facet_wrap(~factor(g, v_ord), nrow = 1) +
  coord_fixed() +
  theme_void() +
  labs(title = "ggplot2 - Bar Chart",
       caption = "twitter @sourcebox7") +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "in"),
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


ggsave("~/github/hugo/app/app-01/content/post/2022/20221014/images/20221014_02.png", 
       width = 8, height = 6, dpi = 120, units = "in")
  