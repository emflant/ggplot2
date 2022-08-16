library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)


tibble(x = rep(1,10), y = rep(1,10))

tibble(x = rep(1,10), y = rep(1,10), z = 1:10) %>% 
  ggplot(aes(x,y, fill = z)) +
  geom_col(color = "gray100")


tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:10, 3))
tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:10, 3)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_col(color = "gray100")


v_size = 10

tibble(x = rep(1:v_size, each = 10), y = rep(1,v_size*10), z = rep(1:10, v_size)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_col(color = "gray100", width = 0.95)


tibble(x = rep(1:v_size, each = 10), y = rep(1,v_size*10), z = c(rep("a", 35), rep("b", 65)))


v_background_color = "gray100"
tibble(x = rep(1:v_size, each = 10), y = rep(1,v_size*10), z = c(rep("a", 35), rep("b", 65))) %>% 
  ggplot(aes(x,y, fill = factor(z, levels = c("b", "a")))) +
  geom_col(color = "gray100", size = 1, width = 1) +
  scale_fill_brewer(palette = "PuRd") +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none", aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = v_background_color))
  

ggsave("~/github/ggplot2/2022/20220816/save_ggplot_4x3_01.png", 
       width = 8, height = 6, dpi = 320, units = "in")
