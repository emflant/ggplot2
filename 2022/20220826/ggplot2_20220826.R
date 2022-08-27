library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

# v_background_color = "#334960"
v_background_color = "gray100"
v_point_size = 9.7
# v_fill_color = c(v_background_color, "#BDD7E7", "#6BAED6", "#2270B5")
# v_fill_color = c("#9e9ac8", "#807dba", "#6a51a3", "#54278f")

tb1 = tibble(x = 1:4,
             y = sample(20:100, 4))

ggplot(tb1, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  
  geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = x, y = 0, label = y), colour = "gray100",
            fontface = "bold") +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  scale_color_brewer(palette = "RdBu", direction = 1) +
  # scale_fill_manual(values = v_fill_color) +
  # scale_color_manual(values = v_fill_color) +
  xlim(c(-2, 5)) +
  scale_y_continuous(breaks = tb1$y,
                     limits = c(0, 100)) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


#E2071B #41E000 #00F0D3
ggsave("~/github/ggplot2/2022/20220826/save_ggplot_01.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 
1920/8
#############################################3


v_point_size = 4.2
v_background_color = "#334960"


tb2 = expand.grid(g = 1:6, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T))

ggplot(tb2, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = "gray100",
            # fontface = "bold", 
            size = 2.5, nudge_y = c(2,1.5,1.2,1),
            family = "BMJUAOTF") +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  scale_color_brewer(palette = "RdBu", direction = 1) +
  xlim(c(-2, 5)) +
  scale_y_continuous(breaks = tb1$y,
                     limits = c(0, 100)) +
  coord_polar(theta = "y") +
  facet_wrap(~g) + 
  labs(title = "ggplot2 - Ring Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        panel.spacing = unit(-0.3, "in"),
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.05, 
                                  size = 20,
                                  margin = margin(0.3,0,-0.2,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 10,
                                    margin = margin(-0.2,0,0.2,0,"in")),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


#E2071B #41E000 #00F0D3
ggsave("~/github/ggplot2/2022/20220826/save_ggplot_02.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 

