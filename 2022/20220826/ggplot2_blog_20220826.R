library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


v_point_size = 6.3
v_background_color = "#F5F8FB"# "#334960"
# v_background_color = "gray100"
v_palette = "YlOrBr"# "PuBuGn" # "PuBu" # "Blues" # "YlOrBr" # "PuOr" #"RdBu"
tb2 = expand.grid(g = 1:2, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T))


set.seed(58893)
tb2 = expand.grid(g = 1:2, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T))


tb1 = tibble(x = 1:4,
             y = sample(20:100, 4, replace = T)) 

tb1

#F5F8FB

ggplot(tb1, aes(x,y, fill= factor(x))) +
  geom_col() +
  theme_bw()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_11.png", 
       width = 8, height = 4.5, dpi = 120, units = "in", bg = "white") 


g1 = ggplot(tb1, aes(x,y, fill= factor(x))) +
  geom_col() +
  coord_polar(theta = "y") +
  lims(y = c(0, 100)) +
  theme_bw() + 
  theme(legend.position = "none")

g2 = ggplot(tb1, aes(x,y, fill= factor(x))) +
  geom_col() +
  coord_polar(theta = "y") +
  lims(x = c(-2, 5), y = c(0, 100)) +
  theme_bw() +
  theme(legend.position = "none")



g1 + g2

ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_12.png", 
       width = 8, height = 4, dpi = 120, units = "in", bg = "white") 


v_point_size = 9.7
ggplot(tb1, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  geom_point(aes(color = factor(x)), size = v_point_size) +
  coord_polar(theta = "y") +
  lims(x = c(-2, 4.5), y = c(0, 100)) +
  theme_bw()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_13.png", 
       width = 8, height = 6, dpi = 120, units = "in", bg = "white") 





#############################################

set.seed(58893)
tb2 = expand.grid(g = 1, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T)) %>% 
  mutate(c = c("gray0","gray0","gray0","gray100"))

tb2  

v_point_size = 10.3
v_background_color = "#334960"
# v_background_color = "gray100"
v_palette = "YlOrBr"# "PuBuGn" # "PuBu" # "Blues" # "YlOrBr" # "PuOr" #"RdBu"


ggplot(tb2, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = tb2$c,
            fontface = "bold", 
            size = 4.5, nudge_y = c(2,1.5,1.2,1),
            family = "BMJUAOTF") +
  scale_fill_brewer(palette = v_palette, direction = 1) +
  scale_color_brewer(palette = v_palette, direction = 1) +
  xlim(c(-2, 5)) +
  ylim(c(0,100)) +
  coord_polar(theta = "y") +
  # facet_wrap(~g) + 
  labs(title = "ggplot2 - Ring Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        panel.spacing = unit(-0.3, "in"),
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.3,0,-0.2,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(-0.2,0,0.2,0,"in")),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


#E2071B #41E000 #00F0D3
ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_00.png", 
       width = 8, height = 7, dpi = 120, units = "in", bg = v_background_color) 




#############################################

set.seed(58893)
tb2 = expand.grid(g = 1:2, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T)) %>% 
  mutate(c = c("gray0","gray0","gray0","gray100","gray0","gray0","gray0","gray100"))

tb2  

v_point_size = 5.9 
v_background_color = "#334960"
# v_background_color = "gray100"
v_palette = "YlOrBr"# "PuBuGn" # "PuBu" # "Blues" # "YlOrBr" # "PuOr" #"RdBu"


ggplot(tb2, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = tb2$c,
            fontface = "bold", 
            size = 3.8, nudge_y = c(2,1.5,1.2,1),
            family = "BMJUAOTF") +
  scale_fill_brewer(palette = v_palette, direction = 1) +
  scale_color_brewer(palette = v_palette, direction = 1) +
  xlim(c(-2, 5)) +
  ylim(c(0,100)) +
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
ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_01.png", 
       width = 8, height = 4.5, dpi = 120, units = "in", bg = v_background_color) 





v_point_size = 4.2 
set.seed(5881872)
tb2 = expand.grid(g = 1:6, x = 1:4) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T)) %>% 
  mutate(c = rep(c("gray0","gray0","gray0","gray100"), 6))

v_palette = "YlOrBr"# "PuBuGn" # "PuBu" # "Blues" # "YlOrBr" # "PuOr" #"RdBu"


ggplot(tb2, aes(x,y, fill= factor(x))) +
  geom_col() +
  geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = tb2$c,
            fontface = "bold", 
            size = 3, nudge_y = c(2,1.5,1.2,1),
            family = "BMJUAOTF") +
  scale_fill_brewer(palette = v_palette, direction = 1) +
  scale_color_brewer(palette = v_palette, direction = 1) +
  xlim(c(-2, 5)) +
  ylim(c(0,100)) +
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
ggsave("~/github/hugo/app/app-01/content/post/2022/20221004/images/20221004_02.png", 
       width = 8, height = 6, dpi = 120, units = "in", bg = v_background_color) 
