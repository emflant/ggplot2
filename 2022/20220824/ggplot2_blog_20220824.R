library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
v_font_color = c("gray100","#334960","#334960","#334960","gray100")
v_palette = "RdBu"
v_direction = -1

tibble(w = c(2,4,1,2,1),
            h = rep(1,5),
            y = c(1.4,1,1,1,1),
            a = c(0.03,-0.06,0,0,0.04)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  ggplot(aes(x,y,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 3, colour = v_background_color) +
  geom_text(aes(x,y + a), label = LETTERS[1:5],
            family = "BMJUAOTF", size = 10,
            colour = v_font_color) +
  ylim(c(-1.5, 2)) +
  coord_polar(theta = "x") +
  scale_fill_brewer(palette = v_palette, direction = v_direction) +
  labs(title = "ggplot2 - Donut Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(-0.7,0,0.5,0,"in")),
        plot.margin = margin(0,0,0,0,"in"),
        plot.background = element_rect(fill = v_background_color, color = NA))



ggsave("~/github/ggplot2/2022/20220824/save_ggplot_blog_01.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 



############################################################################

v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
v_font_color = c("gray90","#334960","#334960","#334960","gray90")
v_palette = "RdBu"
v_direction = -1


tb1 = tibble(w = c(2,4,1,2,1), # 직사각형의 가로 길이
       h = rep(1,5), # 각 직사각형의 높이는 1로 기본셋팅.
       y = c(1.4,1,1,1,1)) %>% # 조각들의 높이값, 띄울게 있으면 1보다 크게 설정.
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2)

tb1
 
ggplot(tb1, aes(x, y, width = w, 
                height = h, 
                fill = factor(x))) +
  geom_tile() +
  coord_fixed() +
  scale_x_continuous(breaks = tb1$w1) +
  scale_y_continuous(breaks = c(0,0.9, 1.5,1.9)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("~/github/ggplot2/2022/20220824/save_ggplot_blog_02.png", 
       width = 8, height = 1.5, dpi = 240, units = "in") 


ggplot(tb1, aes(x, y, width = w, 
                height = h, 
                fill = factor(x))) +
  geom_tile() +
  coord_fixed() +
  scale_x_continuous(breaks = tb1$x) +
  scale_y_continuous(breaks = tb1$y) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("~/github/ggplot2/2022/20220824/save_ggplot_blog_03.png", 
       width = 8, height = 1.5, dpi = 240, units = "in") 


ggplot(tb1, aes(x, y, width = w, 
                height = h, 
                fill = factor(x))) +
  geom_tile() +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("~/github/ggplot2/2022/20220824/save_ggplot_blog_04.png", 
       width = 8, height = 6, dpi = 240, units = "in") 




ggplot(tb1, aes(x, y, width = w, 
                height = h, 
                fill = factor(x))) +
  geom_tile(colour = "gray100", size = 2) +
  coord_polar() +
  ylim(c(-3,2)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

ggsave("~/github/ggplot2/2022/20220824/save_ggplot_blog_05.png", 
       width = 8, height = 6, dpi = 240, units = "in") 

