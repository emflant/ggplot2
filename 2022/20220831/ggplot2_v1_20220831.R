library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

v_div = 10
v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
v_palette = "RdBu"
v_direction = -1

tibble(w = c(2,4,1,2,1),
       h = c(1,1,1,1,1),
       y = c(1,1,1,1,1)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  mutate(a = w / 10) %>% 
  mutate(y1 = a/2 + h, h = a+h) %>% 
  ggplot(aes(x,y1,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 2, colour = v_background_color) +
  scale_fill_brewer(palette = v_palette, direction = v_direction) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220831/save_ggplot_01.png", 
       width = 8, height = 6, dpi = 240, units = "in") 

v_size = c(14,10,8,6,4) # 톤프사이즈
text_color = c("gray100",v_background_color,v_background_color,"gray100","gray100")

tibble(w = c(5,4,3,2,1),
       h = c(1,1,1,1,1),
       y = c(1,1,1,1,1),
       text_height = c(1.25,1.20,1.15,1.2,1.4)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  mutate(a = w / 10) %>% # 파이차트 높이 조정. 그냥 w 값을 쓰면 너무 편차가 커짐.
  mutate(y1 = a/2 + h, h = a+h) %>%  # 계산완료된 y 최종값. 
  mutate(s = w * 2) %>% # s : 글시크기.
  mutate(text = paste0(round(w / sum(w) * 100, 1), "%")) %>% 
  ggplot(aes(x,y1,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 2, colour = v_background_color) +
  geom_text(aes(y = text_height, label = text), 
            colour = text_color,
            size = v_size, family = "BMJUAOTF") +
  scale_fill_brewer(palette = v_palette, direction = v_direction) +
  coord_polar() +
  # ylim(c(0.1,2)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220831/save_ggplot_02.png", 
       width = 8, height = 7, dpi = 240, units = "in") 

################################################
################################################
################################################

v_size = c(14,10,10,8,6) # 톤프사이즈
text_color = c("gray100",v_background_color,v_background_color,v_background_color,"gray100")


tibble(w = c(5,4,3,2,1),
       h = c(1,1,1,1,1),
       y = c(1,1,1,1,1),
       text_height = c(1.25,1.20,1.15,1.2,1.4),
       group_text = rep("A",5)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  mutate(a = w / 10) %>% # 파이차트 높이 조정. 그냥 w 값을 쓰면 너무 편차가 커짐.
  mutate(y1 = a/2 + h, h = a+h) %>%  # 계산완료된 y 최종값. 
  mutate(s = w * 2) %>% # s : 글시크기.
  mutate(text = paste0(round(w / sum(w) * 100, 1), "%")) %>% 
  ggplot(aes(x,y1,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 2, colour = v_background_color) +
  geom_text(aes(y = text_height, label = text), 
            colour = text_color,
            size = v_size, family = "BMJUAOTF") +
  geom_text(aes(y = 0, label = group_text), 
            colour = "gray100",
            size = max(v_size), family = "BMJUAOTF")  +
  scale_fill_brewer(palette = v_palette, direction = v_direction) +
  coord_polar() +
  ylim(c(0,2)) +
  labs(title = "ggplot2 - Pie Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.3,0,-0.2,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 10,
                                    margin = margin(-0.2,0,0.2,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220831/save_ggplot_03.png", 
       width = 8, height = 7, dpi = 240, units = "in") 

################################################
################################################
################################################


v_size = c(14,10,10,8,6) # 톤프사이즈
text_color = c("gray100",v_background_color,v_background_color,v_background_color,"gray100")


tibble(w = c(5,4,3,2,1),
       h = c(1,1,1,1,1),
       y = c(1,1,1,1,1),
       text_height = c(1.35,1.30,1.25,1.3,1.45),
       group_text = rep("A",5)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  mutate(a = w / 10) %>% # 파이차트 높이 조정. 그냥 w 값을 쓰면 너무 편차가 커짐.
  mutate(y1 = a/2 + h, h = a+h) %>%  # 계산완료된 y 최종값. 
  mutate(s = w * 2) %>% # s : 글시크기.
  mutate(text = paste0(round(w / sum(w) * 100, 1), "%")) %>% 
  ggplot(aes(x,y1,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 2, colour = v_background_color) +
  geom_text(aes(y = text_height, label = text), 
            colour = text_color,
            size = v_size, family = "BMJUAOTF") +
  # geom_text(aes(y = 0, label = group_text), 
  #           colour = "gray100",
  #           size = max(v_size), family = "BMJUAOTF")  +
  scale_fill_brewer(palette = v_palette, direction = v_direction) +
  coord_polar() +
  # ylim(c(0,2)) +
  labs(title = "ggplot2 - Pie Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.3,0,-0.2,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 10,
                                    margin = margin(-0.2,0,0.2,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220831/save_ggplot_04.png", 
       width = 8, height = 7, dpi = 240, units = "in") 
