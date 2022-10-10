library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


tb1 = tibble(w = c(5,4,3,2,1)) %>% 
  mutate(x2 = cumsum(w)) %>% 
  mutate(x1 = lag(x2, 1, default = 0), .before = x2) %>% 
  mutate(x_pos = (x1 + x2) / 2) %>% 
  mutate(y1 = 0) %>% 
  mutate(y2 = w) %>% 
  mutate(y3 = w/4 + 4) 

tb1

#######################################

ggplot(tb1, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y2, 
                fill = factor(w))) +
  geom_rect() +
  coord_fixed()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_11.png", 
       width = 8, height = 2.6, dpi = 120, units = "in")   

ggplot(tb1, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y3, 
                fill = factor(w))) +
  geom_rect() +
  coord_fixed()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_12.png", 
       width = 8, height = 2.6, dpi = 120, units = "in")   

#######################################

ggplot(tb1, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y2, 
                fill = factor(w))) +
  geom_rect() +
  coord_polar()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_13.png", 
       width = 6, height = 5, dpi = 120, units = "in")

ggplot(tb1, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y3, 
                fill = factor(w))) +
  geom_rect() +
  coord_polar()

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_14.png", 
       width = 6, height = 5, dpi = 120, units = "in")


ggplot(tb1, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = factor(w))) +
  geom_rect() +
  coord_polar() + 
  theme(legend.position = "none") +
  ggplot(tb1, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y3, fill = factor(w))) +
  geom_rect() +
  coord_polar() +
  theme(legend.position = "none")

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_15.png", 
       width = 8, height = 4, dpi = 120, units = "in")

#######################################


tb2 = tb1 %>% 
  mutate(y_pos = c(3,3,3,3,3.2)) %>% 
  mutate(text_size = c(10,8,6,5,4)) %>% 
  mutate(rate = paste0(round(w / sum(w) * 100, 1), "%")) 
  

ggplot(tb2, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y3, 
                fill = factor(w))) +
  geom_rect() +
  geom_text(aes(x = x_pos, 
                y = y_pos, 
                label = rate), 
            size = tb2$text_size) +
  coord_polar() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_16.png", 
       width = 6, height = 5, dpi = 120, units = "in")

#######################################

v_background_color = "#334960" 
# v_palette = "YlOrBr"
v_palette = "GnBu"

tb3 = tb1 %>% 
  mutate(text_height = c(3,3,3,3,3.5)) %>% 
  mutate(text_size = c(15,10,8,7,6)) %>% 
  mutate(text_size_6 = c(10,8,6,5,4)) %>% 
  mutate(text_colour = c("gray100","gray100","gray0","gray0","gray0"))  %>%
  mutate(rate = paste0(round(w / sum(w) * 100, 1), "%")) 

ggplot(tb3, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y3, 
                fill = factor(w))) +
  geom_rect() +
  geom_text(aes(x = x_pos, 
                y = text_height, 
                label = rate), 
            size = tb3$text_size,
            colour = tb3$text_colour,
            family = "BMJUAOTF") +
  coord_polar() +
  scale_fill_brewer(palette = v_palette) +
  labs(title = "ggplot2 - Pie Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 12,
                                    margin = margin(-0.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_23.png", 
       width = 8, height = 7, dpi = 120, units = "in")

#######################################
v_background_color = "#3D5773" #334960 #3D5773 #446180
v_palette = "GnBu"
tb3 = tb1 %>% 
  mutate(y_pos = c(2.8, 3, 3.2, 3.4, 4)) %>% 
  mutate(text_size = c(15,10,8,7,6)) %>% 
  mutate(text_size_6 = c(10,8,6,5,4)) %>% 
  mutate(text_colour = c("gray100","gray100","gray0","gray0","gray0"))  %>%
  mutate(rate = paste0(round(w / sum(w) * 100, 1), "%")) 

ggplot(tb3, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = 5, 
                fill = factor(w))) +
  geom_rect() +
  geom_text(aes(x = x_pos, 
                y = y_pos, 
                label = rate), 
            size = tb3$text_size,
            colour = tb3$text_colour,
            family = "BMJUAOTF") +
  coord_polar() +
  scale_fill_brewer(palette = v_palette) +
  labs(title = "ggplot2 - Pie Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 12,
                                    margin = margin(-0.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_31.png", 
       width = 8, height = 7, dpi = 120, units = "in")



factor(c(30,40,50,20,10))
factor(c(30,40,50,20,10), levels = c(10,20,50,40,30))
factor(letters[1:20], labels = "letter")

tb1 = tibble(w = c(3,4,5,2,1),
             h = c(1,0,0,0,0)) %>% 
  mutate(x2 = cumsum(w)) %>% 
  mutate(x1 = lag(x2, 1, default = 0), .before = x2) %>% 
  mutate(x_pos = (x1 + x2) / 2) %>% 
  mutate(y1 = 0) %>% 
  mutate(y2 = h+5)

tb3 = tb1 %>% 
  mutate(y_pos = c(4, 2.8, 2.5, 3.4, 4)) %>% 
  mutate(text_size = c(13, 10, 10, 8,6)) %>%  
  mutate(text_colour = c("gray100","gray100","gray0","gray0","gray0"))  %>%
  mutate(rate = paste0(round(w / sum(w) * 100, 1), "%")) 


ggplot(tb3, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y2, 
                fill = factor(w, levels = c(1,2,5,4,3)))) +
  geom_rect(size = c(12, NA, NA, NA, NA), colour = v_background_color) +
  geom_text(aes(x = x_pos, 
                y = y_pos, 
                label = rate), 
            size = tb3$text_size,
            colour = tb3$text_colour,
            family = "BMJUAOTF") +
  coord_polar() +
  scale_fill_brewer(palette = v_palette) +
  labs(title = "ggplot2 - Pie Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.0, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .95, 
                                    size = 12,
                                    margin = margin(-0.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/hugo/app/app-01/content/post/2022/20221006/images/20221006_32.png", 
       width = 8, height = 7, dpi = 120, units = "in")


