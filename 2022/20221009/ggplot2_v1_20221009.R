library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


v_background_color = "#3D5773" #334960 #3D5773 #446180
v_palette = "GnBu"

tb1 = tibble(w = c(3,4,5,2,1),
             h = c(0,0,0,0,0)) %>% 
  mutate(x2 = cumsum(w)) %>% 
  mutate(x1 = lag(x2, 1, default = 0), .before = x2) %>% 
  mutate(x_pos = (x1 + x2) / 2) %>% 
  mutate(y1 = 0) %>% 
  mutate(y2 = h+5)

tb3 = tb1 %>% 
  mutate(y_pos = c(3, 2.8, 2.8, 3.2, 4)) %>% 
  mutate(text_size = c(10, 10, 10, 8,6)) %>%  
  mutate(text_colour = c("gray100","gray100","gray0","gray0","gray0"))  %>%
  mutate(rate = paste0(round(w / sum(w) * 100, 1), "%")) 


ggplot(tb3, aes(xmin = x1, 
                xmax = x2, 
                ymin = y1, 
                ymax = y2, 
                fill = factor(w, levels = c(1,2,5,4,3)))) +
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

ggsave("~/github/hugo/app/app-01/content/post/2022/20221009/images/20221009_01.png", 
       width = 8, height = 7, dpi = 120, units = "in")



#####################################################################


v_background_color = "#3D5773" #334960 #3D5773 #446180
v_palette = "GnBu"

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
  # scale_y_continuous(expand = expansion(c(0,0))) +
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
                                  margin = margin(0.5,0,-0.5,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(-1,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/hugo/app/app-01/content/post/2022/20221009/images/20221009_02.png", 
       width = 8, height = 7, dpi = 120, units = "in")


