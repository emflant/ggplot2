library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)



set.seed(83974)
tb1 = tibble(value = sample(20:100, 10)) %>% 
  arrange(desc(value)) %>% 
  mutate(x = 1:10, y = value)


tb1 = tibble(x = 1:10,
             y = c(115, 110, 88, 75, 63, 43, 35, 28, 25, 20))

ggplot(tb1, aes(x, y)) +
  geom_col(width = 1, fill = "#F0E180",
           colour = v_background_color, size = 3) +
  geom_text(aes(label = y),
            position = position_stack(vjust = 0.5),
            colour = v_background_color,
            family = "BMJUAOTF", 
            fontface = "bold",
            size = c(11,10,9,8,7,6,5,4,4,4)) +
  ylim(c(-50, 120)) +
  coord_polar() +
  labs(title = "ggplot2 - Donut Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(-1.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20221011/save_01.png", 
       width = 8, height = 7, dpi = 240, units = "in")


v_background_color = "#3D5773" #334960 #3D5773 #446180
v_palette = "PuBuGn"



ggplot() +
  geom_col(data = tb2, aes(x, y), width = 1, fill = "gray100") +
  geom_col(data = tb3, aes(x, y), width = 0.3, fill = "gray100",
           alpha = 0.3) +
  geom_col(data = tb4, aes(x, y), width = 0.7, 
           fill = "#DE735F") +
  geom_text(data = tb2, aes(x = -0.5,y = 0, label = y),
            colour = "#6B7BD6", size = 20,
            family = "BMJUAOTF") +
  # geom_text(data = tb4, aes(x, y, label = y),
  #           position = position_stack(vjust = 0.5),
  #           family = "BMJUAOTF",
  #           size = 15) +
  geom_text(data = tb4, aes(x, y, label = r),
            position = position_stack(vjust = 0.5),
            colour = "gray100",
            family = "BMJUAOTF", 
            fontface = "bold",
            size = 11) +
  coord_polar(theta = "y") +
  labs(title = "ggplot2 - Donut Chart",
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
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(-0.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))



ggsave("~/github/ggplot2/2022/20221010/save_01.png", 
       width = 8, height = 7, dpi = 240, units = "in")
