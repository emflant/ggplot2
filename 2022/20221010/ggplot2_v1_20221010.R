library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

tb1 = tibble(x = 1, y = c(68,405))
tb2 = tibble(x = 0, y = sum(tb1$y))
tb3 = tibble(x = 1, y = sum(tb1$y))
tb4 = tibble(x = 1, y = tb1$y[1], t = sum(tb1$y)) %>% 
  mutate(r = paste0(round(y / t * 100, 1), "%"))
tb4

ggplot(tb1) +
  geom_col(aes(x,y, fill = y)) +
  xlim(c(-3.5,1.5)) +
  coord_polar(theta = "y")


ggplot() +
  geom_col(data = tb1, aes(x,y, fill = y)) +
  geom_col(data = tb2, aes(x, y), width = 1) +
  coord_polar(theta = "y")

ggplot() +
  geom_col(data = tb2, aes(x, y), width = 1) +
  geom_col(data = tb3, aes(x, y), width = 0.3) +
  geom_col(data = tb4, aes(x, y), width = 0.7) +
  coord_polar(theta = "y") 


ggplot() +
  geom_col(data = tb2, aes(x, y), width = 1) +
  geom_col(data = tb3, aes(x, y), width = 0.3) +
  geom_col(data = tb4, aes(x, y), width = 0.7, colour = "gray100", size = 1) +
  geom_text(data = tb2, aes(x = -0.5,y = 0, label = y),
            colour = "gray100", size = 10) +
  coord_polar(theta = "y") 


v_donut_colour = "#DE735F"
v_line_colour = "#64676F"
v_font_colour = "#6B7BD6"



ggplot() +
  geom_col(data = tb2, aes(x, y), width = 1, fill = "gray100") +
  geom_col(data = tb3, aes(x, y), width = 0.3, fill = "gray90") +
  geom_col(data = tb4, aes(x, y), width = 0.7, 
           fill = "#DE735F") +
  geom_text(data = tb2, aes(x = -0.5,y = 0, label = y),
            colour = "#6B7BD6", size = 10) +
  coord_polar(theta = "y") +
  theme_dark()




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
