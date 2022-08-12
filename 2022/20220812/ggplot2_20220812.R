library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)

v_size = 10

x = 1:v_size

set.seed(1)
y = sample(80:200, v_size)

z = logical(v_size)
# z = sample(c(T,F), v_size, replace = T, prob = c(0.1,0.9))
 z[c(1,8)] = T

tb = tibble(x, y, z)
tb

v_background_color = "black"

ggplot(tb, aes(x,y, fill = z)) +
  geom_chicklet(width = 0.7, colour = NA, radius = grid::unit(5, "pt")) + # "#424242"
  scale_fill_manual(values = c("#424242", "#94D4FB")) + #94D4FB #8AB2F9 #7D84F7
  scale_x_continuous(expand = expansion(c(0,0))) +
  theme_void() +
  labs(caption = "twitter @sourcebox7") +
  theme(legend.position = "none",
        plot.margin = margin(0.5,0.3,0.1,0.3, "in"),
        plot.caption = element_text(color = "gray70", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.1,0,0.3,0,"cm")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220812/save_ggplot_16x9_01.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")

