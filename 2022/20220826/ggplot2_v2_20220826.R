library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

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
ggsave("~/github/ggplot2/2022/20220826/save_ggplot_03.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 

