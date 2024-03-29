library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
v_font_color = c("gray90","#334960","#334960","#334960","gray90")

g1 = tibble(x = 1:5, y = c(75, 64, 114, 36, 38),
       a = c(1,1,1,0,1)) %>% 
  mutate(z = y / 4 + 30) %>% 
  ggplot() + 
  geom_col(aes(x,z, fill = factor(x)), width = 1, 
           colour = v_background_color, size = 2) +
  geom_col(aes(x,y = 30), width = 1, fill = v_background_color) +
  geom_text(aes(x, y/10 + 30 + a), label = LETTERS[1:5],
             family = "BMJUAOTF", size = 7,
             color = v_font_color) +
  coord_polar(theta = "x") +
  # "#9F9EBF", "#7070A2"
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none")

# g1
g2 = tibble(w = c(2,4,1,2,1),
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
            family = "BMJUAOTF", size = 7,
            colour = v_font_color) +
  ylim(c(-1.5, 2)) +
  coord_polar(theta = "x") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  
  theme_void() +
  theme(legend.position = "none")

##################################################

#2E5E77

plot_spacer() +
  theme(plot.background = element_rect(fill = NA, color = NA)) +
  inset_element(g1, left = -0.2, bottom = -0.1, right = 0.75, top = 1.1) +
  inset_element(g2, left = 0.45, bottom = 0, right = 1, top = 1) +
  # theme(plot.background = element_rect(fill = v_background_color, color = NA)) +
  plot_annotation(caption = "twitter @sourcebox7",
                  title = "ggplot2 - Donut Chart",
                  theme = theme(plot.title = element_text(color = "gray90", 
                                                          family = "Menlo", 
                                                          face = "bold",
                                                          hjust = 0.05, 
                                                          size = 20,
                                                          margin = margin(0.3,0,-0.3,0,"in")),
                                plot.caption = element_text(color = "gray90", 
                                                            family = "Menlo", 
                                                            hjust = .95, 
                                                            size = 10,
                                                            margin = margin(-0.3,0,0.3,0,"in")),
                                plot.margin = margin(0,0,0,0,"in"),
                                plot.background = element_rect(fill = v_background_color, color = NA)
                                ))


ggsave("~/github/ggplot2/2022/20220824/save_ggplot_union_04.png", 
       width = 8, height = 4.5, dpi = 240, units = "in", bg = v_background_color) 

