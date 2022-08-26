library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)

v_background_color = "#2E5E77"
v_background_color = "gray100"
tibble(x = 1:5, y = c(75, 64, 114, 36, 38),
       a = c(1,1,1,0,1)) %>% 
  mutate(z = y / 4 + 30) %>% 
  ggplot() + 
  geom_col(aes(x,z, fill = factor(x)), width = 1, colour = "gray100", size = 2) +
  geom_col(aes(x,y = 30), width = 1, fill = "gray100") +
  # geom_label(aes(x, y/10 + 30), label = LETTERS[1:5],
  #            label.size = 0, family = "BMJUAOTF",
  #            color = "black") +
  geom_text(aes(x, y/10 + 30 + a), label = LETTERS[1:5],
             family = "BMJUAOTF", size = 8,
             color = "gray100") +
  coord_polar(theta = "x") +
  # scale_fill_brewer(palette = "Purples",
  #                   direction = -1) +
  scale_fill_manual(values = c("#9E9AC8", "#9E9AC8", "#53278F", "#9E9AC8", "#9E9AC8")) +
  # theme_minimal() + #9E9AC8 #53278F
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        plot.caption = element_text(color = "gray0", 
                                    family = "Menlo", 
                                    hjust = 0.99, 
                                    size = 10,
                                    margin = margin(0.2,0,0,0,"cm")),
        # plot.margin = margin(0.5,0.5,0.3,0.4,"in"),
        # plot.margin = margin(0,0,0,0,"in"),
        # axis.text.x = element_text(),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))
# g1
ggsave("~/github/ggplot2/2022/20220824/save_ggplot_02.png", 
       width = 8, height = 6, dpi = 320, units = "in")
# tibble(x = 1:5, y = c(30, 30, 30, 30, 30), 
#        c = c(1,2,2,2,2)) %>% 
#   ggplot() + 
#   geom_col(aes(x,y, fill = factor(c)), 
#            width = c(1.1,1,1,1,1),
#            colour = c("gray100", NA, NA, NA, NA),
#            size = c(2,0,0,0,0))

tibble(x = 1:5, y = c(37, 30, 30, 30, 30), 
       c = c(1,2,2,2,2)) %>% 
  ggplot() + 
  geom_col(aes(x,y, fill = factor(c)), 
           width = c(1,1,1,1,1),
           colour = c("gray100", NA, NA, NA, NA),
           size = c(5,0,0,0,0)) +
  geom_col(aes(x,y = c(23,20,20,20,20)), width = 1, fill = "gray100") +
  # geom_label(aes(x, 25, label = x)) +
  coord_polar(theta = "x") +
  ylim(c(-10, 37)) +
  # scale_fill_brewer(direction = -1) +
  scale_fill_manual(values = c("#7b59b3", "#A67EE2")) +
  theme_void() +
  theme(legend.position = "none",
        plot.caption = element_text(color = "gray0", 
                                    family = "Menlo", 
                                    hjust = 0.99, 
                                    size = 10,
                                    margin = margin(0.2,0,0,0,"cm")),
        plot.margin = margin(0.5,0.5,0.3,0.4,"in"))


tibble(x = c(1,3.5), y = c(1.5,1), w = c(1,4), h = c(1,1))  %>% 
  ggplot(aes(x,y, fill = factor(x))) + 
  geom_tile(aes(width = w, height= h), size = 5, colour = "gray0")


tibble(w = c(1,5,2), y = rep(1,3)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = (w1 - w2) / 2)

##################################################
v_background_color = "#2E5E77"
v_background_color = "gray100"
tibble(w = c(1,4),
       h = rep(1,2),
       y = c(1.3,1),
       a = c(0.03,-0.06)) %>% 
  mutate(w1 = cumsum(w)) %>% 
  mutate(w2 = lag(w1, 1, default = 0)) %>% 
  mutate(x = w / 2 + w2) %>% 
  ggplot(aes(x,y,
             width = w, height = h,
             fill = factor(x))) +
  geom_tile(size = 3, colour = v_background_color) +
  geom_text(aes(x,y + a), label = LETTERS[1:2],
            family = "BMJUAOTF", size = 8,
            colour = "gray100") +
  ylim(c(-2, 2)) +
  coord_polar(theta = "x") +
  scale_fill_manual(values = c("#53278F", "#9E9AC8")) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220824/save_ggplot_01.png", 
       width = 8, height = 6, dpi = 320, units = "in")
##################################################




g1 + g2
ggsave("~/github/ggplot2/2022/20220824/save_ggplot_union_01.png", 
       width = 8, height = 4, dpi = 320, units = "in")

# tibble(x = c(1,3.5), y = c(1.3,1), w = c(1,4), h = c(1,1)) %>% 
#   ggplot(aes(x,y, fill = factor(x))) + 
#   geom_tile(aes(width = w, height= h), size = 2, colour = "gray0")  +
#   ylim(c(-2, 2)) +
#   scale_fill_brewer(direction = -1) +
#   coord_polar(theta = "x") +
#   theme_void()
#   
# 
# 
# v_height = 0.3
# 
# tibble(x = c(1,2), y = c(1 + v_height,1), w = c(1,4), h = c(1,1)) %>% 
#   ggplot(aes(x,y, fill = factor(x))) + 
#   geom_tile(aes(width = w, height= h), size = 2, colour = "gray100") +
#   coord_polar(theta = "x")
#   coord_fixed()
# 
# tibble(x = c(1,3.5), y = c(1 + v_height,1), w = c(1,2), h = c(1,1)) %>% 
#   ggplot(aes(x,y, fill = factor(x))) + 
#   geom_tile(aes(width = w, height= h), size = 2, colour = "gray100")  +
#   ylim(c(-2, 2)) +
#   scale_fill_brewer(direction = -1) +
#   coord_polar(theta = "x") +
#   theme_void()
# 
# 
# 
#   geom_col(aes(x,y = c(25,20,20,20,20)), width = 1, fill = "gray100") +
#   # geom_label(aes(x, 25, label = x)) +
#   coord_polar(theta = "x") +
#   scale_fill_brewer() +
#   theme_void() +
#   theme(legend.position = "none",
#         plot.caption = element_text(color = "gray0", 
#                                     family = "Menlo", 
#                                     hjust = 0.99, 
#                                     size = 10,
#                                     margin = margin(0.2,0,0,0,"cm")),
#         plot.margin = margin(0.5,0.5,0.3,0.4,"in"))
# 

