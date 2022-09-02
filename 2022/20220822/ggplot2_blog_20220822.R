

library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)

v_background_color = "#334960"
v_font_color = "gray100"

expand.grid(x = 1:10, y = 1:2)

set.seed(1001)
tb1 = tibble(x = 1:10, y = sample(10:60, 10), z = rep(letters[1:2], 5))

tb1


ggplot(tb1, aes(x,y)) +
  geom_col(aes(fill = z), width = 0.7)

ggsave("~/github/ggplot2/2022/20220822/save_ggplot_blog_01.png", 
       width = 8, height = 4.5, dpi = 240, units = "in")


ggplot(tb1, aes(x,y,fill = z)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  theme_bw()

ggsave("~/github/ggplot2/2022/20220822/save_ggplot_blog_02.png", 
       width = 8, height = 4.5, dpi = 240, units = "in")



ggplot(tb1, aes(x,y,fill = z)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = y), 
            colour = "gray100", vjust = 2, 
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  theme_bw()

ggsave("~/github/ggplot2/2022/20220822/save_ggplot_blog_03.png", 
       width = 8, height = 4.5, dpi = 240, units = "in")



ggplot(tb1, aes(x,y,fill = z)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2D4054", width = 0.7) +
  geom_col(width = 0.7) +
  geom_text(aes(label = y), 
            colour = "gray100", vjust = 2, 
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  theme_bw() + 
  theme(plot.background = element_rect(fill = v_background_color, color = v_background_color))





tibble(x = 1:10, y = sample(10:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2D4054", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$",y)), vjust = 2, colour = v_font_color,
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10],
                     expand = expansion(c(0.03,0.03))) +
  scale_y_continuous(expand = expansion(c(0.05,0.05))) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = v_font_color,
                                   size = 15),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220822/save_ggplot_blog_04.png", 
       width = 8, height = 4.5, dpi = 240, units = "in")


tibble(x = 1:10, y = sample(10:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2D4054", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$",y)), vjust = 2, colour = v_font_color,
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10]) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = v_font_color, size = 15),
        plot.background = element_rect(fill = v_background_color))

tibble(x = 1:10, y = sample(10:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2D4054", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$",y)), vjust = 2, colour = v_font_color,
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10],
                     expand = expansion(c(0.03,0.03))) +
  scale_y_continuous(expand = expansion(c(0.05,0.05))) +
  labs(title = "ggplot2 - Bar Chart",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = v_font_color,
                                   size = 15),
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.05, 
                                  size = 20,
                                  margin = margin(0.1,0,0.2,0,"in")),
        plot.caption = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    hjust = .96, 
                                    size = 10,
                                    margin = margin(0.2,0,0,0,"in")),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20220822/save_ggplot_blog_90.png", 
       width = 8, height = 4.5, dpi = 240, units = "in")



