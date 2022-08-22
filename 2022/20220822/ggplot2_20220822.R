

library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)

LETTERS[1:10]
#815DB9 #A67EE2 
#F1D566 #E4806D  E39788
v_background_color = "#2E5E77"

set.seed(1001)
tibble(x = 1:10, y = sample(20:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2B576E", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$",y)), vjust = 2, colour = "gray100",
            family = "BMJUAOTF", size = 5) +
  # scale_fill_manual(values = c("#A67EE2", "#815DB9")) +
  # scale_fill_manual(values = c("#A1ACC0", "#7686A3")) +
  scale_fill_manual(values = c("#9F9EBF", "#7070A2")) +
  # scale_fill_manual(values = c("#BB66F7", "#9F29F6")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10]) +
  scale_y_continuous(expand = expansion(c(0.05,0.05))) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "gray100",
                                   size = 15),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20220822/save_ggplot_01.png", 
       width = 8, height = 4, dpi = 320, units = "in")




#################################################

tibble(x = 1:10, y = sample(20:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), fill = "#2B576E", width = 0.7) +
  # geom_col(aes(fill = z), width = 0.7) +
  # geom_text(aes(label = paste0("$",y)), vjust = 2, colour = "gray100",
  #           family = "BMJUAOTF", size = 5) +
  # scale_fill_manual(values = c("#A67EE2", "#815DB9")) +
  # scale_fill_manual(values = c("#F0926B", "#ED6232")) +
  # scale_fill_manual(values = c("#F6C471", "#F3AA41")) +
  # scale_fill_manual(values = c("#BB66F7", "#9F29F6")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10]) +
  scale_y_continuous(expand = expansion(c(0.05,0.05))) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "gray100",
                                   size = 15),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220822/save_ggplot_background.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")

#################################################

tibble(x = 1:10, y = sample(20:60, 10), z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  # geom_col(aes(x = 1:10, y = 60), fill = "#2B576E", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$",y)), vjust = 2, colour = "gray100",
            family = "BMJUAOTF", size = 5) +
  scale_fill_manual(values = c("#A67EE2", "#815DB9")) +
  scale_x_continuous(breaks = 1:10, labels = LETTERS[1:10]) +
  scale_y_continuous(expand = expansion(c(0.05,0.05))) +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text.x = element_text(colour = "gray100",
                                   size = 15),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"))


ggsave("~/github/ggplot2/2022/20220822/save_ggplot_bar.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")


#################################################


tibble(x = 1:10, 
       y = sample(20:60, 10), 
       z = rep(letters[1:2], 5)) %>% 
  ggplot(aes(x,y)) +
  geom_col(aes(x = 1:10, y = 60), 
           fill = "gray90", width = 0.7) +
  geom_col(aes(fill = z), width = 0.7) +
  geom_text(aes(label = paste0("$", y)), vjust = 2) +
  scale_x_continuous(breaks = 1:10, 
                     labels = LETTERS[1:10]) +
  theme_void() +
  theme(legend.position = "none",
        axis.text.x = element_text())

