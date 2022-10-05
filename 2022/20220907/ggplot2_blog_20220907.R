library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


v_size1 = 7
v_size2 = 4
v_background_color = "#3A536E" #334960
v_font_color = v_background_color


set.seed(84741)
tb2 = expand.grid(x = 1:v_size1, y = 1:v_size2) %>% 
  as_tibble() %>% 
  mutate(value = sample(1:100, v_size1*v_size2)) %>% 
  slice_sample(n = 9) %>%
  mutate(rank = rank(-value)) %>% 
  mutate(c1 = ifelse(rank <= 6, "gray100", "gray0"))
  
set.seed(84741)
tb1 = expand.grid(x = 1:7, y = 1:4) %>% 
  as_tibble() %>% 
  mutate(value = sample(1:100, 7*4)) %>% 
  slice_sample(n = 9)

ggplot(tb1, aes(x, y, fill = factor(value))) +
  geom_tile() +
  coord_fixed() 

ggsave("~/github/ggplot2/2022/20220907/save_ggplot_91.png", 
       width = 8, height = 4.5, dpi = 240, units = "in") 

ggplot(tb1, aes(x, y, fill = factor(value))) +
  geom_tile() +
  coord_fixed() +
  scale_fill_brewer(palette = "YlGnBu") 
  
ggsave("~/github/ggplot2/2022/20220907/save_ggplot_92.png", 
       width = 8, height = 4.5, dpi = 240, units = "in") 

ggplot(tb1, aes(x, y, fill = factor(value))) +
  geom_tile() +
  coord_fixed() +
  scale_fill_brewer(palette = "YlGnBu") +
  scale_y_continuous(limits = c(0.5,4.5)) +
  scale_x_continuous(breaks = 1:7, 
                     labels = LETTERS[1:7])
ggsave("~/github/ggplot2/2022/20220907/save_ggplot_93.png", 
       width = 8, height = 4.5, dpi = 240, units = "in") 

ggplot(tb1, aes(x, y, fill = factor(value))) +
  geom_tile() +
  geom_text(aes(label = value), size = 10) +
  coord_fixed() +
  scale_fill_brewer(palette = "YlGnBu") +
  # scale_y_continuous(limits = c(0.5,4.5)) +
  scale_x_continuous(breaks = 1:7, 
                     labels = LETTERS[1:7])
ggsave("~/github/ggplot2/2022/20220907/save_ggplot_94.png", 
       width = 8, height = 4.5, dpi = 240, units = "in") 


ggplot(tb2, aes(x, y, fill = factor(value))) +
  geom_tile(colour = v_background_color, size = 3) + 
  geom_text(aes(label = value), colour = tb2$c1,
            family = "BMJUAOTF", size = 10) +
  scale_fill_brewer(palette = "YlOrBr") +
  coord_fixed() +
  scale_y_continuous(limits = c(0.5,4.5)) +
  scale_x_continuous(breaks = 1:v_size1, 
                     labels = LETTERS[1:v_size1],
                     limits = c(0,7.5) ,
                     expand = expansion(c(0, 0))) +
  labs(title = "ggplot2 - geom_tile",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text = element_text(colour = "gray80", 
                                 face = "bold", size = 20),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0, 
                                  size = 20,
                                  margin = margin(0,0,0.2,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(0.4,0,0,0,"in")),
        plot.margin = margin(0.5,0,0.3,0,"in"),
        # aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = NA))


ggsave("~/github/ggplot2/2022/20220907/save_ggplot_95.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 



ggplot(tb2, aes(x, y, fill = factor(value))) +
  geom_tile(colour = "gray100", size = 5,
            linejoin = "round") + 
  geom_text(aes(label = value), colour = tb2$c1,
            family = "BMJUAOTF", size = 10) +
  scale_fill_brewer(palette = "YlGnBu") +
  coord_fixed() +
  scale_y_continuous(limits = c(0.5,4.5)) +
  scale_x_continuous(breaks = 1:v_size1, 
                     labels = LETTERS[1:v_size1],
                     limits = c(0,7.5) ,
                     expand = expansion(c(0, 0))) +
  labs(title = "ggplot2 - geom_tile",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(legend.position = "none",
        axis.text = element_text(colour = "gray80", 
                                 face = "bold", size = 20),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0, 
                                  size = 20,
                                  margin = margin(0,0,0.2,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(0.4,0,0,0,"in")),
        plot.margin = margin(0.5,0,0.3,0,"in"),
        # aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = NA))


ggsave("~/github/ggplot2/2022/20220907/save_ggplot_04.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 



