library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


tibble(x = 1:60, y = rep(1,60)) %>% 
  ggplot(aes(x,y)) +
  geom_col(width = 0.7) +
  coord_polar() +
  ylim(c(-2, 1))



v_size = 60
v_value = 38
# round(v_value / sum(v_value) * 100,0)

#F7C848 #888A8B
#88F9FD #888A8B

tibble(x = rep(1, v_size), 
       y = rep(1, v_size), 
       z = c(rep(1,v_value), rep(0, v_size-v_value)),
       l = rep(v_value, v_size)) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  geom_text(aes(x = -3, label = l),
            family = "BMJUAOTF",
            size = 20) +
  scale_fill_manual(values = c("#888A8B", "#F7C848")) +
  coord_polar(theta = "y") +
  xlim(-3, 1.5) +
  theme_void() +
  theme(legend.position = "none")





v_size = 12
v_value = 4

tibble(x = rep(1, v_size), 
       y = rep(1, v_size), 
       z = c(rep(1,v_value), rep(0, v_size-v_value)),
       l = rep(v_value, v_size)) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  geom_text(aes(x = -3, label = l),
            family = "BMJUAOTF",
            size = 20) +
  scale_fill_manual(values = c("#888A8B", "#F7C848")) +
  coord_polar(theta = "y") +
  xlim(-3, 1.5) +
  theme_void() +
  theme(legend.position = "none")



############################################################3



v_size1 = 5
v_count1 = 12
v_value1 = 4


tb1 = tibble(g = 1, x = rep(1, v_count1), 
       y = rep(v_size1, v_count1), 
       z = c(rep(1,v_value1), rep(0, v_count1-v_value1)),
       l = rep(v_value1, v_count1),
       l1 = rep("/ 12", v_count1))

v_size2 = 1
v_count2 = 60
v_value2 = 38

tb2 = tibble(g = 2, x = rep(1, v_count2), 
       y = rep(v_size2, v_count2), 
       z = c(rep(1,v_value2), rep(0, v_count2-v_value2)),
       l = rep(v_value2, v_count2),
       l1 = rep("/ 60", v_count2))



v_background_color = "#334960"
tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = v_background_color, size = 1) +
  geom_text(aes(x = -1.8, y = 45, label = l), 
            family = "BMJUAOTF", colour = "gray100",
            size = 20) +
  
  geom_text(aes(x = -1.5, y = 18, label = l1), 
            family = "BMJUAOTF", colour = "gray100",
            size = 10) +
  scale_fill_manual(values = c("#28384E", "#F7C848")) + #2E5E77 #888A8B
  coord_polar(theta = "y") +
  xlim(-3, 2.5) +
  facet_wrap(~g) +
  theme_void() +
  theme(legend.position = "none",
        panel.spacing = unit(-20,"mm"),
        strip.text = element_blank(),
        plot.background = element_rect(fill = v_background_color, color = NA))
  

ggsave("~/github/ggplot2/2022/20220902/save_ggplot_01.png", 
       width = 8, height = 6, dpi = 240, units = "in", bg = v_background_color) 




v_background_color = "#3A536E" #334960
tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = v_background_color, size = 1) +
  geom_text(aes(x = -3, y = 0, label = l), 
            family = "BMJUAOTF", colour = "gray100",
            size = 20) +
  scale_fill_manual(values = c("gray50", "#F7C848")) +  #415D7A  #4E7094 
  coord_polar(theta = "y") +
  xlim(-3, 2.5) +
  facet_wrap(~g) +
  labs(title = "ggplot2 - Progress Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        panel.spacing = unit(-0.5,"in"),
        strip.text = element_blank(),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0, 
                                  size = 20,
                                  margin = margin(0,0,0,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(0,0,0,0,"in")),
        plot.margin = margin(0,0.5,0,0.5,"in"),
        plot.background = element_rect(fill = v_background_color, color = NA))


ggsave("~/github/ggplot2/2022/20220902/save_ggplot_02.png", 
       width = 8, height = 5, dpi = 240, units = "in", bg = v_background_color) 
