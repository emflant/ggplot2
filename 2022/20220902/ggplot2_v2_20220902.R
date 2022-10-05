library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)


v_size = 60
v_value = 38
c(rep(1,v_value), rep(0, v_size-v_value))

tibble(x = rep(1, v_size), 
       y = rep(1, v_size), 
       z = c(rep(1,v_value), rep(0, v_size-v_value)),
       l = v_value)

list(rep(1,v_value), rep(0, v_size-v_value)) %>% 
  unlist()

append(rep(1,v_value), rep(0, v_size-v_value))

tb_hour = tibble(x = rep(1,12),
                 y = rep(1,12),
                 z = c(rep(1,4), rep(0,12-4)))
tb_hour %>% print(n = Inf)


ggplot(tb_hour, aes(x, y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1)

ggsave("~/github/ggplot2/2022/20220902/21.png", 
       width = 8, height = 4.5, dpi = 120, units = "in") 


ggplot(tb_hour, aes(x, y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  coord_polar(theta = "y")

ggsave("~/github/ggplot2/2022/20220902/20221002_22.png", 
       width = 6, height = 4.5, dpi = 120, units = "in") 






ggplot(tb_hour, aes(x, y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  coord_polar(theta = "y") +
  xlim(-3, 1.5)

ggsave("~/github/ggplot2/2022/20220902/20221002_23.png", 
       width = 6, height = 4.5, dpi = 180, units = "in") 



########################################################



########################################################


v_background_color = "#3A536E" #334960
v_size1 = 5
v_count1 = 12
v_value1 = 4


tb1 = tibble(g = 1, x = rep(1, v_count1), 
       y = rep(v_size1, v_count1), 
       z = c(rep(1,v_value1), rep(0, v_count1-v_value1)),
       n = 1:v_count1) %>% 
  mutate(l = ifelse(n == 1, v_value1, NA))

tb1
v_size2 = 1
v_count2 = 60
v_value2 = 38

tb2 = tibble(g = 2, x = rep(1, v_count2), 
       y = rep(v_size2, v_count2), 
       z = c(rep(1,v_value2), rep(0, v_count2-v_value2)),
       n = 1:v_count2) %>% 
  mutate(l = ifelse(n ==1, v_value2, NA))



tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = v_background_color, size = 1) +
  geom_text(aes(x = -3, y = 0, label = l), 
            family = "BMJUAOTF", colour = "gray100",
            size = 20, na.rm = T) +
  #F7C848 C7F2A4  56BBF1  5EE6EB FC92E3 F9FD50
  scale_fill_manual(values = c("gray50", "#C7F2A4")) +  
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


ggsave("~/github/ggplot2/2022/20220902/20221002_01.png", 
       width = 8, height = 5, dpi = 120, units = "in", bg = v_background_color) 


tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  xlim(-3, 1.5) +
  coord_polar(theta = "y") +
  facet_wrap(~g) +
  theme(legend.position = "none")

ggsave("~/github/ggplot2/2022/20220902/20221002_02.png", 
       width = 8, height = 4.5, dpi = 120, units = "in") 


tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = "gray100", size = 1) +
  xlim(-3, 1.5) +
  coord_polar(theta = "y") +
  facet_wrap(~g) +
  scale_fill_manual(values = c("gray50", "#F7C848")) +
  theme_light() +
  theme(legend.position = "none")


ggsave("~/github/ggplot2/2022/20220902/20221002_03.png", 
       width = 8, height = 4.5, dpi = 120, units = "in") 


tb1


tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = v_background_color, size = 1) +
  geom_text(aes(x = -3, y = 0, label = l),
            family = "BMJUAOTF", colour = "gray100",
            size = 20, na.rm = T) +
  #F7C848 C7F2A4  56BBF1  5EE6EB FC92E3 F9FD50
  scale_fill_manual(values = c("gray50", "#F7C848")) +  
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

ggsave("~/github/ggplot2/2022/20220902/20221002_04.png", 
       width = 8, height = 5, dpi = 120, units = "in", bg = v_background_color) 





tb1 %>% union_all(tb2) %>% 
  ggplot(aes(x,y, fill = factor(z))) +
  geom_col(colour = v_background_color, size = 1) +
  geom_text(aes(x = -3, y = 0, label = l),
            family = "BMJUAOTF", colour = "gray100",
            size = 20, na.rm = T) +
  #F7C848 C7F2A4  56BBF1  5EE6EB FC92E3 F9FD50
  scale_fill_manual(values = c("gray50", "#5EE6EB")) +  
  coord_polar(theta = "y") +
  xlim(-3, 2.5) +
  facet_wrap(~g) +
  # labs(title = "ggplot2 - Progress Chart",
  #      caption = "twitter @sourcebox7") +
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



ggsave("~/github/ggplot2/2022/20220902/20221002_00.png", 
       width = 8, height = 5, dpi = 120, units = "in", bg = v_background_color) 


