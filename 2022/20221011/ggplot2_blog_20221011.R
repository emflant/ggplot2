library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)



# set.seed(83974)
# tb1 = tibble(value = sample(20:100, 10)) %>% 
#   arrange(desc(value)) %>% 
#   mutate(x = 1:10, y = value)


tb1 = tibble(x = 1:10,
             y = c(115, 110, 88, 75, 63, 
                   43, 35, 28, 25, 20))
tb1
ggplot(tb1, aes(x, y, fill = factor(1))) +
  geom_col(width = 1, size = 1, colour = "white") +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/app-01/content/post/2022/20221016/images/20221016_11.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1, aes(x, y, fill = factor(1))) +
  geom_col(width = 1, size = 1, colour = "white") +
  coord_polar() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/app-01/content/post/2022/20221016/images/20221016_12.png", 
       width = 6, height = 5, dpi = 120, units = "in")

ggplot(tb1, aes(x, y, fill = factor(1))) +
  geom_col(width = 1, size = 1, colour = "white") +
  coord_polar() +
  ylim(c(-50, 120)) +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/app-01/content/post/2022/20221016/images/20221016_13.png", 
       width = 6, height = 5, dpi = 120, units = "in")


v_background_color = "#334960" #334960 #3D5773 #446180
ggplot(tb1, aes(x, y)) +
  geom_col(width = 1, fill = "#F0E180",
           colour = v_background_color, size = 3) +
  geom_text(aes(label = y),
            position = position_stack(vjust = 0.5),
            colour = v_background_color,
            family = "BMJUAOTF", 
            fontface = "bold",
            size = c(11,10,9,8,7,6,5,4,4,4)) +
  ylim(c(-50, 120)) +
  coord_polar() +
  labs(title = "ggplot2 - Donut Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(-1.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/hugo/app/app-01/content/post/2022/20221016/images/20221016_01.png", 
       width = 8, height = 7, dpi = 120, units = "in")









v_background_color = "gray100" #334960 #3D5773 #446180
ggplot(tb1, aes(x, y)) +
  geom_col(aes(fill = factor(y)), width = 1, 
           colour = v_background_color, size = 3) +
  geom_text(aes(label = y),
            position = position_stack(vjust = 0.5),
            colour = c("gray100","gray100","gray100","gray20","gray20",
                       "gray20","gray100","gray100","gray100","gray100"),
            family = "BMJUAOTF", 
            fontface = "bold",
            size = c(11,10,9,8,7,6,5,4,4,4)) +
  scale_fill_brewer(palette = "RdYlBu") +
  ylim(c(-50, 120)) +
  coord_polar() +
  labs(title = "ggplot2 - Donut Chart",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(color = "gray30", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.5,0,-0.3,0,"in")),
        plot.caption = element_text(color = "gray30", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(-1.5,0,0.5,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = "gray30", size = 1))


ggsave("~/github/hugo/app/app-01/content/post/2022/20221016/images/20221016_02.png", 
       width = 8, height = 7, dpi = 120, units = "in", )


