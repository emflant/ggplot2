library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)

# rnorm(10)
# runif(10)

##################################################
v_xsize = 52
v_ysize = 7
v_width = 0.8
expand.grid(x = 1:v_xsize, y = 1:v_ysize) %>% 
  as_tibble() %>% 
  mutate(z = rnorm(v_xsize * v_ysize)) %>% 
  ggplot(aes(x,y,fill = z)) +
  geom_tile(width = v_width, height = v_width)+
  coord_fixed() +
  # scale_fill_brewer() +
  theme_void() +
  theme(legend.position = "none")

##################################################

v_xsize = 52
v_ysize = 7
v_width = 0.8

# seq.Date(ymd("2022-01-01"), ymd("2022-12-31"), by = "day")
# tibble(x = seq.Date(ymd("2022-01-01"), ymd("2022-12-31"), by = "day"),
#        y = wday(x, label = T, abbr = T),
#        z = runif(365)) %>% 
#   ggplot(aes(x, y, fill = z)) +
#   geom_tile(width = v_width, height = v_width)

tb1 = tibble(date = seq.Date(ymd("2021-12-26"), ymd("2022-07-30"), by = "day"),
       wday = wday(date, label = T, abbr = T)) %>% 
  mutate(sun_date = ifelse(wday == "Sun", 1, 0)) %>% 
  mutate(x = cumsum(sun_date)) %>% 
  mutate(y = wday(date),
         z = rnorm(n())) %>% 
  print()

ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile()

ggsave("~/github/ggplot2/2022/20220819/save_ggplot_blog_01.png", 
       width = 8, height = 2.5, dpi = 320, units = "in")

ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile(width = 0.8, height = 0.8) +
  coord_fixed() 

ggsave("~/github/ggplot2/2022/20220819/save_ggplot_blog_02.png", 
       width = 8, height = 2.5, dpi = 320, units = "in")

ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile(width = 0.8, height = 0.8) +
  coord_fixed() +
  scale_y_reverse(breaks = c(2,4,6),
                  labels = c("Mon", "Wed", "Fri")) 

ggsave("~/github/ggplot2/2022/20220819/save_ggplot_blog_03.png", 
       width = 8, height = 2.5, dpi = 320, units = "in")

ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile(width = 0.8, height = 0.8) +
  coord_fixed() +
  scale_y_reverse(breaks = c(2,4,6),
                  labels = c("Mon", "Wed", "Fri")) +
  scale_x_continuous(breaks = tb1_x_breaks,
                     labels = month.abb[1:length(tb1_x_breaks)],
                     position = "top") 

ggsave("~/github/ggplot2/2022/20220819/save_ggplot_blog_04.png", 
       width = 8, height = 2.5, dpi = 320, units = "in")

tb1_x_breaks = tb1 %>% 
  filter(day(date) == 1) %>% 
  pull(x)
# month.abb
tb1_x_breaks
length(tb1_x_breaks)
month.abb[1:length(tb1_x_breaks)]

v_background_color = "gray100"
ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile(width = v_width, height = v_width) +
  scale_fill_gradient(low = "#EFFFFD", high = "#42C2FF") +
  # scale_fill_gradient(low = "#F3FCF9", high = "#68BDBA") +
  # scale_fill_gradient(low = "#EFFFFA", high = "#32D2DA") +
  
  scale_y_reverse(breaks = c(2,4,6),
                  labels = c("Mon", "Wed", "Fri"),
                  expand = expansion(c(0.05,0.06))) +
  scale_x_continuous(breaks = tb1_x_breaks,
                     labels = month.abb[1:length(tb1_x_breaks)],
                     position = "top",
                     expand = expansion(c(0.01,0))) +
  coord_fixed() +
  labs(caption = "twitter @sourcebox7") +
  theme_void(base_family = "Menlo-Bold") + #  "AppleSDGothicNeo-Bold"
  theme(legend.position = "none",
        plot.margin = margin(0,0.4,0,0.3, "in"),
        axis.text = element_text(colour = "gray30"),
        plot.caption = element_text(color = "gray30", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.1,0,0,0,"cm")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220819/save_ggplot_blog_2x1_02.png", 
       width = 8, height = 2.5, dpi = 320, units = "in")

##################################################

tb1 = tibble(date = seq.Date(ymd("2021-12-26"), ymd("2022-04-30"), by = "day"),
             wday = wday(date, label = T, abbr = T)) %>% 
  mutate(sun_date = ifelse(wday == "Sun", 1, 0)) %>% 
  mutate(x = cumsum(sun_date)) %>% 
  mutate(y = wday(date),
         z = rnorm(n())) %>% 
  print()

tb1

tb1_x_breaks = tb1 %>% 
  filter(day(date) == 1) %>% 
  pull(x)
# month.abb
tb1_x_breaks
length(tb1_x_breaks)
month.abb[1:length(tb1_x_breaks)]

v_background_color = "gray100"
# TEXTURE / color 수정
ggplot(tb1, aes(x, y, fill = z)) +
  geom_tile(width = v_width, height = v_width) +
  scale_fill_gradient(low = "#EFFFFD", high = "#42C2FF") +
  # scale_fill_gradient(low = "#F3FCF9", high = "#68BDBA") +
  # scale_fill_gradient(low = "#EFFFFA", high = "#32D2DA") +
  
  scale_y_reverse(breaks = c(2,4,6),
                  labels = c("Mon", "Wed", "Fri"),
                  expand = expansion(c(0.05,0.06))) +
  scale_x_continuous(breaks = tb1_x_breaks,
                     labels = month.abb[1:length(tb1_x_breaks)],
                     position = "top",
                     expand = expansion(c(0.01,0))) +
  coord_fixed() +
  labs(caption = "twitter @sourcebox7") +
  theme_void(base_family = "Menlo-Bold") + #  "AppleSDGothicNeo-Bold"
  theme(legend.position = "none",
        plot.margin = margin(0.5,0.4,0.1,0.3, "in"),
        axis.text = element_text(colour = "gray30"),
        plot.caption = element_text(color = "gray30", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.1,0,0.3,0,"cm")))

ggsave("~/github/ggplot2/2022/20220819//save_ggplot_2x1_11.png", 
       width = 8, height = 4, dpi = 320, units = "in")
