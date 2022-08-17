library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)


tibble(x = rep(1,10), y = rep(1,10), z = 1:10)

tibble(x = rep(1,10), y = rep(1,10), z = 1:10) %>% 
  ggplot(aes(x,y, fill = z)) +
  geom_col(color = "gray100")

ggsave("~/github/ggplot2/2022/20220816/save_ggplot_blog_01.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")

# tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:10, 3))
# tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:3, each = 10))
tibble(x = rep(1:3, each = 10), 
       y = rep(1,30), 
       z = rep(1:3, each = 10)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_col(color = "gray100", width = 1)

ggsave("~/github/ggplot2/2022/20220816/save_ggplot_blog_02.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")



# tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:10, 3))
# tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:3, each = 10))
# tibble(x = rep(1:3, each = 10), y = rep(1,30), z = rep(1:3, each = 10)) %>% 
#   ggplot(aes(x, y, fill = z)) +
#   geom_col(color = "gray100", width = 1)


v_size = 10

tibble(x = rep(1:10, each = 10), 
       y = rep(1,100), 
       z = rep(1:10, each = 10)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_col(color = "gray100", width = 1) +
  theme(aspect.ratio = 1)

ggsave("~/github/ggplot2/2022/20220816/save_ggplot_blog_03.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")

v_background_color = "gray100"

#  x좌표임. 1,1,1,...2,2,2,...10,10,10 총 100개
# 1.1.1.1.1.... 1.1.1.1.1.1.1 총 100개 
v_background_color = "gray100"

tibble(x = rep(1:v_size, each = 10), 
       y = rep(1,v_size*10), 
       z = c(rep("a", 32), rep("b", 45), rep("c", 23)))

tibble(x = rep(1:v_size, each = 10), 
       y = rep(1,v_size*10), 
       z = c(rep("a", 32), rep("b", 45), rep("c", 23))) %>% 
  ggplot(aes(x,y, fill = z)) +
  geom_col(color = "gray100", size = 1, width = 1) +
  # scale_fill_brewer(palette = "Blues") +
  scale_x_reverse() +
  coord_flip() +
  theme_void() +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/ggplot2/2022/20220816/save_ggplot_blog_04.png", 
       width = 8, height = 4.5, dpi = 320, units = "in")





############## 실패...

f_sample = function(in_sample){
  sample(0:(100-in_sample), 1)
}

f_rep = function(in_count, in_chr){
  rep(in_chr, times = in_count)
}
f_rep(20, "a")

v_size = 9
tibble(x = rep(1:v_size, each = 10), y = rep(1,v_size*10)) %>% 
  mutate(v1 = sample(1:100,100, replace = T)) %>% 
  mutate(v2 = map_int(v1, f_sample)) %>% 
  mutate(v3 = 100 - v1 - v2) %>% 
  mutate(v4 = map(v1, f_rep, "a")) %>% 
  mutate(v5 = map(v2, f_rep, "b")) %>% 
  mutate(v6 = map(v3, f_rep, "c")) %>% 
  mutate(v7 = map2(v4,v5, append)) %>% 
  mutate(v8 = map2(v7,v6, append)) %>% 
  select(x,y,v8)


###############
v_plot_count = 12


# rep(rep(1:10, each = 10), v_plot_count)
# rep(rep(1,100), v_plot_count)
#  x좌표임. 1,1,1,...2,2,2,...10,10,10 총 100개
# 1.1.1.1.1.... 1.1.1.1.1.1.1 총 100개 
tb1 = tibble(v1 = sample(1:100, v_plot_count, replace = T)) %>% 
  mutate(v2 = map_int(v1, f_sample)) %>% 
  mutate(v3 = 100 - v1 - v2) %>% 
  mutate(v4 = map(v1, f_rep, "a")) %>% 
  mutate(v5 = map(v2, f_rep, "b")) %>% 
  mutate(v6 = map(v3, f_rep, "c")) %>% 
  mutate(v7 = map2(v4,v5, append)) %>% 
  mutate(v8 = map2(v7,v6, append)) %>% 
  select(v8) %>% 
  unnest_longer(v8) %>% 
  mutate(x = rep(rep(1:10, each = 10), v_plot_count),
         y = rep(rep(1,100), v_plot_count),
         g = rep(1:v_plot_count, each = 100)) %>% 
  select(g, x,y,z = v8) %>% 
  print(n = 5)


v_background_color = "black"
v_background_color = "#282A36" #282A36
# , levels = c("c", "b", "a")
ggplot(tb1,aes(x,y, fill = factor(z))) +
  geom_col(color = v_background_color, size = 1, width = 1) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~g) +
  labs(caption = "twitter @sourcebox7") +
  theme_void() +
  theme(legend.position = "none", 
        aspect.ratio = 1,
        strip.text = element_blank(),
        legend.title =element_blank(), 
        plot.margin = margin(0.5,0.1,0.1,0.1, "in"),
        plot.caption = element_text(color = "gray70", 
                                    family = "Menlo", 
                                    hjust = 0.98, 
                                    size = 10,
                                    margin = margin(0.1,0,0.3,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))

ggsave("~/github/ggplot2/2022/20220816/save_ggplot_blog_16x13_Spectral.png", 
       width = 8, height = 6.5, dpi = 320, units = "in")



# twitter @sourcebox7
ggplot(tb1, aes(x, y, fill = z)) +
  geom_col(color = "gray100", 
           size = 1, width = 1) +
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~ g) +
  theme_void() +
  theme(aspect.ratio = 1)
