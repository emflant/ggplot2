source('./core.R')


# https://www.itstat.go.kr/itstat/main.html

# 21.12월 

tb1 = tibble(x = 2017:2021,
       y_2G = c(2556242, 1672741, 1020510, 502585, 171930),
       y_3G = c(10661556, 9549356, 7515903, 5604557, 3479622),
       y_4G = c(50440880, 55133681, 55687974, 52555161, 48288764),
       y_5G = c(0,0, 4668154, 11851373, 20915176))

tb2 = tb1 %>% pivot_longer(cols = y_2G:y_5G,
                     names_to = "code",
                     values_to = "count")

ggplot(tb1) +
  geom_line(aes(x, y_2G)) +
  geom_line(aes(x, y_3G)) +
  geom_line(aes(x, y_4G)) +
  geom_line(aes(x, y_5G))



tb2 = tb1 %>% pivot_longer(cols = y_2G:y_5G,
                           names_to = "code", 
                           values_to = "count") %>% 
  mutate(code = str_sub(code, 3))

tb2
tb2 %>% group_by(code) %>% 
  summarise(max(count))



# user_theme = function(){
#   theme_bw(base_family = "NanumGothicExtraBold") +
#     theme(axis.title = element_blank())
# }

ggplot(tb2, aes(x, count, colour = code)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(breaks = seq(20000000, 60000000, 20000000),
                     labels = c("2천만", "4천만", "6천만"),
                     limits = c(0, 60000000)) +
  theme_bw(base_family = "NanumGothicExtraBold") +
  theme(axis.title = element_blank())



ggplot(tb2, aes(x, count, colour = code)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = seq(20000000, 60000000, 20000000),
                     labels = c("2천만", "4천만", "6천만"),
                     limits = c(0, 60000000)) +
  labs(title = "이동통신 기술방식별 회선 현황('17~21년)") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.background = element_rect(fill = "gray100", color = "gray100"),
        plot.margin = margin(0.2,0.2,0.2,0.2,"in"),
        panel.grid.major.y = element_line(colour = "gray70"),
        plot.title = element_text(family = "NanumGothicExtraBold",
                                  margin = margin(0.1,0,0.1,0,"in")),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(margin = margin(0,0.1,0,0,"in")))

ggsave("./2022/20221029/save_21.png", 
       width = 6, height = 4, dpi = 180, units = "in")


scale_color_brewer()
v_background_color = "#334960" 
v_color = c('#fc8d59','#fee090','#abdda4','#91bfdb')
# v_color = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4')
ggplot(tb2, aes(x, count, colour = code)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_colour_manual(values = v_color) +
  scale_y_continuous(breaks = seq(0, 60000000, 20000000),
                     labels = c("0", "2천만", "4천만", "6천만"),
                     limits = c(0, 60000000)) +
  labs(title = "이동통신 기술방식별 회선 현황('17~21년)",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.background = element_rect(fill = v_background_color, color = v_background_color),
        plot.margin = margin(0.2,0.5,0.5,0.5,"in"),
        panel.grid.major.y = element_line(colour = "gray50"),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  # face = "bold",
                                  hjust = -0.4, 
                                  size = 20,
                                  margin = margin(0.3, 0, 0.4, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.4,0,0,0,"in")),
        legend.margin = margin(0,0,0,5,"mm"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray100",
                                   size = 15,
                                   margin = margin(0,0,3,0,"mm")),
        # legend.spacing.y = unit(1.1, "in"),
        axis.title = element_blank(),
        axis.text = element_text(colour = "gray100",
                                 size = 15,
                                 margin = margin(0.1,0.1,0,0,"in")))


ggsave("./2022/20221029/save_22.png", 
       width = 8, height = 6, dpi = 120, units = "in")

######################################################
######################################################
######################################################



tb3 = tb2 %>% 
  filter(x == 2021) %>% 
  mutate(r = round(count/sum(count) * 100, 2)) %>% 
  mutate(rate = paste0(r, "%")) %>% 
  mutate(h = c(0,0,0,10))
# tb3
tb3

# round(0.234, 2)
g1 = ggplot(tb3, aes(x, count, fill = factor(code))) +
  geom_col() +
  geom_text(aes(label = rate),
            family = "BMJUAOTF",
            size = 5,
            position = position_stack(0.5), 
            hjust = c(-1.5,2,1.5,-0.5),
            vjust = c(-3.5,-3.5,0.5,0.5)) +
  coord_polar(theta = "y") +
  # scale_fill_manual()
  xlim(c(2015, 2024)) +
  labs(title = "이동통신 기술방식별 회선 현황('21년말 기준)") +
  theme_void(base_family = "NanumGothicExtraBold") +
  theme(plot.background = element_rect(fill = "gray100", color = "gray100"),
        plot.margin = margin(0.2,0.2,0.2,0.2,"in"),
        legend.title = element_blank(),
        legend.text = element_text(family = "BMJUAOTF"),
        legend.position = c(1, 0.4))
  
ggsave(plot = g1, "./2022/20221029/save_11.png", 
         width = 6, height = 5, dpi = 180, units = "in")




plot_spacer() +
  theme(plot.background = element_rect(fill = NA, color = NA)) +
  inset_element(g1, left = -0.1, bottom = -0.1, right = 1, top = 0.98) +
  inset_element(ggplot() +
                  # 왼쪽 꺾은선
                  annotate("segment", x = 0.38, xend = 0.42, y = 0.83, yend = 0.83) +
                  annotate("segment", x = 0.42, xend = 0.46, y = 0.83, yend = 0.75) +
                  # 오른쪽 꺾은 선
                  annotate("segment", x = 0.5, xend = 0.55, y = 0.75, yend = 0.83) +
                  annotate("segment", x = 0.55, xend = 0.62, y = 0.83, yend = 0.83) +
                  xlim(0, 1) +
                  ylim(0, 1) +
                  theme_void(), 
                left = -0.1, bottom = -0.1, right = 1, top = 0.98) +
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0,"in")))

ggsave("./2022/20221029/save_51.png", 
       width = 6, height = 5, dpi = 180, units = "in", bg = "gray0")





######################################################
######################################################
######################################################

v_background_color = "#334960" 
v_color = c('#fc8d59','#fee090','#abdda4','#91bfdb')


tb3 = tb2 %>% 
  filter(x == 2021) %>% 
  mutate(r = round(count/sum(count) * 100, 2)) %>% 
  mutate(rate = paste0(r, "%")) %>% 
  mutate(h = c(0,0,0,10))
# tb3
tb3


v_color = c('#d73027','#e0f3f8','#fee090','#4575b4')
# ['#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8','#91bfdb','#4575b4']

g1 = ggplot(tb3, aes(x, count, fill = factor(code))) +
  geom_col() +
  geom_text(aes(label = rate),
            family = "BMJUAOTF",
            size = 7, colour = "gray100",
            position = position_stack(0.5), 
            hjust = c(-1.5,2,1.5,-0.5),
            vjust = c(-3.5,-3.5,0.5,0.5)) +
  # scale_fill_manual(values = v_color) +
  scale_fill_brewer(palette = "RdYlBu") +
  coord_polar(theta = "y") +
  # scale_fill_manual()
  xlim(c(2015, 2024)) +
  # labs(title = "이동통신 기술방식별 회선 현황('21년말 기준)") +
  theme_void(base_family = "NanumGothicExtraBold") +
  theme(plot.background = element_rect(fill = v_background_color, color = v_background_color),
        plot.margin = margin(0.2,0.2,0.2,0.2,"in"),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  # face = "bold",
                                  hjust = -0.4, 
                                  size = 20,
                                  margin = margin(0.3, 0, 0, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.4,0,0,0,"in")),
        legend.title = element_blank(),
        legend.text = element_text(family = "BMJUAOTF", 
                                   color = "gray100",
                                   size = 15),
        # legend.key = element_rect(size = 6, colour = v_background_color),
        # legend.spacing.y = unit(10, "cm"),
        legend.position = c(0.9, 0.4))

ggsave(plot = g1, "./2022/20221029/save_12.png", 
       width = 8, height = 6, dpi = 120, units = "in")

g1


plot_spacer() +
  theme(plot.background = element_rect(fill = NA, color = NA)) +
  inset_element(g1, left = 0, right = 1, bottom = -0.05, top = 0.95) +
  inset_element(ggplot() +
                  # 왼쪽 꺾은선
                  annotate("segment", x = 0.38, xend = 0.42, y = 0.83, yend = 0.83, colour = "gray100") +
                  annotate("segment", x = 0.42, xend = 0.46, y = 0.83, yend = 0.74, colour = "gray100") +
                  # 오른쪽 꺾은 선
                  annotate("segment", x = 0.5, xend = 0.55, y = 0.74, yend = 0.83, colour = "gray100") +
                  annotate("segment", x = 0.55, xend = 0.64, y = 0.83, yend = 0.83, colour = "gray100") +
                  xlim(0, 1) +
                  ylim(0, 1) +
                  theme_void(), 
                left = 0, right = 1, bottom = 0, top = 1) +
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0,"in"),
                                plot.background = element_rect(fill = v_background_color, color = v_background_color)
                                
                                ))

ggsave("./2022/20221029/save_52.png", 
       width = 8, height = 7, dpi = 120, units = "in", bg = v_background_color)


