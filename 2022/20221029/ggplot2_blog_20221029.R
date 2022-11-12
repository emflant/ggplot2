source('./core.R')




tb1 = tibble(년도 = 2017:2021,
             서울 = seq(100, 140, 10),
             대전 = seq(200, 240, 10),
             대구 = seq(300, 340, 10),
             광주 = seq(400, 440, 10),
             부산 = seq(500, 540, 10)) 
tb1
tidyr::pivot_longer()

pivot_longer(tb1, cols = 서울:부산)

pivot_longer(tb1, cols = 서울:부산,
             names_to = "도시",
             values_to = "통계값")

tb1 = tibble(x = 2017:2021,
       y_2G = c(2556242, 1672741, 1020510, 502585, 171930),
       y_3G = c(10661556, 9549356, 7515903, 5604557, 3479622),
       y_4G = c(50440880, 55133681, 55687974, 52555161, 48288764),
       y_5G = c(0,0, 4668154, 11851373, 20915176))
tb1
tb2 = tb1 %>% pivot_longer(cols = y_2G:y_5G,
                     names_to = "code",
                     values_to = "count")

ggplot(tb1) +
  geom_line(aes(x, y_2G)) +
  geom_line(aes(x, y_3G)) +
  geom_line(aes(x, y_4G)) +
  geom_line(aes(x, y_5G)) +
  theme(axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221030/images/20221030_11.png", 
       width = 6, height = 4, dpi = 180, units = "in")

tb2 = tb1 %>% pivot_longer(cols = y_2G:y_5G,
                           names_to = "code", 
                           values_to = "count")

tb2 = tb1 %>% pivot_longer(cols = y_2G:y_5G,
                           names_to = "code", 
                           values_to = "count") %>% 
  mutate(code = str_sub(code, 3))

tb2
tb2 %>% group_by(code) %>% 
  summarise(max(count))

ggplot(tb2, aes(x, count, colour = code)) +
  geom_line() +
  geom_point() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221030/images/20221030_12.png", 
       width = 6, height = 4, dpi = 180, units = "in")

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


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221030/images/20221030_01.png", 
       width = 8, height = 6, dpi = 120, units = "in")
