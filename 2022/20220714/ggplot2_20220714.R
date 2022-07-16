
# https://www.data.go.kr/data/15048032/fileData.do
# subway = read_excel(path = "~/github/ggplot2/2022/20220714/subway_202205.xlsx")

# 휴일정보
holiday = tibble(date = c("2022-01-31", "2022-02-01", "2022-02-02", "2022-03-01", "2022-03-09", "2022-05-05")) %>% 
  mutate(date = ymd(date), code = "h")

tb_input = tibble(seq = 1:4,
       station_name = c("서울역", "여의도", "화곡", "강남"),
       low_colour = c("#E7AB79","#B2C8DF","#98B4AA","#889EAF"),
       high_colour = c("#B25068","#495C83","#596E79","#506D84")) %>% 
  filter(seq == 1)



v_station_name = tb_input %>% pull(station_name)
v_low = tb_input %>% pull(low_colour)
v_high = tb_input %>% pull(high_colour)
v_file_seq = tb_input %>% pull(seq)


subway1 = subway %>% janitor::clean_names() 

tb_time = subway1 %>% 
  filter(yeogmyeong == v_station_name, gubun == '하차',
         naljja == as.Date('2022-05-01')) %>%  
  pivot_longer(cols = starts_with("x"), names_to = "time") %>% 
  distinct(time) %>% 
  mutate(time_dbl = 6:24)

subway2 = subway1 %>% 
  filter(yeogmyeong == v_station_name, gubun == '하차',
         naljja >= as.Date('2022-05-01') ) %>% 
  group_by(naljja, yeogmyeong) %>% 
  summarise_at(.vars = vars(x06si_ijeon:x23si_ihu), sum) %>% 
  pivot_longer(cols = starts_with("x"), names_to = "time") %>% 
  left_join(tb_time, by = "time") %>% 
  mutate(times = map2(time_dbl, value, rep),
         naljja = ymd(naljja)) %>% 
  mutate(wday = wday(naljja, label = T), .after = "naljja") %>% 
  mutate(week = epiweek(naljja), .after = "wday") %>% 
  unnest_longer(times) %>% 
  left_join(holiday, by = c("naljja"="date")) %>% 
  mutate(holiday = code=="h"|wday =="Sat"|wday == "Sun") %>% 
  mutate(holiday = !is.na(holiday)) %>% 
  select(-time, -value, -code) %>% 
  print(n = 100)

###############################################################
# subway2 %>% select(-value, -code) %>% 
#   filter(naljja == "2022-05-13") %>% 
#   mutate(time_str =  as.character(time_dbl / 3 + 4)) %>% 
#   ggplot(aes(times, fill = ..count..)) +
#   geom_histogram(binwidth = 3) +
#   scale_x_continuous(breaks = seq(6,24,6)) +
#   scale_fill_gradient(low = "#E7AB79", high = "#B25068") +
#   theme_void() +
#   theme(legend.position = "none",
#         plot.margin = margin(1,1,1,1,"cm"))
###############################################################
week_label
week_label <- paste0('Week ', 18:22)
names(week_label) <- 18:22

ggplot(subway2,aes(x = times)) +
  geom_histogram(aes(fill = ..count..), alpha = 1, binwidth = 6) +
  scale_x_continuous(breaks = seq(6,24,6)) +
  scale_y_continuous(position = "right") +
  scale_fill_gradient(low = v_low, high = v_high) + 
  facet_grid(week ~ wday, 
             switch = "y",
             labeller = labeller(week = week_label)) +
  labs(title = paste0("'22.5월 ", subway2 %>% pull(yeogmyeong) %>% head(1), " 지하철역 하차인원 통계"),
       caption = "twitter @sourcebox7") +
  theme_minimal(base_family = "AppleSDGothicNeo-Bold", base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "gray80", size = 0.3),
        axis.title = element_blank(),
        plot.title = element_text(margin = margin(0,0,.5,0, "cm"),
                                  hjust = 0.5),
        # strip.switch.pad.grid = element_text(angle = 0),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside",
        plot.caption = element_text(color = "gray30", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 6,
                                    margin = margin(.5,0,0,0,"cm")),
        plot.margin = margin(.7,1,.7,1,"cm"),
        plot.background = element_rect(fill = "#F1F0EA", color = "#F1F0EA")) 


ggsave(paste0("~/github/ggplot2/2022/20220714/result", v_file_seq, "_20220714.png"),
       width = 8, height = 6, dpi = 320, units = "in")



