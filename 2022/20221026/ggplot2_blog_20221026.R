source('./core.R')

# https://www.data.go.kr/data/15051059/fileData.do
tb1 = read_excel('./2022/20221024/pharmacy_202206.xlsx')

tb1

tb2 = tb1 %>% select(sido = 시도코드명, name = 요양기관명) %>% 
  group_by(sido) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(7)



tibble(x = c(1,2,3, 1),
       y = c(10,-43, 20, -5)) %>% 
  ggplot(aes(x,y)) +
  geom_col() + 
  ylim(c(-100, 100))


# https://www.data.go.kr/data/15070293/fileData.do

tb1 = read_excel( "./2022/20221026/data_20211231.xlsx")

tb2 = tb1 %>% 
  select(age = 사상자연령층, gender = 사상자성별, n = 사망자수) %>% 
  filter(age != '불명', gender != "기타/불명")
tb2
ggplot(tb2, aes(age, n, fill = gender)) +
  geom_col() +
  theme_bw(base_family = "NanumGothicExtraBold") +
  theme(axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221026/images/20221026_11.png", 
       width = 6, height = 4, dpi = 180, units = "in", )



tb3 = tb2 %>% 
  mutate(n2 = ifelse(gender == "남", n * -1, n),
         ord = as.numeric(str_sub(age, 1, 2))) 
tb3

ggplot(tb3, aes(age, n2, fill = gender)) +
  geom_col() + 
  coord_flip() +
  theme_bw(base_family = "NanumGothicExtraBold") +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221026/images/20221026_12.png", 
       width = 6, height = 4, dpi = 180, units = "in", )



ggplot(tb3, aes(reorder(age, -ord), n2, fill = gender)) +
  geom_col() + 
  coord_flip() +
  theme_bw(base_family = "NanumGothicExtraBold") +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221026/images/20221026_13.png", 
       width = 6, height = 4, dpi = 180, units = "in", )



tb3 = tb2 %>% 
  mutate(n2 = ifelse(gender == "남", n * -1, n),
         h = ifelse(gender == "남", 1.3, -0.6)) %>% 
  mutate(ord = as.numeric(str_sub(age, 1, 2)))

v_background_color = "#334960" 

# ['#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d']
ggplot(tb3, aes(reorder(age, -ord), n2)) +
  geom_col(aes(fill = gender)) + 
  geom_text(aes(label = n),hjust = tb3$h,
            colour = "gray100",
            size = 5.5,
            family = "BMJUAOTF") +
  ylim(c(min(tb3$n2) * 1.2, max(tb3$n2) * 1.22)) +
  # scale_fill_brewer(palette = "Purples") +
  scale_fill_manual(values = c('#dadaeb','#807dba')) +
  coord_flip() +
  labs(title = "'21년 교통사고 사망자 통계",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.5,0.3,0.4,"in"),
        legend.position = c(0.9, 0.93),
        legend.key.size = unit(7, "mm"),
        # legend.spacing.y = unit(1, "cm"),
        legend.direction = "horizontal",
        # legend.box.margin = margin(2,0,2,0,"mm"),
        axis.text.y = element_text(colour = "gray100", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray100", size = 12,
                                   margin = margin(0,3,0,0,"mm")),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  # face = "bold",
                                  hjust = -0.2, 
                                  size = 25,
                                  margin = margin(0.2, 0, 0.3, 0,"in")),
        plot.subtitle = element_text(color = "gray100", 
                                     family = "BMJUAOTF",
                                     vjust = 5.6,
                                     hjust = 0.41, 
                                     size = 15),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.3,0,0.1,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221026/images/20221026_01.png", 
       width = 8, height = 6, dpi = 120, units = "in", )







tb3 = tb2 %>% 
  mutate(n2 = ifelse(gender == "남", n * -1, n),
         h = ifelse(gender == "남", 1.3, -0.6)) %>% 
  mutate(ord = as.numeric(str_sub(age, 1, 2)))

v_background_color = "#334960" 

# ['#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d']
ggplot(tb3, aes(reorder(age, -ord), n2)) +
  geom_col(aes(fill = gender)) + 
  geom_text(aes(label = n),hjust = tb3$h,
            colour = "gray100",
            size = 5.5,
            family = "BMJUAOTF") +
  ylim(c(min(tb3$n2) * 1.2, max(tb3$n2) * 1.22)) +
  # scale_fill_brewer(palette = "Greys") +
  scale_fill_manual(values = c('gray90','gray70')) +
  coord_flip() +
  labs(title = "'21년 교통사고 사망자 통계",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.5,0.3,0.4,"in"),
        legend.position = c(0.9, 0.93),
        legend.key.size = unit(7, "mm"),
        # legend.spacing.y = unit(1, "cm"),
        legend.direction = "horizontal",
        # legend.box.margin = margin(2,0,2,0,"mm"),
        axis.text.y = element_text(colour = "gray100", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(colour = "gray100", size = 12,
                                   margin = margin(0,3,0,0,"mm")),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  # face = "bold",
                                  hjust = -0.2, 
                                  size = 25,
                                  margin = margin(0.2, 0, 0.3, 0,"in")),
        plot.subtitle = element_text(color = "gray100", 
                                     family = "BMJUAOTF",
                                     vjust = 5.6,
                                     hjust = 0.41, 
                                     size = 15),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 12,
                                    margin = margin(0.3,0,0.1,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221026/images/20221026_02.png", 
       width = 8, height = 6, dpi = 120, units = "in", )
