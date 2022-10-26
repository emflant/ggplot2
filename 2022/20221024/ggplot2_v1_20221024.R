source('./core.R')

# https://www.data.go.kr/data/15051059/fileData.do
tb1 = read_excel('./2022/20221024/pharmacy_202206.xlsx')

tb1 %>% janitor::clean_names()


tb2 = tb1 %>% select(sido = 시도코드명, name = 요양기관명) %>% 
  group_by(sido) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(7)

tb3 = tb2 %>% mutate(max_yn = max(n) == n)

tb2
tb2$sido

reorder(tb2$sido, tb2$n)
ggplot(tb2, aes(reorder(sido, -n), n)) +
  geom_col() +
  theme_bw(base_family = "BMJUAOTF")

v_color = c('#fc8d59','#fee090','#e0f3f8','#91bfdb')
# ['#d73027','#fc8d59','#fee090','#e0f3f8','#91bfdb','#4575b4']
# ['#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#0c2c84']
v_background_color = "#334960" 
ggplot(tb3, aes(reorder(sido, -n), n)) +
  geom_col(aes(fill = factor(max_yn, c(T,F))), width = 0.7) +
  geom_text(aes(label = n),
            vjust = -0.8, size = 7,
            colour = "gray100",
            family = "BMJUAOTF") +
  scale_y_continuous(expand = expansion(c(0.05, 0.15))) +
  scale_fill_manual(values = c('#FF7433', '#fee090')) +
  labs(title = "지역별 약국 현황",
       subtitle = "('22.6월 기준)",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.3,0.4,0.3,"in"),
        legend.position = "none",
        axis.text.x = element_text(colour = "gray100", size = 15),
        plot.title = element_text(color = "gray100", 
                                  family = "BMJUAOTF", 
                                  # face = "bold",
                                  hjust = 0.06, 
                                  size = 25,
                                  margin = margin(0.2, 0, 0, 0,"in")),
        plot.subtitle = element_text(color = "gray100", 
                                     family = "BMJUAOTF", 
                                     # face = "bold",
                                     vjust = 5.6,
                                     hjust = 0.41, 
                                     size = 15),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.95, 
                                    size = 12,
                                    margin = margin(0.4,0,0,0,"in")),
        plot.background = element_rect(fill = v_background_color, color = v_background_color))


ggsave("./2022/20221024/save_01.png", 
       width = 8, height = 6, dpi = 120, units = "in", bg = "gray50")

