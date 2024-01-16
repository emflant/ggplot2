source('./core.R')



tb1 = read_excel('./2023/20230331/unsold_new_housings.xlsx', sheet='Sheet1')
tb1

tb2 = tb1 %>% 
  pivot_longer(!구분) %>% 
  mutate(date = ymd(paste0(name, '.1')))



tb3 = tb2 %>% 
  mutate(rank = row_number()) %>% 
  filter(rank == 1 | rank == max(rank)) %>% 
  mutate(height = c(2, -1)) %>% 
  mutate(height2 = c(1, -0.5))

v_dark_bgcolor = '#3E4353'  # 배경색
v_line_color = '#EECB6F'  # 그래프 선
#6E727E  회색줄

ggplot(tb2, aes(date, value)) +
  geom_line(size = 1, colour = v_line_color) +
  geom_point(size = 1.8,colour = v_line_color) +
  geom_label(data = tb3, aes(label = formatC(value, format="f", big.mark=",", digits = 0)),
             vjust = c(1.5,-0.5),
             colour = v_line_color,
             fill = v_dark_bgcolor,
             family = v_font_bm, size = 4.5,
             label.size = 0) +
  scale_y_continuous(limits = c(15000, 85000),
                     breaks = seq(20000, 80000, 20000),
                     labels = paste0(seq(20000, 80000, 20000) / 1000, "K")) +
  scale_x_date(breaks = ymd(c('2022-02-01', '2023-02-01')), 
               limits = ymd(c('2022-01-01', '2023-03-01')),
               date_labels = '%b \'%y') +
  labs(caption = "twitter @sourcebox7") +
  theme_void(base_family = v_font_bm) +
  theme(plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.margin = margin(0.3,0.3,0.2,0.2,"in"),
        axis.text.x = element_text(colour = "gray100",
                                   margin = margin(0.1,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100",
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "gray100", size = 1),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    # hjust = 0.88, 
                                    size = 8,
                                    margin = margin(0.2,0,0,0,"in")))


ggsave("./2023/20230331/v2_01.png",
       width = 6, height = 4, dpi = 240, units = "in")
