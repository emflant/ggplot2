
source("./core.R")


# 전세자금대출 증감(조원): 22.9월 +0.6 → 10월 +0.2 → 11월 -1.0p(21.11월 +1.7)
# 전세자금대출 증감(조원): 22.8월 +0.9 → 9월 +0.6 → 10월 +0.2p(21.10월 +2.2)
# 전세자금대출 증감(조원): 22.7월 +1.1 → 8월 +0.9 → 9월 +0.6p(21.9월 +2.5)
# 전세자금대출 증감(조원): 22.6월 +0.9 → 7월 +1.1 → 8월 +0.9p(21.8월 +2.8)
# 전세자금대출 증감(조원): 22.5월 +1.1 → 6월 +0.9 → 7월 +1.1p(21.7월 +2.8)
# 전세자금대출 증감(조원): 22.4월 +1.1 → 5월 +1.1 → 6월 +0.9p(21.6월 +2.2)
# 전세자금대출 증감(조원): 22.3월 +1.2 → 4월 +1.1 → 5월 +1.1p(21.5월 +2.3)

read_excel("./2022/20221209/data.xlsx") %>% 
  mutate(date = as.Date(date))
read_excel("./2022/20221209/data.xlsx") %>% 
  mutate(date = as.Date(date)) %>% 
  write_csv("./2022/20221209/data.csv")


tb1 = read_csv("./2022/20221209/data.csv") %>% 
  mutate(x = row_number())
tb1


sb1 = tb1 %>% 
  filter(x %% 2 == 1) %>% 
  mutate(v = c(0.3,0.5,0.4,0.3,0.3,0.3,0.4,-0.3))

# ggplot(tb1, aes(x, value)) +
#   geom_line()
# 
# formatC(-1, format = "f")
# formatC(-1, format = "f", digits = 1)
# sprintf()

ggplot(tb1, aes(x, value)) +
  # annotate("line", x = -1:17, y = 0, size = 0.8) +
  geom_hline(yintercept = 0, size = 0.8) +
  geom_line(colour = "#EB6440", size = 2,
            lineend = "round") +
  geom_point(data = sb1, aes(x, value), 
             colour = "#EB6440", fill = "gray100",
             shape = 21, size = 2) +
  geom_label(data = sb1, aes(x, value + v, label = formatC(value, format = "f", digits = 1)),
            colour = "#EB6440",
            label.padding = unit(0.2, "lines"),
            label.size = unit(0, "mm"),
            family = v_font_bm) +
  # scale_x_continuous(expand = expansion(0),
  #                    breaks = sb1$x,
  #                    label = sb1$date) +
  
  scale_y_continuous(breaks = seq(-1, 2.5, by= 0.5),
                     labels = seq(-1, 2.5, by= 0.5) %>% 
                       formatC(format = "f", digits = 1),
                     expand = expansion(0.2)) +
  
  theme_bw() +
  theme(text = element_text(family = v_font_bm),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        # axis.ticks.length.x = unit(-1,"mm"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        
        )
ggsave("./2022/20221209/20221209_01.png",
       width = 6, height = 4, dpi = 180, units = "in")


tb1
ggplot(tb1, aes(date, value)) +
  # annotate("line", x = ymd("2021-08-01"):ymd("2022-12-01"), y = 0, size = 0.8) +
  geom_hline(yintercept = 0, size = 0.8) +
  geom_line(colour = "#EB6440", size = 2,
            lineend = "round") +
  geom_point(data = sb1, aes(date, value), 
             colour = "#EB6440", fill = "gray100",
             shape = 21, size = 2) +
  geom_label(data = sb1, aes(date, value + v, label = formatC(value, format = "f", digits = 1)),
             colour = "#EB6440", size = 5,
             label.padding = unit(0.2, "lines"),
             label.size = unit(0, "mm"),
             family = v_font_bm) +
  scale_x_date(expand = expansion(0.1), date_breaks = "2 month",
               date_labels = "%y %b") +
  scale_y_continuous(breaks = seq(-1.5, 3.0, by= 0.5),
                     labels = seq(-1.5, 3.0, by= 0.5) %>%
                       formatC(format = "f", digits = 1),
                     expand = expansion(0.15)) +
  # scale_y_continuous(breaks = seq(-1, 3.0, by= 1),
  #                    labels = seq(-1, 3.0, by= 1) %>% 
  #                      formatC(format = "f", digits = 1),
  #                    expand = expansion(0.15)) +
  
  theme_bw() +
  theme(text = element_text(family = v_font_bm),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(hjust = 0.5), 
        axis.ticks.y = element_blank(),
        # axis.ticks.length.x = unit(-1,"mm"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.3)
  )
ggsave("./2022/20221209/20221209_02.png",
       width = 6, height = 4, dpi = 180, units = "in")







ggplot(tb1, aes(date, value, colour = value)) +
  geom_hline(yintercept = 0, size = 0.8, colour = "gray100") +
  geom_line(size = 2, colour = "#FCE700",
            lineend = "round") +
  # geom_point(data = sb1, aes(date, value), 
  #            # colour = "#EB6440", 
  #            # fill = "gray100",
  #            shape = 21, size = 2) +
  # geom_label(data = sb1, aes(date, value + v, label = formatC(value, format = "f", digits = 1)),
  #            fill = v_dark_bgcolor,
  #            size = 5,
  #            label.padding = unit(0.2, "lines"),
  #            label.size = unit(0, "mm"),
  #            family = v_font_bm) +
  scale_x_date(expand = expansion(0.1), date_breaks = "2 month",
               date_labels = "%y %b") +
  scale_y_continuous(breaks = seq(-1.5, 3.0, by= 0.5),
                     labels = seq(-1.5, 3.0, by= 0.5) %>%
                       formatC(format = "f", digits = 1),
                     expand = expansion(0.15)) +
  scale_colour_gradient(high = "yellow", low = "red") +
  theme_void() +
  theme(text = element_text(family = v_font_bm),
        plot.margin = margin(0.3,0.5,0.3,0.5,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(hjust = 0.5, colour = "gray100"), 
        axis.ticks.y = element_blank(),
        # axis.ticks.length.x = unit(-1,"mm"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.3,
                                          colour = "gray100")
  )
ggsave("./2022/20221209/20221209_03.png",
       width = 6, height = 4, dpi = 180, units = "in")


read.table("aa.txt")

read.table("./data/aa.txt")


getwd()

setwd("c:/data")
read.table("aa.txt")