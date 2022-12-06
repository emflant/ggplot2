source("./core.R")


tb1 = read_excel("./2022/20221129/data.xlsx")

tb1 %>% distinct(계정항목)
sb1 = tb1 %>% distinct(계정항목) %>% 
  mutate(ord = 1:5) %>% 
  select(period = 계정항목, ord)
sb2 = tb1 %>% distinct(계정항목) %>% 
  mutate(ord = c(1,2,3,4,4), 
         class = c("6개월미만", "6개월이상 1년미만",
                   "1년이상 2년미만", "2년이상", "2년이상")) %>% 
  select(period = 계정항목, ord, class)
tb2 = tb1 %>% 
  pivot_longer("2020/09":"2022/09") %>% 
  mutate(date = ymd(paste0(name, "/01"))) %>% 
  select(period = 계정항목, date, value) %>% 
  inner_join(sb1) %>% 
  group_by(date) %>% 
  mutate(rate = round(value / sum(value) * 100, 1)) %>%
  mutate(rate2 = value / sum(value)) %>% 
  ungroup()
  
tb2 %>% print(n = Inf)
ggplot(tb2, aes(date, value, group = period, colour = period)) +
  geom_line() +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())



ggplot(tb2 %>% filter(ord == 1 | ord == 5), 
       aes(date, rate, group = period, colour = reorder(period, ord))) +
  geom_line() +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

ggplot(tb2, aes(date, value, fill = reorder(period, ord))) +
  geom_area() +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

tb2 %>% filter(date == max(date) | date == min(date))

ggplot(tb2 %>% filter(date == max(date) | date == min(date)), 
       aes(date, value, fill = reorder(period, ord))) +
  geom_col(position = "fill", width = 300) +
  scale_x_date(breaks = ymd(c("2020-09-01", "2022-09-01"))) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())


tb3 = tb2 %>% filter(date == max(date) | date == min(date)) %>% 
  inner_join(tibble(x = 1:2, date = ymd(c("2020-09-01", "2022-09-01"))))

tb4 = tb3 %>% 
  group_by(x) %>% 
  arrange(x, desc(ord)) %>% 
  mutate(cum_rate = cumsum(rate2))

tb4
pos_y = tb4 %>% 
  arrange(x, ord) %>% 
  filter(x == 1) %>% 
  pull(cum_rate) %>% 
  append(0)

pos_yend = tb4 %>% 
  arrange(x, ord) %>% 
  filter(x == 2) %>% 
  pull(cum_rate) %>% 
  append(0)

pos_yend
tb21 = tibble(x = 1.25, y = pos_y, 
       xend = 1.75, yend = pos_yend) 
# tb22 = tb21 %>% 
#   mutate(y = ifelse(y == 1, 0.999, y)) %>% 
#   mutate(yend = ifelse(yend == 1, 0.999, yend)) 
  
# ggplot(tb21, aes(x = x, y = y, xend = xend, yend = yend)) +
#   geom_segment()


ggplot(tb4, aes(x, value)) +
  geom_col(aes(fill = reorder(period, ord), colour = reorder(period, ord)), 
           position = position_fill(), width = 0.5) +
  geom_text(aes(label = rate), position = position_fill(.5),
            family = v_font_bm) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = tb22,
               linetype = "dashed") +
  scale_x_continuous(breaks = 1:2,
                     label = c("20.09", "22.09"),
                     expand = expansion(c(0.15, 0.15))) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

ggsave("./2022/20221129/20221129_01.png",
       width = 6, height = 4, dpi = 180, units = "in")
