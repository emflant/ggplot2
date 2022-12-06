source("./core.R")


tb1 = read_excel("./2022/20221129/data.xlsx")
tb1 %>% str
tb1 %>% distinct(계정항목)
# sb1 = tb1 %>% distinct(계정항목) %>% 
#   mutate(ord = 1:5) %>% 
#   select(period = 계정항목, ord)
sb2 = tb1 %>% distinct(계정항목) %>% 
  mutate(ord = c(1,2,3,4,4), 
         class = c("6개월미만", "6개월이상 1년미만",
                   "1년이상 2년미만", "2년이상", "2년이상")) %>% 
  select(period = 계정항목, ord, class)

sb2

tb2 = tb1 %>% 
  pivot_longer("2020/09":"2022/09") %>% 
  mutate(date = ymd(paste0(name, "/01"))) %>% 
  select(period = 계정항목, date, value) %>% 
  inner_join(sb2) %>% 
  group_by(date, ord, class) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  group_by(date) %>%
  mutate(rate = round(value / sum(value) * 100, 1)) %>%
  mutate(rate2 = value / sum(value)) %>%
  ungroup()

tb3 = tb2 %>% filter(date == max(date) | date == min(date)) %>% 
  inner_join(tibble(x = 1:2, date = ymd(c("2020-09-01", "2022-09-01"))))

tb4 = tb3 %>% 
  group_by(x) %>% 
  arrange(x, desc(ord)) %>% 
  mutate(cum_rate = cumsum(rate2),
         cum_value = cumsum(value))

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



################################################################################



ggplot(tb4, aes(x, value)) +
  geom_col(aes(fill = reorder(class, ord), colour = reorder(class, ord)), 
           position = position_stack(), width = 0.5) +
  geom_text(aes(label = paste0(round(value / 1000, 0), "조")), # 조단위
            position = position_stack(.5),
            family = v_font_bm) +
  # geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = tb21,
  #              linetype = "dashed", size = 0.4) +
  scale_x_continuous(breaks = 1:2,
                     label = c("'20.9월", "'22.9월"),
                     expand = expansion(c(0.15, 0.15))) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(title = "그림1. 일반적인 누적막대그래프") +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_01.png",
       width = 6, height = 4, dpi = 180, units = "in")


################################################################################

tb5 = tb4 %>% ungroup() %>%
  arrange(x, ord) %>% 
  select(x, date, ord, value, rate, class)


write_excel_csv(tb5,file = "./2022/20221129/data.csv")
readr::read_csv("./2022/20221129/data.csv")



################################################################################
ggplot(tb5, aes(x, value, fill = reorder(class, ord))) +
  geom_col(width = 0.5) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_11.png",
       width = 6, height = 4, dpi = 180, units = "in")


################################################################################
ggplot(tb5, aes(x, value, fill = reorder(class, ord))) +
  geom_col(position = position_fill(),
           width = 0.5) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())



ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_12.png",
       width = 6, height = 4, dpi = 180, units = "in")



################################################################################
ggplot(tb5, aes(x, value, fill = reorder(class, ord))) +
  geom_col(position = position_fill(),
           width = 0.5) +
  geom_text(aes(label = rate), 
            position = position_fill(0.5),
            family = v_font_bm) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

tb5


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_13.png",
       width = 6, height = 4, dpi = 180, units = "in")


################################################################################



################################################################################
ggplot(tb5, aes(x, value, fill = reorder(class, ord))) +
  geom_col(position = position_fill(),
           width = 0.5) +
  geom_text(aes(label = rate), 
            position = position_fill(0.5),
            family = v_font_bm) +
  scale_x_continuous(breaks = 1:2,
                     label = c("'20.9월", "'22.9월"),
                     expand = expansion(c(0.15, 0.15))) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

tb5


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_14.png",
       width = 6, height = 4, dpi = 180, units = "in")


################################################################################


ggplot(tb4, aes(x, value)) +
  geom_col(aes(fill = reorder(class, ord), colour = reorder(class, ord)), 
           position = position_fill(), width = 0.5) +
  geom_text(aes(label = paste0(rate, "%")), position = position_fill(.5),
            family = v_font_bm) +
  # geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = tb21,
  #              linetype = "dashed", size = 0.4) +
  scale_x_continuous(breaks = 1:2,
                     label = c("'20.9월", "'22.9월"),
                     expand = expansion(c(0.15, 0.15))) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_colour_brewer(palette = "RdYlBu") +
  labs(title = "그림2. 100%기준 누적막대그래프") +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())



ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_02.png",
       width = 6, height = 4, dpi = 180, units = "in")



################################################################################
################################################################################
################################################################################


ggplot(tb2 %>% filter(ord == 1 | ord == 4), 
       aes(date, rate, group = class, colour = reorder(class, ord))) +
  geom_line() +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_02.png",
       width = 6, height = 3, dpi = 180, units = "in")




################################################################################
################################################################################
################################################################################


ggplot(tb2, aes(date, value, fill = reorder(class, ord))) +
  geom_area() +
  scale_fill_brewer(palette = "RdGy") +
  scale_colour_brewer(palette = "RdGy") +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221129/images/20221129_03.png",
       width = 6, height = 3, dpi = 180, units = "in")
