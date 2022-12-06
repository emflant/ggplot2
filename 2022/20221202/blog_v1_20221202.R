source("./core.R")


# https://stat.molit.go.kr/portal/cate/statView.do?hRsId=32&hFormId=5328&hSelectId=5328&hPoint=00&hAppr=1&hDivEng=&oFileName=&rFileName=&midpath=&month_yn=N&sFormId=5328&sStart=202201&sEnd=202210&sStyleNum=1&EXPORT=
tb1 = read_excel("./2022/20221202/data.xlsx") %>% 
  janitor::clean_names()
tb1
tb2 = tb1 %>% filter(gu_bun == "계") %>% 
  pivot_longer(-1) %>% 
  mutate(x = row_number()) %>% 
  filter(x >= 13) %>%
  mutate(x = row_number()) %>% 
  select(-1)

write_csv(tb2, "~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/data.csv")

readr::read_csv("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/data.csv")
tb2 = readr::read_csv("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/data.csv")
tb2


# tb2 = readr::read_csv("data.csv")
tb2 %>% print(n = Inf)
tb11 = tb2 %>% 
  filter(x == 1 | x == max(x)) %>% 
  mutate(height = c(-1.5, -0.5))
tb11 = tb2 %>% 
  filter(x == 1 | x == max(x)) 

tb11

geom_label()
ggplot(data = tb2, mapping = aes(x = x, y = value)) +
  geom_line() + 
  annotate("label", label = "숫자 혹은 date 형식의 필드를 x축으로 설정할때는,\n값의 순서대로 그려진다.", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 3,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_11.png",
       width = 5, height = 3, dpi = 200, units = "in")


ggplot(tb2, aes(name, value)) +
  geom_line() + 
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

# geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?


ggplot(tb2, aes(name, value, value, group = 1)) +
  geom_line() + 
  annotate("label", label = "문자열 필드를 x축으로 설정할때\n순서대로 그래프가 그려지지 않을 수 있음.", 
           x = 3, y = 43000, 
           hjust = 0, family = v_font_base, size = 3,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_21.png",
       width = 5, height = 3, dpi = 200, units = "in")


ggplot(tb2, aes(reorder(name, x), value, group = 1)) +
  geom_line() +
  annotate("label", label = "문자열 필드를 x축으로 설정할때\nreorder 함수를 이용해서 순서대로 그려지도록 정렬한다.", 
           x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 3,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_22.png",
       width = 5, height = 3, dpi = 200, units = "in")

ggplot(tb2, aes(x, value)) +
  geom_line(colour = "blue") + 
  annotate("label", label = "colour = \"blue\"", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_12.png",
       width = 5, height = 3, dpi = 200, units = "in")


colors()





ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440") + 
  annotate("label", label = "colour = \"#EB6440\"", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_13.png",
       width = 5, height = 3, dpi = 200, units = "in")



ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440", size = 3) + 
  annotate("label", label = "size = 3 ", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_14.png",
       width = 5, height = 3, dpi = 200, units = "in")


ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440", size = 10) + 
  annotate("label", label = "size = 10 ", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())






ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_15.png",
       width = 5, height = 3, dpi = 200, units = "in")




ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440", size = 3, alpha = 0.2) + 
  annotate("label", label = "alpha = 0.2 ", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_16.png",
       width = 5, height = 3, dpi = 200, units = "in")


ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440", size = 3, alpha = 0.5) + 
  annotate("label", label = "alpha = 0.5 ", x = 1.5, y = 43000, 
          hjust = 0, family = v_font_base, size = 5,
          label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_17.png",
       width = 5, height = 3, dpi = 200, units = "in")


ggplot(tb2, aes(x, value)) +
  geom_line(colour = "#EB6440", size = 3, alpha = 0.8) + 
  annotate("label", label = "alpha = 0.8 ", x = 1.5, y = 43000, 
           hjust = 0, family = v_font_base, size = 5,
           label.padding = unit(0.5, "lines")) +
  theme(text = element_text(family = v_font_base),
        legend.position = "none",
        axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221203/images/20221203_18.png",
       width = 5, height = 3, dpi = 200, units = "in")










ggplot(tb2, aes(x, value, colour = 1)) +
  geom_line(size = 1.5) +
  geom_segment(data = tb11, aes(x = x, y = 15000, xend = x, yend = value),
               linetype = "dotted") +
  geom_point(data = tb11, size = 2, shape = 21, fill = "gray100") +
  geom_label(data = tb11, aes(label = paste0(prettyNum(value, ","), " 가구")),
            vjust = tb11$height, family = v_font_bm, colour = "gray0",
            label.size = 0) +
  scale_x_continuous(expand = expansion(c(0.2, 0.2)), 
                     breaks = tb11$x,
                     labels = c("'22.1월", "10월")) +
  scale_y_continuous(expand = expansion(c(0, 0.3))) +
  labs(title = "그림1. 미분양 주택수") +
  theme_bw(base_family = v_font_bm) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        # axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


# prettyNum("1000000000", big.mark = ",")


ggsave("./2022/20221202/20221202_01.png",
       width = 4, height = 2.5, dpi = 240, units = "in")












9/4
set.seed(1949)
a = sample(1000000:100000000, 10)
format(a, big.mark = ",")

formatC(a, big.mark = ",", format = "f")


## re-cycle arguments
sprintf("%s %d", "test", 1:3)

sprintf("%1.f", 101)


sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%e", pi)
sprintf("%E", pi)
sprintf("%g", pi)
sprintf("%g",   1e6 * pi) # -> exponential
sprintf("%.9g", 1e6 * pi) # -> "fixed"
sprintf("%G", 1e-6 * pi)

## re-use one argument three times, show difference between %x and %X
sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify = "right"))

