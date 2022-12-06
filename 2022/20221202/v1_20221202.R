source("./core.R")


# https://stat.molit.go.kr/portal/cate/statView.do?hRsId=32&hFormId=5328&hSelectId=5328&hPoint=00&hAppr=1&hDivEng=&oFileName=&rFileName=&midpath=&month_yn=N&sFormId=5328&sStart=202201&sEnd=202210&sStyleNum=1&EXPORT=
tb1 = read_excel("./2022/20221202/data.xlsx") %>% 
  janitor::clean_names()
tb1
tb2 = tb1 %>% filter(gu_bun == "계") %>% 
  pivot_longer(-1) %>% 
  mutate(x = row_number()) %>% 
  filter(x >= 13) %>%
  select(-1)
tb2 %>% print(n = Inf)
tb11 = tb2 %>% 
  filter(x == 13 | x == max(x)) %>% 
  mutate(height = c(-1.5, -0.5))

ggplot(tb2, aes(x, value, colour = "")) +
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

