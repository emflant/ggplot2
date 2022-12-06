source("./core.R")


data.frame(shape = 0:25) %>% 
  ggplot(aes(0, 0, shape = shape)) +
  geom_point(size = 8, fill = 'red') +
  scale_shape_identity() +
  facet_wrap(~shape) +
  theme_void() + 
  theme(plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = "gray100", color = "gray100"),
        strip.text = element_text(size = 15, margin = margin(0.2,0,0,0,"in")))

ggsave("./2022/20221123/save_01.png",
       width = 8, height = 6, dpi = 120, units = "in")


tb1 = read_csv("./2022/20221123/data.csv")
tb1
tb1$crmm
sb1 = tibble(cd = c("m1_work1", "m2_work1", "m3_work1"),
             cdnm = c("임의경매", "강제경매", "공매공고"),
             ord = c(2,1,3))
sb1

tb2 = tb1 %>% 
  inner_join(sb1, by = c("crmm" = "cd")) %>% 
  pivot_longer(cols = "2021-11":"2022-10") %>% 
  mutate(x = rep(1:12, 3))
tb2

tb2 = tb2 %>% mutate(date = ymd(paste0(name, "-01")))


ggplot(tb2, aes(date, value,
                colour = reorder(cdnm, ord))) +
  geom_line(size = 1) +
  geom_point(aes(shape = reorder(cdnm, ord)), size = 4) +
  scale_x_date(date_labels = "%b") +
  scale_shape_manual(values = 12:14) +
  theme_bw(base_family = v_font_bm) +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 12))

ggsave("./2022/20221123/save_02.png",
       width = 8, height = 4.5, dpi = 120, units = "in")


tb3 = tb2 %>% 
  filter(cdnm == "임의경매") %>% 
  select(x, value)

ggplot(tb3, aes(x,value)) +
  geom_line()

spline(tb3$x, tb3$value, n = 1000 + 1, method = "natural") %>% 
  as_tibble() %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  geom_point(data = tb3, aes(x, value))

ggplot(data = tb3, aes(x, value)) +
  geom_point(data = tb3, aes(x, value)) +
  stat_smooth(se = F, method = "lm", formula = y ~ poly(x, 7))




