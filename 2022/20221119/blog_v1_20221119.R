source("core.R")


tb1 = read_excel(path = "./2022/20221119/data2.xls", skip = 3, 
                 col_names = c("일자","국고채", "회사채"))

tb2 = tb1 %>% 
  mutate(bond1 = as.numeric(국고채),
         bond2 = as.numeric(회사채)) %>% 
  mutate(gap = bond2 - bond1) %>% 
  mutate(date = ymd(일자)) %>% 
  select(date, gap)
tb2


ggplot(tb2, aes(date, gap)) +
  geom_line() + 
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_01.png",
       width = 6, height = 4, dpi = 120, units = "in")

tb1

# min(tb2$date)
# max(tb2$date)

sb1 = tibble(date = c("20220103", "20220928", "20221118")) %>% 
  mutate(date = as.Date(date, "%Y%m%d"))

# sb1 = tibble(date = c(min(tb2$date), ymd("20220928"), max(tb2$date)),
#              vjust = c(-2, 2.5, -2))
sb1


tb3 = tb2 %>% 
  inner_join(sb1)
tb3
ggplot() +
  geom_line(data = tb2, aes(date, gap), size = 1) +
  geom_point(data = tb3, aes(date, gap), size = 3, colour = "gray0") +
  theme(axis.title = element_blank())
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_02.png",
       width = 6, height = 4, dpi = 120, units = "in")


tb3

ggplot() +
  geom_line(data = tb2, aes(date, gap), size = 1) +
  geom_point(data = tb3, aes(date, gap), size = 3, colour = "gray0") +
  geom_point(data = tb3, aes(date, gap), size = 1, colour = "gray100") +
  geom_text(data = tb3, aes(date, gap, label = gap), 
            vjust = c(-2, 2.5, -2), family = "BMJUAOTF") +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_03.png",
       width = 6, height = 4, dpi = 120, units = "in")


ggplot() +
  geom_line(data = tb2, aes(date, gap), size = 1) +
  geom_point(data = tb3, aes(date, gap), size = 3, colour = "gray0") +
  geom_point(data = tb3, aes(date, gap), size = 1, colour = "gray100") +
  geom_text(data = tb3, aes(date, gap, label = gap), 
            vjust = c(-2, 2.5, -2), family = "BMJUAOTF") +
  scale_y_continuous(expand = expansion(c(0.2, 0.3))) +
  scale_x_date(expand = expansion(c(0.1, 0.1))) +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_04.png",
       width = 6, height = 4, dpi = 120, units = "in")

ggplot() +
  geom_line(data = tb2, aes(date, gap), size = 1) +
  geom_point(data = tb3, aes(date, gap), size = 4, colour = "gray0") +
  geom_point(data = tb3, aes(date, gap), size = 2, colour = "gray100") +
  geom_text(data = tb3, aes(date, gap, label = gap), 
            vjust = c(-2, 2.5, -2), family = "BMJUAOTF") +
  scale_y_continuous(expand = expansion(c(0.2, 0.3))) +
  scale_x_date(expand = expansion(c(0.1, 0.1)),
               breaks = pull(tb3, "date"),
               date_labels = "%m/%d") +
  theme_bw() +
  theme(axis.title = element_blank())

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_05.png",
       width = 6, height = 4, dpi = 120, units = "in")


sb1 = tibble(date = c(min(tb2$date), ymd("20220928"), max(tb2$date)),
             vjust = c(-2, 1, -2),
             hjust = c(0.5,-0.3,0.5))
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
ggplot() +
  geom_line(data = tb2, aes(date, gap), size = 1.5, colour = "#ED9266") +
  geom_segment(data = tb3, aes(x = date, y = 0.45, xend = date, yend = gap),
               linetype = "dotted", colour = "gray100") +
  geom_point(data = tb3, aes(date, gap), size = 3, colour = "#ED9266") + #F9E19A
  geom_point(data = tb3, aes(date, gap), size = 1.5, colour = v_dark_bgcolor) +
  geom_text(data = tb3, aes(date, gap, label = gap), 
            vjust = sb1$vjust, hjust = sb1$hjust, family = "BMJUAOTF", 
            colour = "gray100", size = 5) +
  scale_y_continuous(expand = expansion(c(0, 0.3))) +
  scale_x_date(expand = expansion(c(0.05, 0.05)),
               breaks = pull(tb3, "date"),
               date_labels = "%b/%e") +
  labs(title = "회사채 신용스프레드",
       subtitle = "(단위 = %포인트)",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "BMJUAOTF") +
  theme(plot.margin = margin(0.3,0.5,0.3,0.5,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "AppleSDGothicNeo-Bold", 
                                  hjust = 0.03, 
                                  size = 20,
                                  margin = margin(0.3, 0, 0, 0,"in")),
        plot.subtitle = element_text(color = "gray100", 
                                  family = "AppleSDGothicNeo-Medium", 
                                  hjust = 0.03, 
                                  size = 12,
                                  margin = margin(0.1, 0, -0.7, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 1, 
                                    size = 10,
                                    margin = margin(0.2,0,0,0,"in")),
        axis.title = element_blank(),
        axis.text.x = element_text(colour = "gray100", size = 12,
                                   margin = margin(0.15,0,0,0,"in")),
        axis.line.x = element_line(colour = "gray100", size = 1))

ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221120/images/20221120_91.png",
       width = 8, height = 4.5, dpi = 120, units = "in")






tb11 = as_tibble(spline(tb2))

ggplot(tb11, aes(x,y)) +
  geom_line(size = 1)







