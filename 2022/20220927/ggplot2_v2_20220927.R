#F5F8FB

#F5F8FB background
#3070B5 line-colour
#5EC6B5 label-colour  #58BAB5
#7B807D text colour


library(tidyverse)

ll = 1:10
pos = sample(c(T,F), 10, replace = T, prob = c(0.7, 0.3))

pos
replace(ll, pos, NA)


v_vjust = c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,
            1.5,1.5,1.5,1.5,1.5,1.5)

set.seed(10983)
tb1 = tibble(x = 1:12,
       y = cumsum(floor(rnorm(12) * 10)) + 30,
       p = sample(c(T,F), 12, replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(l = replace(y, p, NA)) %>% 
  mutate(xl = replace(month.abb,p, NA))

tb1
v_breaks = which(pull(tb1, p) == F)
v_labels = month.abb[v_breaks]

tb2 = tibble(x = c(4,4), y = c(0, 35))
ggplot(tb1) + geom_line(tb2, aes(x,y))

ggplot(tb1, aes(x,y)) + 
  geom_col(aes(x, y = l), width = 0.03, fill = "gray70",
           na.rm = T, alpha = 0.7) +
  geom_line(colour = "#3070B5", size = 2) + 
  geom_point(colour = "#3070B5", size = 5) + 
  geom_label(aes(label = l), vjust = v_vjust, 
             na.rm = T, size = 10,
             fill = "#58BAB5", family = "BMJUAOTF",
             colour = "gray100", label.padding = unit(3, "mm")) +
  scale_y_continuous(limits = c(0,75)) +
  scale_x_continuous(labels = v_labels, breaks = v_breaks) +
  theme_void(base_family = "BMJUAOTF") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 20),
        plot.margin = margin(0.3,0.3,0.5,0.3,"in"),
        plot.background = element_rect(fill = "#F5F8FB", color = NA))

ggsave("~/github/ggplot2/2022/20220927/save_ggplot_01.png", 
       width = 8, height = 4.5, dpi = 240, units = "in", bg = "#F5F8FB") 



df1 = tibble(x = c(1,2,4), y = c(2,4,3))
df2 = tibble(x = c(2,3), y = c(2,4))

ggplot(df1) + 
  geom_line(aes(x,y)) +
  geom_line(data = df2, aes(x,y))


df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(df, aes(trt, resp, colour = group))
p +
  geom_errorbar(aes(ymin = 4, ymax = 0), width = c(0.1, 1,0.2, 1), linetype = 1)
