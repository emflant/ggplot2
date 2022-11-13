source('./core.R')


tb1 = tibble(x = c(1,3), 
       y = c(4, 7.7))

ggplot(tb1, aes(x,y)) +
  geom_col() +
  xlim(-5, 10) +
  ylim(0, 15) +
  geom_line(aes(x,y), data = tb4)


tb2 = tibble(x = c(1,-3, 0),
             y = c(4, 5, 5)) 


tb2 = tibble(pos = c("x", "y"), start = c(1,4), end = c(-3, 7))
tb2
tb3 = tb2 %>% 
  mutate(middle = ifelse(pos == "x", (start + end)/2 + 1, end))
  
tb3
tb4 = tb3 %>% 
  pivot_longer(cols = 2:4) %>% 
  pivot_wider(names_from = pos, values_from = value)
  
tb4

tb4 = add_line(in_start = c(1,4), in_end = c(-2, 7), 0.5)
tb5 = add_line(in_start = c(3,7.7), in_end = c(6, 10), -1)

ggplot(tb1, aes(x,y)) +
  geom_col() +
  xlim(-5, 10) +
  ylim(0, 15) +
  geom_line(aes(x,y), data = tb4) +
  geom_line(aes(x,y), data = tb5) +
  annotate("text", x = -2.7, y = 7, label = 4) +
  annotate("text", x = 7, y = 10, label = 7.7) +
  # theme_void() +
  theme(axis.title = element_blank())
  



x1 = 11
x2 = 15

# 11,12,13,14,15
11 + 4 * 1/2
11 + 2


