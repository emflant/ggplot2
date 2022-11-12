source("./core.R")

tb1 = tibble(x = c(1, 2),
             y = c(3.5, 7.7),
             z = 2) 
  
tb1
tb2 = tibble(x = 1:2,
             x_text = c("10월 물가상승률", "목표치"),
             y_text = c("7.7%", "2%"))

tb3 = tb1 %>% left_join(tb2) 
  # mutate(nrow = row_number())  %>% 
  # mutate(cnt = n()) %>% 
  # mutate(y2 = sum(y)) %>% 
  # mutate(axis_x_text = ifelse(nrow == cnt, x_text, NA)) %>% 
  # mutate(label_y_text = ifelse(nrow == cnt, y_text, NA)) %>% 
  # mutate(segment_y_value = 2) %>% 
  # ungroup()

tb3



ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  theme(axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_01.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray")+
  theme(axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_02.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2) +
  theme(axis.title = element_blank())


ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_03.png", 
       width = 6, height = 4, dpi = 120, units = "in")



ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2,
               linetype = "solid") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"solid\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())


# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_04.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2,
               linetype = "dashed") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"dashed\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())

# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_05.png", 
       width = 6, height = 4, dpi = 120, units = "in")



ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2,
               linetype = "dotted") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"dotted\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())

# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_06.png", 
       width = 6, height = 4, dpi = 120, units = "in")



ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2,
               linetype = "dotdash") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"dotdash\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())

# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_07.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = 0, y = 2, xend = 3, yend = 2,
               linetype = "longdash") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"longdash\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())

# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_08.png", 
       width = 6, height = 4, dpi = 120, units = "in")


ggplot(tb1) +
  geom_col(aes(x,y), width = 0.5) +
  geom_col(aes(x,z), width = 0.5, fill = "gray") +
  geom_segment(x = .5, y = 2, xend = 3, yend = 2,
               linetype = "twodash") +
  annotate("text", x = 0.5, y = 7, label = "linetype = \"twodash\"", 
           hjust = 0,
           size = 7) +
  theme(axis.title = element_blank())

# (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash)
ggsave("~/github/hugo/app/sourcebox-hugo-v0.104/content/post/2022/20221111/images/20221111_09.png", 
       width = 6, height = 4, dpi = 120, units = "in")



################################################################################
################################################################################
################################################################################


tibble(x = c(1, 1, 2),
       y = c(2, 5.7, 2),
       # l = c(NA, "10월 물가상승률", "목표치"),
       l = c(NA, "A", "B"),
       y2 = c(NA, "7.7%", "2%")) %>% 
  ggplot(aes(x,y, fill = reorder(y, -y))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = y2), size = 8,
            position = position_stack(),
            vjust = -1, colour = "gray100",
            na.rm = T,
            family = "BMJUAOTF") +
  scale_y_continuous(expand = expansion(c(0.05,0.5))) +
  scale_x_continuous(expand = expansion(c(0.15,0.15))) +
  annotate("segment", x = 0, xend = 3, y = 2, yend = 2,
           linetype = "dotted", colour = "gray100", size = 0.7) +
  annotate("rect", xmin = 0, xmax = 3, ymin = -2, ymax = 0,
           fill = v_dark_bgcolor, colour = "gray100", size = 0.3) +
  geom_text(aes(y = -1, label = l),
            na.rm = T, size = 7, colour = "gray100",
            family = "BMJUAOTF") + 
  scale_fill_brewer(palette = "RdGy" ) + #"RdGy"
  labs(title = "(ggplot2) geom_col + geom_segment",
       caption = "twitter @sourcebox7") +
  theme_void() +
  theme(plot.margin = margin(0.3,0.3,1,0.3,"in"), 
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        plot.title = element_text(color = "gray100", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.1, 
                                  size = 20,
                                  margin = margin(0.3, 0, -0.3, 0,"in")),
        plot.caption = element_text(color = "gray100", 
                                    family = "Menlo", 
                                    hjust = 0.93, 
                                    size = 12,
                                    margin = margin(0.3,0,-0.5,0,"in")),
        legend.position = "none")

ggsave("./2022/20221111/save_01.png", 
       width = 8, height = 6, dpi = 120, units = "in")

