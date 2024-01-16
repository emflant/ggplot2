rm(list = ls())
source('./core.R')

tb1 = read_excel('./2023/20230413/bond.xls', skip = 1) %>% 
  select(bond_date = 발행일, bond_amount = 발행액)


tb2 = tb1 %>% 
  arrange(bond_date) %>% 
  mutate(bond_date = ymd(bond_date)) %>% 
  mutate(bond_year = year(bond_date)) %>% 
  mutate(bond_month = month(bond_date)) 


round(10.234, 2)

tb2 %>% 
  group_by(bond_year) %>% 
  summarise(bond_amount = sum(bond_amount)) %>% 
  mutate(yn = bond_year == 2022)

tb3 = tb2 %>% 
  group_by(bond_year) %>% 
  summarise(bond_amount = sum(bond_amount) / 10000) %>% 
  mutate(yn = bond_year == 2022) %>% 
  mutate(f_size = c(4.5,4.5,4.5,4.5,4.5,6))
tb3
v_dark_bgcolor = '#3E4353'  # 배경색
v_line_color = '#F7D060'  # 그래프 선
#FF6D60

# https://coolors.co/083d77-ebebd3-f4d35e-ee964b-f95738
# https://coolors.co/6c698d-d4d2d5-bfafa6-aa968a-6e6a6f


fill_colours = c('#F5E9BF', '#FFE74C')

################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col()

ggsave("./2023/20230413/v5_01.png",
       width = 8, height = 4.5, dpi = 240, units = "in")


################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn))

ggsave("./2023/20230413/v5_02.png",
       width = 8, height = 4.5, dpi = 240, units = "in")

################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  scale_x_continuous(breaks = 2017:2022) +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("./2023/20230413/v5_03.png",
       width = 8, height = 4.5, dpi = 240, units = "in")


################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1))+
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("./2023/20230413/v5_04.png",
       width = 8, height = 4.5, dpi = 240, units = "in")

################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             # fill = v_dark_bgcolor,
             # family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             # nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1)) +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("./2023/20230413/v5_05.png",
       width = 8, height = 4.5, dpi = 240, units = "in")

################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             # fill = v_dark_bgcolor,
             # family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1)) +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("./2023/20230413/v5_06.png",
       width = 8, height = 4.5, dpi = 240, units = "in")

################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             # fill = v_dark_bgcolor,
             # family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1)) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave("./2023/20230413/v5_07.png",
       width = 8, height = 4.5, dpi = 240, units = "in")


################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             # fill = v_dark_bgcolor,
             # family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1)) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),)

ggsave("./2023/20230413/v5_08.png",
       width = 8, height = 4.5, dpi = 240, units = "in")


################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1)) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),)

ggsave("./2023/20230413/v5_09.png",
       width = 8, height = 4.5, dpi = 240, units = "in")



################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1),
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5))

ggsave("./2023/20230413/v5_10.png",
       width = 8, height = 4.5, dpi = 240, units = "in")



################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             # label.padding = unit(0.5, "lines"),
             # label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = tb3$f_size
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1),
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5))

ggsave("./2023/20230413/v5_11.png",
       width = 8, height = 4.5, dpi = 240, units = "in")



################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             label.padding = unit(0.5, "lines"),
             label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             # size = c(6,6,6,6,6,8)
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1),
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5))

ggsave("./2023/20230413/v5_12.png",
       width = 8, height = 4.5, dpi = 240, units = "in")




################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             label.padding = unit(0.5, "lines"),
             label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             size = c(6,6,6,6,6,8)
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1),
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5))

ggsave("./2023/20230413/v5_13.png",
       width = 8, height = 4.5, dpi = 240, units = "in")


################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn)) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             label.padding = unit(0.5, "lines"),
             label.size = 0,
             # nudge_x = 0,
             nudge_y = 3,
             size = c(6,6,6,6,6,8)
  ) +
  scale_x_continuous(breaks = 2017:2022) +
  scale_y_continuous(limits = c(0,40+1),
                     expand = expansion(c(0, 0))) +
  scale_colour_manual(values = fill_colours) +
  scale_fill_manual(values = fill_colours) +
  theme_void(base_family = v_font_base) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5))

ggsave("./2023/20230413/v5_14.png",
       width = 8, height = 4.5, dpi = 240, units = "in")

################################
################################
################################
################################
################################

ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn), width = 0.9) +
  geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                 colour = yn),
             fill = v_dark_bgcolor,
             family = v_font_bm,
             label.padding = unit(0.5, "lines"),
             label.size = 0,
             nudge_x = 0,
             nudge_y = 3,
             size = c(6,6,6,6,6,8)
  ) +
  scale_colour_manual(values = fill_colours) +
  scale_fill_manual(values = fill_colours) +
  scale_x_continuous(breaks = 2017:2022, 
                     limits = c(2016.55, 2022.45),
                     expand = expansion(c(0, 0))) +
  scale_y_continuous(limits = c(0,40+1), 
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        legend.position = "none",
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        axis.text.x = element_text(colour = "gray100", size = 13,
                                   margin = margin(0.15,0,0,0, "in")),
        axis.text.y = element_text(colour = "gray100", size = 13,
                                   margin = margin(0,0.1,0,0, "in")),
        axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5),)




ggsave("./2023/20230413/v5_00.png",
       width = 8, height = 4.5, dpi = 240, units = "in")



ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
  geom_col(aes(fill = yn), width = 0.9) +
  # geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
  #                colour = yn),
  #            fill = v_dark_bgcolor,
  #            family = v_font_bm,
  #            label.padding = unit(0.5, "lines"),
  #            label.size = 0,
  #            nudge_x = 0,
  #            nudge_y = 3,
  #            size = tb3$f_size
  # ) +
  scale_colour_manual(values = fill_colours) +
  scale_fill_manual(values = fill_colours) +
  scale_x_continuous(breaks = 2017:2022, 
                     limits = c(2016.55, 2022.45),
                     expand = expansion(c(0, 0))) +
  scale_y_continuous(limits = c(0,40+1), 
                     expand = expansion(c(0, 0))) +
  theme_void(base_family = v_font_base) +
  theme(#plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
        legend.position = "none",
        plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
        # axis. = element_blank(colour = "gray100",
        #                            margin = margin(0.1,0,0,0, "in")),
        # axis.text.y = element_text(colour = "gray100",
        #                            margin = margin(0,0.1,0,0, "in")),
        # axis.line.x = element_line(colour = "#6E727E", size = 0.5),
        # panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5),
        )





ggsave("./2023/20230413/v2_02.png",
       width = 6, height = 4, dpi = 240, units = "in")


g1(c('#EAEAEA', '#53B3CB'))

ggsave("./2023/20230413/v2_03.png",
       width = 6, height = 4, dpi = 240, units = "in")
