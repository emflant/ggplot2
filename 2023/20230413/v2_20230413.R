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



g1 = function(fill_colours){
  ggplot(tb3, aes(bond_year, bond_amount, group = bond_year)) +
    geom_col(aes(fill = yn)) +
    geom_label(aes(label = formatC(bond_amount, format="f", big.mark=",", digits = 1),
                   colour = yn), 
               fill = v_dark_bgcolor,
               family = v_font_bm, 
               label.padding = unit(0.5, "lines"),
               label.size = 0,
               nudge_x = 0,
               nudge_y = 3,
               size = tb3$f_size
               # vjust = -0.5
    ) +
    scale_colour_manual(values = fill_colours) +
    scale_fill_manual(values = fill_colours) +
    scale_x_continuous(breaks = 2017:2022) +
    scale_y_continuous(expand = expansion(c(0, 0.2))) +
    theme_void(base_family = v_font_base) +
    theme(plot.background = element_rect(fill = v_dark_bgcolor, color = v_dark_bgcolor),
          legend.position = "none",
          plot.margin = margin(0.3,0.3,0.2,0.2,"in"),
          axis.text.x = element_text(colour = "gray100",
                                     margin = margin(0.1,0,0,0, "in")),
          axis.text.y = element_text(colour = "gray100",
                                     margin = margin(0,0.1,0,0, "in")),
          axis.line.x = element_line(colour = "#6E727E", size = 0.5),
          panel.grid.major.y = element_line(colour = "#6E727E", size = 0.5),)
}


g1(c('#F5E9BF', '#FFE74C'))

ggsave("./2023/20230413/v2_01.png",
       width = 6, height = 4, dpi = 240, units = "in")

g1(c('#EAEAEA', '#8ACBE6'))

ggsave("./2023/20230413/v2_02.png",
       width = 6, height = 4, dpi = 240, units = "in")


g1(c('#EAEAEA', '#53B3CB'))

ggsave("./2023/20230413/v2_03.png",
       width = 6, height = 4, dpi = 240, units = "in")
