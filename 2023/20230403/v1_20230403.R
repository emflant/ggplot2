rm(list = ls())
source('./core.R')



read_bond_file = function(v_file_name, v_date){
  read_excel(v_file_name, skip=2) %>% 
    mutate(date = v_date, 
           year = str_extract(종류명, '\\d+') %>% as.integer,
           num = row_number(),
           rate = as.double(오후)) %>% 
    select(num, date, title = 종류명, year, rate) %>% 
    filter(str_detect(title, '국고채권'))
}

tb1 = read_bond_file('./2023/20230403/bond_202303.xls', '2023-03') %>% 
  union_all(read_bond_file('./2023/20230403/bond_202212.xls', '2022-12')) %>% 
  # union_all(read_bond_file('./2023/20230403/bond_202209.xls', '2022-09')) %>% 
  # union_all(read_bond_file('./2023/20230403/bond_202206.xls', '2022-06')) %>% 
  union_all(read_bond_file('./2023/20230403/bond_202112.xls', '2021-12'))

# tb2303 = read_excel('./2023/20230403/bond_202303.xls', skip=2) %>% 
#   mutate(date = '2023-03', 
#          year = str_extract(종류명, '\\d+') %>% as.integcer,
#          num = row_number(),
#          rate = as.double(오후)) %>% 
#   select(num, date, title = 종류명, year, rate) %>% 
#   filter(str_detect(title, '국고채권'))
  

tb1
write_excel_csv(tb1, file = './2023/20230403/data.csv')

tb1 = readr::read_csv('./2023/20230403/data.csv')

ggplot(tb1, aes(year, rate, colour = date)) +
  theme_bw() +
  theme(title = element_blank())

ggsave("./2023/20230403/v1_00_1.png",
       width = 6, height = 4, dpi = 240, units = "in")

ggplot(tb1, aes(year, rate, colour = date)) +
  theme_bw() +
  theme(title = element_blank(),
        panel.grid.major = element_line(colour = "red"),
        panel.grid.minor = element_line(colour = "purple"))

ggsave("./2023/20230403/v1_00_2.png",
       width = 6, height = 4, dpi = 240, units = "in")

ggplot(tb1, aes(year, rate, colour = date)) +
  geom_line(size = 1) +
  theme_bw()

ggsave("./2023/20230403/v1_01.png",
       width = 6, height = 4, dpi = 240, units = "in")

bk = tb1 %>% distinct(year) %>% pull

ggplot(tb1, aes(year, rate, colour = date)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(1,2,3,5,10,20,30,50),
                     minor_breaks = NULL) +
  theme_bw()

ggsave("./2023/20230403/v1_02.png",
       width = 6, height = 4, dpi = 240, units = "in")

ggplot(tb1, aes(year, rate, colour = date)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = bk) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

ggsave("./2023/20230403/v1_03.png",
       width = 6, height = 4, dpi = 240, units = "in")


ggplot(tb1, aes(year, rate, colour = date)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = bk, minor_breaks = NULL) +
  theme_bw() +
  theme(panel.grid.major.x = element_line(linetype = "dotted",
                                          colour = "gray30"))

ggsave("./2023/20230403/v1_04.png",
       width = 6, height = 4, dpi = 240, units = "in")


#2A2952 #FDFFF6 #배경색
#5F544B #D3D4CE #y-major
#FDD735 #989895 # x/y 축.

#FDD735 #F0928F # line1
#FE6959 #F6C577 # line2
#697984 #57BFC9 # line3

tb2 = tb1 %>% 
  filter(year == 50) %>% 
  mutate(pos_x = 57) %>% 
  mutate(date2 = ymd(paste0(date, '-01'))) %>% 
  mutate(date3 = format(date2, '%b, %Y'))
  
bk2 = as.character(bk)
bk2[2] = ""
bk2[8] = "50Y"
bk2
ggplot(tb1, aes(year, rate, colour = date)) +
  geom_line(size = 1.5) +
  geom_label(data = tb2, aes(label = date3),
             nudge_x = 10,
             fill = "#FDFFF6", label.size = 0.5,
             family = v_font_bm) +
  scale_color_manual(values = c('#F0928F', '#F6C577', '#57BFC9')) +
  scale_x_continuous(breaks = bk, 
                     labels = bk2,
                     limits = c(0, 60)) +
  scale_y_continuous(breaks = seq(1,4), labels = paste0(seq(1,4), '%'),
                     limits = c(1.2,4), expand = expansion(c(0,0))) +
  labs(caption = "twitter @sourcebox7") +
  theme_void(base_family = v_font_bm) +
  theme(plot.background = element_rect(fill = "#FDFFF6", color = "#989895",
                                       ),
        plot.margin = margin(0.4,0.3,0.2,0.2,"in"),
        legend.position = "none",
        axis.line.x = element_line(colour = "#989895", size = 0.5),
        axis.text.x = element_text(colour = "#989895", 
                                   margin = margin(0.1,0,0,0,"in")),
        axis.text.y = element_text(colour = "#989895"),
        panel.grid.major.x = element_line(colour = "#D3D4CE", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "#D3D4CE", size = 0.5),
        plot.caption = element_text(color = "#989895", 
                                    family = "Menlo", 
                                    size = 8,
                                    margin = margin(0.15,0,0,0,"in")))


ggsave("./2023/20230403/v1_12.png",
       width = 6, height = 4, dpi = 240, units = "in")
