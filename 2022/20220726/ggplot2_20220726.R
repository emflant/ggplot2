library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)

Sys.timezone()
Sys.getlocale() #[1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"

Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
Sys.setlocale("LC_ALL", "en_US.UTF-8")


tb_covid1 = read_excel(path = "~/github/ggplot2/2022/20220726/covid19_220726.xlsx")
# mutate(wday = wday(naljja, label = T), .after = "naljja") %>% 
#   mutate(week = epiweek(naljja), .after = "wday") %>% 

tb_covid2 = tb_covid1 %>% 
  rename(date = 일자,  total = `계(명)`, local = `국내발생(명)`, 
         imported = `해외유입(명)`, deaths = `사망(명)`) %>% 
  tail(-1) %>% 
  mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) %>% 
  mutate_at(3:5, str_replace, pattern = "-", replacement ="0") %>% 
  mutate_at(3:5, as.double) %>% 
  mutate(wday = wday(date, label = T), .after = date) %>% 
  mutate(month = month(date), .after = "wday") %>% 
  mutate(week = isoweek(date), .after = "month")

tb_covid2


tb_covid_2207 = tb_covid2 %>% filter(date >= ymd('2022-07-26') - days(7-1))
tb_covid_2207 = tb_covid2 %>% filter(date >= ymd('2022-07-26') - days(14-1))
# tb_covid_2207 = tb_covid2 %>% filter(date >= ymd('2022-07-26') - months(1) + days(1))

tb_covid_2207$total
v_colors = c("#18587A", "#FC624D")
v_colors = c("#827397", "#363062")
v_colors = c("#396EB0", "#2E4C6D")
v_colors = c("#95ADBE", "#574F7D")

# seq(0, 200000, 25000)

g_covid = function(in_colors = c("#18587A", "#FC624D")){
  ggplot(tb_covid_2207, aes(date, total)) +
    geom_col(aes(fill = date == max(date)), width = 0.6) +
    scale_y_continuous(breaks = seq(0, 200000, 25000),
                       labels = formatC(seq(0, 200000, 25000), format="f", big.mark=",", digits = 0)) +
    scale_x_date(date_labels = "%d(%a)", date_breaks = "1 days") +
    scale_fill_manual(values = in_colors) +
    labs(y = "확진자수(명)") +
    theme_minimal(base_family = "AppleSDGothicNeo-Bold") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0, vjust = 1.07,size = 8,
                                      margin = margin(0,-1.2,0,0,"cm")),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = .1),
          plot.margin = margin(1.3,1,1,.8, "cm"),
          plot.background = element_rect(fill = "#F1F0EA", color = "#F1F0EA"))  
}


# formatC(1000000, format="f", big.mark=",", digits = 0)

g_covid(c("#18587A", "#FC624D"))
ggsave("~/github/ggplot2/2022/20220726/save_ggplot_1.png", 
       width = 6, height = 4, dpi = 320, units = "in")
 
g_covid(c("#827397", "#363062"))
ggsave("~/github/ggplot2/2022/20220726/save_ggplot_2.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_covid(c("#396EB0", "#2E4C6D"))
ggsave("~/github/ggplot2/2022/20220726/save_ggplot_3.png", 
       width = 6, height = 4, dpi = 320, units = "in")

g_covid(c("#95ADBE", "#574F7D"))
ggsave("~/github/ggplot2/2022/20220726/save_ggplot_4.png", 
       width = 6, height = 4, dpi = 320, units = "in")
