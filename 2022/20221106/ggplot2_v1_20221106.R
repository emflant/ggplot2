source("./core.R")


scale_fill_brewer()

#FDFCFB
v_background_color ="#F5F8FB"


tb1 = tibble(x = c("20220401","20220501","20220601","20220701","20220801","20220901","20221001"),
       y = c(-24.77, -16.06, -24.97, -50.89, -93.94, -37.78, -66.96)) %>% 
  mutate(x = ymd(x))

ggplot(tb1, aes(x,y)) +
  geom_col(width = 20, fill = "#3690c0") +
  geom_text(aes(label = y), vjust = 1.7, hjust = 0.52,
            family = "BMJUAOTF", size = 5,
            colour = "gray10") +
  # scale_x_continuous(position = "top") +
  scale_x_date(position = "top",
               date_breaks = "1 months",
               date_labels = "%b") +
  scale_y_continuous(expand = expansion(c(0.1,0))) +
  labs(title = "ggplot2 - Bar Chart",
       caption = "twitter @sourcebox7") +
  theme_void(base_family = "ArialRoundedMTBold") + #ArialRoundedMTBold  Arial-BoldMT
  theme(plot.margin = margin(0.3,0.5,0.3,0.5,"in"), 
        plot.background = element_rect(fill = v_background_color, color = v_background_color),
        plot.title = element_text(color = "gray0", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.03, 
                                  size = 20,
                                  margin = margin(0.3, 0, 0.4, 0,"in")),
        plot.caption = element_text(color = "gray0", 
                                    family = "Menlo", 
                                    hjust = 0.96, 
                                    size = 12,
                                    margin = margin(0.1,0,0.2,0,"in")),
        axis.text.x.top = element_text(margin = margin(0,0,0.15,0,"in"),
                                       size = 14),
        axis.line.x.top = element_line(colour = "gray10"))


ggsave("./2022/20221106/save_01.png", 
       width = 8, height = 6, dpi = 180, units = "in")


