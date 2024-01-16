rm(list = ls())
source('./core.R')

tb1 = tibble(x = c(7.9, 42.1, 35.5, 13.2, 1.3, 100)) 

ggplot(tb1, aes(1, x, fill = factor(x))) +
  geom_col(colour = 'gray100', size = 1) +
  geom_text(aes(label = x), position = position_stack())

# https://coolors.co/264653-2a9d8f-e9c46a-f4a261-e76f51

# c('#264653', '#2a9d8f', '#e9c46a', '#f4a261', '#e76f51')

g1 = ggplot(tb1, aes(1, x, fill = factor(x))) +
  geom_col(colour = 'gray100', size = 1) +
  geom_text(aes(label = paste0(x, '%')), 
            colour = c('gray100', 'gray100', 'gray100', 'gray100', 'gray0', 'gray100'),
            position = position_stack(0.5),
            hjust = c(0,0,0,0,-2.5,0) +0.5,
            # vjust = c(0,0,0,0,0.5,0),
            family = v_font_bm, 
            size = c(5.5,10,7,5.5,5.5,5.5)) +
  coord_polar(theta = 'y', start = 90 * pi / 180) +
  scale_x_continuous(limits = c(-0.2, 1.5)) +
  scale_fill_manual(values = c('#264653', '#2a9d8f', '#e9c46a', '#f4a261', '#e76f51', 'gray100')) +
  theme_void(base_family = v_font_bm) +
  theme(axis.title = element_blank(),
        legend.position = "none")

plot_spacer() +
  theme(plot.background = element_rect(fill = NA, color = NA)) +
  inset_element(g1, left = 0, bottom = -0.8, right = 1, top = 1) +
  plot_annotation(caption = "twitter @sourcebox7",
                  title = "ggplot2 - Semi circle donut chart",
                  theme = theme(plot.title = element_text(color = "gray30", 
                                                          family = "Menlo", 
                                                          face = "bold",
                                                          hjust = 0.05, 
                                                          size = 20,
                                                          margin = margin(0.3,0,-0.5,0,"in")),
                                plot.caption = element_text(color = "gray0", 
                                                            family = "Menlo", 
                                                            hjust = .95, 
                                                            size = 10,
                                                            margin = margin(-0.2,0,0.3,0,"in")),
                                plot.margin = margin(0,0,0,0,"in"),
                                plot.background = element_rect(fill = 'gray100', color = NA)
                  ))

ggsave(paste0("./2023/20230421/v1_", as.integer(now()), '.png'),
       width = 8, height = 4.5, dpi = 320, units = "in")


as.Date.numeric(now(), origin = "1899-12-30")
as.integer(now())
