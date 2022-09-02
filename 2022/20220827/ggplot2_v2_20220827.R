library(tidyverse)
library(patchwork)
library(readxl)
library(lubridate)
library(ggchicklet)
library(treemapify)



#######################################################################
#######################################################################
#######################################################################

plot_treemap = function(){
  
  tb1 = tibble(x = LETTERS[1:9], y = c(500, 40, 59, 29, 39, 98, 110, 5, 10)) %>% 
    mutate(ord = rank(y, ties.method = "min")) %>% 
    mutate(max_value = ord == 9) %>% 
    mutate(font_size = as.integer(ord * 4) ) %>% 
    mutate(rate = round(y / sum(y) * 100, 1)) %>% 
    mutate(rate_label = paste0(rate, "%"))
  tb1
  v_background_color = "#334960"
  v_direction = 1
  v_font_color = c("gray100", v_background_color, v_background_color, #abc
                   v_background_color, v_background_color, v_background_color, #edf
                   "gray100", v_background_color, v_background_color) #ghi
  
  tb1 %>% 
    mutate(ord = rank(y, ties.method = "min")) %>% 
    ggplot(aes(area = y, fill = factor(ord))) +
    geom_treemap(start = "topleft", 
                 # layout = "srow",
                 colour = v_background_color, size = 10) +
    geom_treemap_text(aes(label = x), 
                      size = 30,
                      start = "topleft",
                      colour = v_font_color,
                      padding.x = unit(0.2, "in"),
                      padding.y = unit(0.2, "in"),
                      family = "BMJUAOTF", min.size = 10) +
    geom_treemap_text(aes(label = rate_label),
                      size = tb1$font_size,
                      start = "topleft",
                      colour = v_font_color, 
                      padding.x = unit(0.15, "in"),
                      padding.y = unit(0.2, "in"),
                      place = "bottomright", 
                      family = "BMJUAOTF",
                      min.size = 10) +
    scale_fill_brewer(palette = "RdBu", direction = v_direction) +
    # labs(title = "Treemap Chart") +
    theme_void(base_family = "BMJUAOTF") +
    theme(legend.position = "none",
          plot.title = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    face = "bold",
                                    hjust = 0.01, 
                                    size = 20,
                                    margin = margin(0.1,0,0.2,0,"in")),
          plot.margin = margin(0.3,0.3,0.3,0.3,"in"))
  
}

plot_treemap()
# ggsave("~/github/ggplot2/2022/20220827/save_ggplot_temp_treemap.png", 
#        width = 8, height = 5, dpi = 240, units = "in")

#######################################################################
#######################################################################
#######################################################################

plot_ring = function(in_point_size = 4){
  
  v_point_size = in_point_size
  v_background_color = "#334960"
  
  tb2 = expand.grid(g = 1:6, x = 1:4) %>% 
    as_tibble() %>% 
    arrange(g,x) %>% 
    mutate(y = sample(20:100, n(), replace = T))
  
  ggplot(tb2, aes(x,y, fill= factor(x))) +
    geom_col(width = 0.7) +
    # geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
    # geom_point(aes(color = factor(x)), size = v_point_size) +
    geom_text(aes(x = -1.5, y = 0, label = LETTERS[g]), 
              colour = "gray100", family = "BMJUAOTF",
              size = 10) +
    # geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = "gray100",
    #           # fontface = "bold", 
    #           size = 2.5, nudge_y = c(2,1.5,1.2,1),
    #           family = "BMJUAOTF") +
    scale_fill_brewer(palette = "RdBu", direction = 1) +
    scale_color_brewer(palette = "RdBu", direction = 1) +
    xlim(c(-2, 5)) +
    coord_polar(theta = "y") +
    facet_wrap(~g) + 
    # labs(title = "ggplot2 - Ring Chart") +
    theme_void() +
    theme(legend.position = "none", 
          strip.text = element_blank(),
          panel.spacing = unit(-0.3, "in"),
          plot.title = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    face = "bold",
                                    hjust = 0.05, 
                                    size = 20,
                                    margin = margin(0.3,0,-0.2,0,"in")),
          plot.margin = margin(0.1,0.1,0.1,0.1,"in"))
}

plot_ring()


#######################################################################
#######################################################################
#######################################################################

# plot_donut1 = function(){
#   v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
#   v_font_color = c("gray90","#334960","#334960","#334960","gray90")
#   v_palette = "RdBu"
#   v_direction = -1
#   
#   tibble(x = 1:5, y = c(75, 64, 114, 36, 38),
#               a = c(1,1,1,0,1)) %>% 
#     mutate(z = y / 4 + 30) %>% 
#     ggplot() + 
#     geom_col(aes(x,z, fill = factor(x)), width = 1, 
#              colour = v_background_color, size = 2) +
#     geom_col(aes(x,y = 30), width = 1, fill = v_background_color) +
#     geom_text(aes(x, y/10 + 30 + a), label = LETTERS[1:5],
#               family = "BMJUAOTF", size = 10,
#               color = v_font_color) +
#     coord_polar(theta = "x") +
#     # "#9F9EBF", "#7070A2"
#     scale_fill_brewer(palette = v_palette, direction = v_direction) +
#     theme_void(base_family = "BMJUAOTF") +
#     theme(legend.position = "none")
# }


plot_donut2 = function(){
  v_background_color = "#334960" #2E5E77 #2D4A62 #334960 #28384E 
  v_font_color = c("gray90","#334960","#334960","#334960","gray90")
  v_palette = "RdBu"
  v_direction = -1
  
  tibble(w = c(2,4,1,2,1),
              h = rep(1,5),
              y = c(1.4,1,1,1,1),
              a = c(0.03,-0.06,0,0,0.04)) %>% 
    mutate(w1 = cumsum(w)) %>% 
    mutate(w2 = lag(w1, 1, default = 0)) %>% 
    mutate(x = w / 2 + w2) %>% 
    ggplot(aes(x,y,
               width = w, height = h,
               fill = factor(x))) +
    geom_tile(size = 3, colour = v_background_color) +
    geom_text(aes(x,y + a), label = LETTERS[1:5],
              family = "BMJUAOTF", size = 10,
              colour = v_font_color) +
    ylim(c(-1.5, 2)) +
    coord_polar(theta = "x") +
    scale_fill_brewer(palette = v_palette, direction = v_direction) +
    
    theme_void() +
    theme(legend.position = "none")
}

plot_donut1()
plot_donut2()


#######################################################################
#######################################################################
#######################################################################



plot_bar = function(){
  v_background_color = "#334960"
  v_font_color = "gray100"
  v_bar_count = 6
  
  
  set.seed(1001)
  tibble(x = 1:v_bar_count, y = sample(20:60, v_bar_count), z = rep(letters[1:2], v_bar_count/2)) %>% 
    ggplot(aes(x,y)) +
    geom_col(aes(x = 1:v_bar_count, y = 60), fill = "#2D4054", width = 0.7) +
    geom_col(aes(fill = z), width = 0.7) +
    geom_text(aes(label = paste0("$",y)), vjust = 2, colour = v_font_color,
              family = "BMJUAOTF", size = 7) +
    scale_fill_manual(values = c("#B2182B", "#2166AC")) +
    scale_x_continuous(breaks = 1:v_bar_count, labels = LETTERS[1:v_bar_count],
                       expand = expansion(c(0.03,0.03))) +
    scale_y_continuous(expand = expansion(c(0.05,0.05))) +
    theme_void(base_family = "BMJUAOTF") +
    theme(legend.position = "none",
          axis.text.x = element_text(colour = v_font_color,
                                     size = 30),
          plot.title = element_text(color = "gray90", 
                                    family = "Menlo", 
                                    face = "bold",
                                    hjust = 0.05, 
                                    size = 20,
                                    margin = margin(0.1,0,0.2,0,"in")),
          plot.margin = margin(0.3,0.3,0.3,0.3,"in"))
}

plot_bar()






#######################################################################
#######################################################################
#######################################################################


plot_spacer() +
  theme(plot.background = element_rect(fill = NA, color = NA)) +
  inset_element(plot_donut2(), left = 0, right = 0.5, bottom = 0.47, top = 1.02) +
  inset_element(plot_treemap(), left = 0.5, right = 1, bottom = 0.035, top = 0.48) +
  inset_element(plot_ring(3.8), left = 0.5, right = 1, bottom = 0.5, top = 1) +
  inset_element(plot_bar(), left = 0, right = 0.5, bottom = 0, top = 0.5) +
  
  
  plot_annotation(caption = "twitter @sourcebox7",
                  title = "ggplot2 DashBoard",
                  theme = theme(plot.title = element_text(color = "gray90", 
                                                          family = "Menlo", 
                                                          face = "bold",
                                                          hjust = 0.05, 
                                                          size = 20*1.5,
                                                          margin = margin(0.3,0,-0.1,0,"in")),
                                plot.caption = element_text(color = "gray90", 
                                                            family = "Menlo", 
                                                            hjust = .97, 
                                                            size = 10*1.5,
                                                            margin = margin(-0.3,0,0.3,0,"in")),
                                plot.margin = margin(0.3,0.3,0.3,0.3,"in"),
                                plot.background = element_rect(fill = v_background_color, color = v_background_color)
                  ))


ggsave("~/github/ggplot2/2022/20220827/save_ggplot_01.png", 
       width = 8*2, height = 6*2, dpi = 120, units = "in") 






#######################################################################
#######################################################################
#######################################################################




v_point_size = 4
v_background_color = "#334960"

tb2 = expand.grid(g = 1:6, x = 1:2) %>% 
  as_tibble() %>% 
  arrange(g,x) %>% 
  mutate(y = sample(20:100, n(), replace = T))

ggplot(tb2, aes(x,y, fill= factor(x))) +
  geom_col(width = 0.7) +
  # geom_point(aes(x = x, y = 0, color = factor(x)), size = v_point_size) +
  # geom_point(aes(color = factor(x)), size = v_point_size) +
  geom_text(aes(x = -1.5, y = 0, label = LETTERS[g]), 
            colour = "gray100", family = "BMJUAOTF",
            size = 10) +
  # geom_text(aes(x = x + 0.05, y = 0, label = paste0(y, "%")), colour = "gray100",
  #           # fontface = "bold", 
  #           size = 2.5, nudge_y = c(2,1.5,1.2,1),
  #           family = "BMJUAOTF") +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  scale_color_brewer(palette = "RdBu", direction = 1) +
  xlim(c(-2, 5)) +
  ylim(c(0, 200)) +
  coord_polar(theta = "y", start = 270 * pi /180) +
  facet_wrap(~g) + 
  # labs(title = "ggplot2 - Ring Chart") +
  theme_void() +
  theme(legend.position = "none", 
        strip.text = element_blank(),
        panel.spacing = unit(-0.3, "in"),
        plot.title = element_text(color = "gray90", 
                                  family = "Menlo", 
                                  face = "bold",
                                  hjust = 0.05, 
                                  size = 20,
                                  margin = margin(0.3,0,-0.2,0,"in")),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"))

plot_ring()
