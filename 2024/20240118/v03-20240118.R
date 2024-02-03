source('./geo_core.R')

v_fill_color = "#475368"

x = st_point(c(0,1))
y = st_point(c(1,1))

b = st_sfc(x,y)

b = st_buffer(b, dist = 1)

intersection = st_intersection(b[1], b[2])
union = st_union(b[1], b[2])
difference = st_difference(b[1], b[2])
sym_difference = st_sym_difference(b[1], b[2])







g1 = ggplot() +
  geom_sf(data = intersection, fill = "#475368") +
  geom_sf(data = b, colour = "gray0", fill = NA, linewidth = 0.5) +
  scale_y_continuous(expand = expansion(c(0.05, 0.1))) +
  annotate("text", x = -1, y = 2.2, label = "st_intersection(x,y)", 
           hjust = 0, family = v_font_bold2, size = 7) +
  annotate("text", x = -0.5, y = 1, label ="x", size = 10, family = v_font_bold) +
  annotate("text", x = 1.5, y = 1, label ="y", size = 10, family = v_font_bold) +
  annotate("text", x = 0.5, y = 0.3, label = v_water_mark,
           colour = "gray0",size = 7, alpha = 0.5,
           family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank())




g2 = ggplot() +
  geom_sf(data = union, fill = v_fill_color) +
  geom_sf(data = b, colour = "gray0", fill = NA, linewidth = 0.5) +
  scale_y_continuous(expand = expansion(c(0.05, 0.1))) +
  annotate("text", x = -1, y = 2.2, label = "st_union(x,y)", 
           hjust = 0, family = v_font_bold2, size = 7) +
  annotate("text", x = -0.5, y = 1, label ="x", size = 10, colour = "gray100", family = v_font_bold) +
  annotate("text", x = 1.5, y = 1, label ="y", size = 10, colour = "gray100", family = v_font_bold) +
  annotate("text", x = 0.5, y = 0.3, label = v_water_mark,
           colour = "gray0",size = 7, alpha = 0.5,
           family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank())



g3 = ggplot() +
  geom_sf(data = difference, fill = v_fill_color) +
  geom_sf(data = b, colour = "gray0", fill = NA, linewidth = 0.5) +
  scale_y_continuous(expand = expansion(c(0.05, 0.1))) +
  annotate("text", x = -1, y = 2.2, label = "st_difference(x,y)", 
           hjust = 0, family = v_font_bold2, size = 7) +
  annotate("text", x = -0.5, y = 1, label ="x", size = 10, colour = "gray100", family = v_font_bold) +
  annotate("text", x = 1.5, y = 1, label ="y", size = 10, family = v_font_bold) +
  annotate("text", x = 0.5, y = 0.3, label = v_water_mark,
           colour = "gray0",size = 7, alpha = 0.5,
           family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank())




g4 = ggplot() +
  geom_sf(data = sym_difference, fill = v_fill_color) +
  geom_sf(data = b, colour = "gray0", fill = NA, linewidth = 0.5) +
  scale_y_continuous(expand = expansion(c(0.05, 0.1))) +
  annotate("text", x = -1, y = 2.2, label = "st_sym_difference(x,y)", 
           hjust = 0, family = v_font_bold2, size = 7) +
  annotate("text", x = -0.5, y = 1, label ="x", size = 10, colour = "gray100", family = v_font_bold) +
  annotate("text", x = 1.5, y = 1, label ="y", size = 10, colour = "gray100", family = v_font_bold) +
  annotate("text", x = 0.5, y = 0.3, label = v_water_mark,
           colour = "gray0",size = 7, alpha = 0.5,
           family = v_font_heavy) +
  theme_bw(base_family = v_font_bold) +
  theme(axis.title = element_blank())

# g4
g1 + g2 + g3 + g4 + 
  patchwork::plot_layout(ncol = 2)
 
ggsave(filename = "./2024/20240118/v03-01.png", 
       device = grDevices::png,
       width = 12, height = 9, dpi = 180, units = "in")

