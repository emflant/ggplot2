source('./geo_core.R')

polygon_square = function(x, y, gap) {
  p1 = c(x, y) 
  p2 = c(x, y + gap)
  p3 = c(x + gap, y + gap)
  p4 = c(x + gap, y)
  p5 = c(x, y) 
  
  st_polygon(list(rbind(p1, p2, p3, p4, p5)))  
}


gg2 = function(con, v_title){
  ggplot() +
    # geom_sf(data = sf_grid1, fill = NA) +
    geom_sf(data = sf_grid1[con,], fill = "#DB8582") +
    geom_sf(data = sf_sam1, fill = NA) +
    geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 7, alpha = 0.2,
              family = v_font_heavy) +
    coord_sf(xlim = c(0.5,11.5), ylim = c(0.5,11.5), expand = F) +
    labs(title = v_title) +
    theme_bw(base_family = v_font_bold2) +
    theme(axis.title = element_blank(),
          plot.title = element_text(family = "Menlo-Bold"))
}
gg2(log_within)

grid1 = expand.grid(x = seq(1, 10, by = 1), 
                    y = seq(1, 10, by = 1))
grid1
sf_grid1 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1)) |> 
  pull(geometry) |> 
  st_sfc() |> 
  st_sf(geometry = _)


g_grid = ggplot() +
  geom_sf(data = sf_grid1, fill = "gray70") +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,11.5), ylim = c(0.5,11.5), expand = F) +
  labs(title = "A") +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.title = element_text(family = "Menlo-Bold"))

g_grid

sf_sam1 = list(rbind(c(3,7), c(9,9), c(8,3), c(3,7))) |> 
  st_polygon() |> 
  st_sfc() |> 
  st_sf(geometry = _)

sf_sam1

g_triangle = ggplot() +
  # geom_sf(data = sf_grid1, fill = NA) +
  # geom_sf(data = sf_grid1) + # , fill = "#DB8582"
  geom_sf(data = sf_sam1, fill = "gray70") +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,11.5), ylim = c(0.5,11.5), expand = F) +
  labs(title = "B") +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.title = element_text(family = "Menlo-Bold"))

g_triangle




log_within = st_within(sf_grid1, sf_sam1) |> 
  lengths() > 0

g_within = gg2(log_within, "st_within(A,B)")

log_touches = st_touches(sf_grid1, sf_sam1) |> 
  lengths() > 0

g_touches = gg2(log_touches, "st_touches(A,B)")

log_overlaps = st_overlaps(sf_grid1, sf_sam1) |> 
  lengths() > 0

g_overlaps = gg2(log_overlaps, "st_overlaps(A,B)")


log_intersects = st_intersects(sf_grid1, sf_sam1) |> 
  lengths() > 0

g_intersects = gg2(log_intersects, "st_intersects(A,B)")



log_disjoint = st_disjoint(sf_grid1, sf_sam1) |> 
  lengths() > 0

g_disjoint = gg2(log_disjoint, "st_disjoint(A, B)")



g_final = ggplot() +
  geom_sf(data = sf_grid1[log_disjoint,], fill = "gray70") +
  geom_sf(data = sf_grid1[log_within,], fill = "#DB8582") +
  geom_sf(data = sf_sam1, fill = NA) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,11.5), ylim = c(0.5,11.5), expand = F) +
  labs(title = "st_within + st_disjoint") +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.title = element_text(family = "Menlo-Bold"))

g_final2 = ggplot() +
  geom_sf(data = sf_grid1[log_disjoint,], fill = "gray70") +
  geom_sf(data = sf_grid1[log_intersects,], fill = "#DB8582") +
  geom_sf(data = sf_sam1, fill = NA) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,11.5), ylim = c(0.5,11.5), expand = F) +
  labs(title = "st_intersects + st_disjoint") +
  theme_bw(base_family = v_font_bold2) +
  theme(axis.title = element_blank(),
        plot.title = element_text(family = "Menlo-Bold"))

g_grid
g_triangle
g_within
g_touches
g_overlaps
g_intersects
g_disjoint
g_final
g_final2



g_grid +
  g_triangle +
  g_within +
  g_touches +
  g_overlaps +
  g_intersects +
  g_disjoint +
  g_final +
  g_final2 + 
  plot_layout(ncol = 3)


ggsave(filename = "./2024/20240214/v05-01.png", 
       device = grDevices::png,
       width = 10, height = 10, dpi = 100, units = "in")
