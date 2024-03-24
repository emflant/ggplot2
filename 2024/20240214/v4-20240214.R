source('./geo_core.R')

polygon_square = function(x, y, gap) {
  p1 = c(x, y) 
  p2 = c(x, y + gap)
  p3 = c(x + gap, y + gap)
  p4 = c(x + gap, y)
  p5 = c(x, y) 
  
  st_polygon(list(rbind(p1, p2, p3, p4, p5)))  
}


gg2 = function(con){
  ggplot() +
    # geom_sf(data = sf_grid1, fill = NA) +
    geom_sf(data = sf_grid1[con,], fill = "#DB8582") +
    geom_sf(data = sf_sam1, fill = NA) +
    geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
              colour = "gray0",size = 7, alpha = 0.2,
              family = v_font_heavy) +
    coord_sf(xlim = c(0.5,21.5), ylim = c(0.5,11.5), expand = F) +
    theme_bw(base_family = v_font_bm) +
    theme(axis.title = element_blank(),
          # panel.grid = element_blank(),
          plot.margin = margin(0,0,0.1,0,"in"))
}
gg2(log_within)

grid1 = expand.grid(x = seq(1, 20, by = 1), 
                    y = seq(1, 10, by = 1))
grid1
sf_grid1 = tibble(grid1) |> 
  mutate(geometry = map2(x,y, polygon_square, gap = 1)) |> 
  pull(geometry) |> 
  st_sfc() |> 
  st_sf(geometry = _)

sf_sam1 = list(list(rbind(c(3,7), c(9,9), c(8,3), c(3,7))),
               list(rbind(c(13,4), c(17,9), c(19,3), c(13,4)))) |> 
  st_multipolygon() |> 
  st_sfc() |> 
  st_sf(geometry = _)

sf_sam1

#####################################################
# sam2 는 
p1 = list(rbind(c(3,7), c(9,9), c(8,3), c(3,7))) |> 
  st_polygon()

p2 = list(rbind(c(13,4), c(17,9), c(19,3), c(13,4))) |> 
  st_polygon()

sf_sam2 = st_sfc(p1, p2) |> st_sf()

# rm(sf_sam2)

sf_sam2
#####################################################

# log_within = st_within(sf_grid1, sf_sam2) |> 
#   lengths() > 0
# gg2(log_within)
# 
# 
#E5E5E5
##E5E5E5
ggplot() +
  # geom_sf(data = sf_grid1, fill = NA) +
  # geom_sf(data = sf_grid1) + # , fill = "#DB8582"
  geom_sf(data = sf_sam1) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,21.5), ylim = c(0.5,11.5), expand = F) +
  theme_bw(base_family = v_font_bm) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0,0,0.1,0,"in"))

ggsave(filename = "./2024/20240214/v04-00.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")


ggplot() +
  geom_sf(data = sf_grid1) + # , fill = "#DB8582"
  # geom_sf(data = sf_sam1, fill = NA) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.2,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,21.5), ylim = c(0.5,11.5), expand = F) +
  theme_bw(base_family = v_font_bm) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0,0,0.1,0,"in"))

ggsave(filename = "./2024/20240214/v04-01.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")


log_within = st_within(sf_grid1, sf_sam1) |> 
  lengths() > 0

st_within(sf_grid1, sf_sam1) |> print(n = Inf)
log_within

ggplot() +
  geom_sf(data = sf_sam1, fill = "black", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_within,], fill = "red", alpha = 0.5) +
  theme_bw()
  

gg2(log_within)
ggsave(filename = "./2024/20240214/v04-02.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")

log_touches = st_touches(sf_grid1, sf_sam1) |> 
  lengths() > 0

gg2(log_touches)

ggsave(filename = "./2024/20240214/v04-03.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")


log_overlaps = st_overlaps(sf_grid1, sf_sam1) |> 
  lengths() > 0
st_overlaps(sf_grid1, sf_sam1, sparse = F) |> print(n = Inf)
gg2(log_overlaps)

ggsave(filename = "./2024/20240214/v04-04.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")


log_intersects = st_intersects(sf_grid1, sf_sam1) |> 
  lengths() > 0

gg2(log_intersects)

ggsave(filename = "./2024/20240214/v04-05.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")


log_disjoint = st_disjoint(sf_grid1, sf_sam1) |> 
  lengths() > 0

gg2(log_disjoint)

ggsave(filename = "./2024/20240214/v04-06.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")

# log_disjoint = st_disjoint(sf_grid1, sf_sam2) |> 
#   lengths() > 0
# 
# gg2(log_disjoint)





ggplot() +
  geom_sf(data = sf_sam1) +
  geom_sf(data = sf_grid1[log_disjoint,], fill = "black", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_overlaps,], fill = "blue", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_touches,], fill = "green", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_within,], fill = "red", alpha = 0.5) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,21.5), ylim = c(0.5,11.5), expand = F) +
  theme_bw(base_family = v_font_bm) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0,0,0.1,0,"in"))

ggsave(filename = "./2024/20240214/v04-11.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")
#E48B88
ggplot() +
  geom_sf(data = sf_sam1) +
  geom_sf(data = sf_grid1[log_disjoint,], fill = "black", alpha = 0.5) +
  # geom_sf(data = sf_map2[log_overlaps,], fill = "green", alpha = 0.5) +
  # geom_sf(data = sf_map2[log_touches,], fill = "green", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_within,], fill = "red", alpha = 0.5) +
  geom_text(data = get_center(sf_grid1), aes(x,y), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(0.5,21.5), ylim = c(0.5,11.5), expand = F) +
  theme_bw(base_family = v_font_bm) +
  theme(axis.title = element_blank(),
        plot.margin = margin(0,0,0.1,0,"in"))

ggsave(filename = "./2024/20240214/v04-12.png", 
       device = grDevices::png,
       width = 6, height = 3.2, dpi = 180, units = "in")

colours()
ggplot() +
  geom_sf(data = sf_sam1, fill = "gray90") +
  # geom_sf(data = sf_grid1[log_disjoint,], fill = "black", alpha = 0.5) +
  # geom_sf(data = sf_grid1[log_overlaps,], fill = "green", alpha = 0.5) +
  # geom_sf(data = sf_grid1[log_touches,], fill = "blue", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_within,], fill = "tomato", alpha = 0.4) +#E7B7AE
  # geom_sf(data = sf_grid1[log_within,], fill = "skyblue", alpha = 0.4) +#C6DBE6
  # geom_sf(data = sf_grid1[log_within,], fill = "seagreen3", alpha = 0.4) +#ACD9A0
  # geom_sf(data = sf_grid1[log_within,], fill = "yellow", alpha = 0.4) +#EFEEA5
  geom_text(data = get_center(sf_grid1), aes(x,y-0.4), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(1,21), ylim = c(1,11), expand = F) +
  theme_bw() +
  theme(axis.title = element_blank())

v_red_color = "#E7B7AE"
v_blue_color = "#C6DBE6"
v_green_color = "#B5DAC1"
v_yellow_color = "#EFEEA5"

ggplot() +
  geom_sf(data = sf_sam1) +
  # geom_sf(data = sf_map2[log_disjoint,], fill = "black", alpha = 0.5) +
  geom_sf(data = sf_grid1[log_intersects,], fill = "blue", alpha = 0.5) +
  geom_text(data = get_center(sf_grid1), aes(x,y-0.4), label = v_water_mark, na.rm = T,
            colour = "gray0",size = 7, alpha = 0.3,
            family = v_font_heavy) +
  coord_sf(xlim = c(1,21), ylim = c(1,11), expand = F ) +
  theme_bw() +
  theme(axis.title = element_blank())

