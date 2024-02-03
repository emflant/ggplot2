# https://scales.r-lib.org/reference/label_number.html

sig = sig_with_dokdo()


ggplot() +
  geom_sf(data = sig)

bb = st_bbox(sig)
bb
center = st_as_sfc(bb) |> 
  st_centroid()

circle = st_buffer(center, 200 * 1000)

st_crs(5179)

zero = st_point(c(0,0))
sfc_zero = st_sfc(zero, crs = st_crs(5179))
sfc_zero

bb = st_bbox(c(xmin = 122.71, xmax = 134.28, ymax = 28.6, ymin = 40.27), 
             crs = st_crs(4326)) |> 
  st_as_sfc() |> 
  st_transform(crs = st_crs(5179))

bb
ggplot() +
  geom_sf(data = sig) +
  geom_sf(data = bb, size = 1, fill = NA) +
  # geom_sf(data = circle, fill = NA, linewidth = 0.5) +
  coord_sf(crs = st_crs(4179)) +
  theme_bw()

st_crs(5179)$proj4string
st_crs(5179)$units
st_crs(5179)$id
bb = st_bbox(c(xmin = 122.71, xmax = 134.28, ymax = 28.6, ymin = 40.27), 
             crs = st_crs(4326)) |> 
  st_as_sfc()



cc = st_crs(5179)
a = st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
st_bbox(a) |> 
  class()


st_polygon()
st_as_sfc(c(28.6,122.71,40.27,134.28))

ggplot() +
  geom_sf(data = sig, aes(fill = SIG_CD)) +
  scale_fill_viridis_d(begin = 0.3) +
  # coord_sf(datum = st_crs(5179)) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "./2024/20240118/v04-00.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

center = st_bbox(sig) |> 
  st_as_sfc() |> 
  st_centroid()

ggplot() +
  geom_sf(data = sig, aes(fill = SIG_CD)) +
  geom_sf_text(data = center, label = v_water_mark,
               family = v_font_heavy, alpha = 0.5, size = 7) +
  scale_fill_viridis_d(begin = 0.3) +
  coord_sf(datum = st_crs(5179)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave(filename = "./2024/20240118/v04-01.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

sig
# scales::scientific_format()
ggplot() +
  geom_sf(data = sig, aes(fill = SIG_CD)) +
  geom_sf_text(data = center, label = v_water_mark,
               family = v_font_heavy, alpha = 0.5, size = 7) +
  scale_fill_viridis_d(begin = 0.3) +
  scale_x_continuous(labels = scales::label_comma(scale = 1/1000)) +
  scale_y_continuous(labels = scales::label_comma(scale = 1/1000)) +
  coord_sf(datum = st_crs(5179)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave(filename = "./2024/20240118/v04-02.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")

ggplot() +
  geom_sf(data = sig, aes(fill = SIG_CD)) +
  geom_sf_text(data = center, label = v_water_mark,
               family = v_font_heavy, alpha = 0.5, size = 7) +
  scale_fill_viridis_d(begin = 0.3) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si('m'))) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si('m'))) +
  coord_sf(datum = st_crs(5179)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())

ggsave(filename = "./2024/20240118/v04-03.png", 
       device = grDevices::png,
       width = 6, height = 5, dpi = 180, units = "in")
