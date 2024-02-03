library(scales)

my_plot = function(vdata, scale_name, ...){
  
  scale = getExportedValue("ggplot2", scale_name)
  
  ggplot(tibble(x = vdata)) +
    geom_blank(aes(x,1)) +
    scale(name = NULL,
          expand = expansion(c(0.1, 0.1)),
          ...) +
    scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
    theme_bw(base_family = v_font_bold) +
    theme(aspect.ratio = 1/4) 
}



# case1 - nothing
# ggplot(tibble(x = c(-1e6, 1e6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL,
#                      expand = expansion(c(0.1, 0.1))) +
#   scale_y_continuous(name = NULL, breaks = NULL) +
#   theme_bw() +
#   theme(aspect.ratio = 1/4)

my_plot(c(-1e6, 1e6), "scale_x_continuous")

ggsave(filename = "./2024/20240118/v06-01.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


# case1-1. label_number
# ggplot(tibble(x = c(-1e6, 1e6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_number()) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   annotate("text", x = -1e6, y = 1.6, hjust = 0, 
#            label = "label_number()", size = 5,
#            family = v_font_bold) +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4) 

my_plot(c(-1e6, 1e6),
        "scale_x_continuous", labels = label_number())

ggsave(filename = "./2024/20240118/v06-02.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


#case1-2. label_comma
# ggplot(tibble(x = c(-1e6, 1e6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_comma()) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   annotate("text", x = -1e6, y = 1.6, hjust = 0, 
#            label = "label_comma()", size = 5,
#            family = v_font_bold) +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4) 



my_plot(c(-1e6, 1e6), 
        "scale_x_continuous", labels = label_comma())

ggsave(filename = "./2024/20240118/v06-03.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


my_plot(c(-1e6, 1e6), 
        "scale_x_continuous", 
        labels = label_number(style_positive = "plus"))

ggsave(filename = "./2024/20240118/v06-04.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

my_plot(c(-1e6, 1e6), 
        "scale_x_continuous", 
        labels = label_number(style_negative = "parens"))

ggsave(filename = "./2024/20240118/v06-05.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

# case 2-1 nothing
# ggplot(tibble(x = c(0, 1e6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1))) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "no setting") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4) 


my_plot(c(0, 1e6), 
        "scale_x_continuous")

ggsave(filename = "./2024/20240118/v06-11.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


# case 2-3 1/1000
# ggplot(tibble(x = c(0, 1e6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_number(scale = 1/1000)) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number()") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20)) 


my_plot(c(0, 1e6), "scale_x_continuous",
        labels = label_number(scale = 1/1000))

ggsave(filename = "./2024/20240118/v06-12.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

# case 3-1 
# ggplot(tibble(x = c(0, 1e-6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1))) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "no setting") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20)) 


my_plot(c(0, 1e-6), "scale_x_continuous")

ggsave(filename = "./2024/20240118/v06-21.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

# case 3-3
# ggplot(tibble(x = c(0, 1e-6))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_number(scale = 1e6)) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number(scale = 1e6)") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20)) 

my_plot(c(0, 1e-6), "scale_x_continuous", labels = label_number(scale = 1e6))

ggsave(filename = "./2024/20240118/v06-22.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

#case4-1. 
# ggplot(tibble(x = c(1, 1e9))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL,
#                      expand = expansion(c(0.1, 0.1)),
#                      # breaks = log_breaks(10),
#                      labels = label_number(scale_cut = cut_short_scale())) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   # labs(title = "label_number(scale_cut = cut_short_scale())") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))

my_plot(c(0, 1e9), "scale_x_continuous")
ggsave(filename = "./2024/20240118/v06-31.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

my_plot(c(0, 1e9), "scale_x_continuous", 
        labels = label_number(scale_cut = cut_short_scale()))

ggsave(filename = "./2024/20240118/v06-32.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


#case 4-2
# ggplot(tibble(x = c(1, 1e9))) +
#   geom_blank(aes(x,1)) +
#   scale_x_log10(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      breaks = log_breaks(10),
#                      labels = label_number(scale_cut = cut_short_scale())) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "breaks = log_breaks(10)") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))

my_plot(c(1, 1e9), 
        "scale_x_log10", breaks = log_breaks(10),
        labels = label_number(scale_cut = cut_short_scale()))

ggsave(filename = "./2024/20240118/v06-33.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")



#case 4-3
# ggplot(tibble(x = c(1, 1e9))) +
#   geom_blank(aes(x,1)) +
#   scale_x_log10(name = NULL, 
#                 expand = expansion(c(0.1, 0.1)),
#                 breaks = log_breaks(n = 10),
#                 labels = label_number(scale_cut = cut_si('g'))) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number(scale_cut = cut_si('g'))") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))

my_plot(c(1, 1e9), 
        "scale_x_log10", breaks = log_breaks(10),
        labels = label_number(scale_cut = cut_si('g')))

ggsave(filename = "./2024/20240118/v06-34.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


#case 4-4
# ggplot(tibble(x = c(1, 1e9))) +
#   geom_blank(aes(x,1)) +
#   scale_x_log10(name = NULL, 
#                 expand = expansion(c(0.1, 0.1)),
#                 breaks = log_breaks(n = 10),
#                 labels = label_number(scale_cut = cut_si('m'))) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number(scale_cut = cut_si('m'))") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))


my_plot(c(1, 1e9), 
        "scale_x_log10", breaks = log_breaks(10),
        labels = label_number(scale_cut = cut_si('m')))

ggsave(filename = "./2024/20240118/v06-35.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

my_plot(c(0, 100), 
        "scale_x_continuous", 
        labels = label_number(suffix = "°C"))

ggsave(filename = "./2024/20240118/v06-41.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")






#case 4-5label_number(suffix = "°C")
# ggplot(tibble(x = c(-1e3, 1e3))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                 expand = expansion(c(0.1, 0.1)),
#                 labels = label_number()) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number()") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))


# my_plot(c(-1e3, 1e3), 
#         "scale_x_continuous", 
#         labels = label_number())
# 
# ggsave(filename = "./2024/20240118/v06-41.png", 
#        device = grDevices::png,
#        width = 6, height = 1.8, dpi = 180, units = "in")




#case 4-6
# ggplot(tibble(x = c(-1e3, 1e3))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_number(style_positive = "plus")) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number(style_positive = \"plus\")") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))


# my_plot(c(-1e3, 1e3), 
#         "scale_x_continuous", 
#         labels = label_number(style_positive = "plus"))
# 
# ggsave(filename = "./2024/20240118/v06-41.png", 
#        device = grDevices::png,
#        width = 6, height = 1.8, dpi = 180, units = "in")




#case 4-7
# ggplot(tibble(x = c(-1e3, 1e3))) +
#   geom_blank(aes(x,1)) +
#   scale_x_continuous(name = NULL, 
#                      expand = expansion(c(0.1, 0.1)),
#                      labels = label_number(style_negative = "parens")) +
#   scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
#   labs(title = "label_number(style_negative = \"parens\")") +
#   theme_bw(base_family = v_font_bold) +
#   theme(aspect.ratio = 1/4,
#         plot.title = element_text(hjust = 0.1, vjust = -20))



# my_plot(c(-1e3, 1e3), 
#         "scale_x_continuous", 
#         labels = label_number(style_negative = "parens"))
# 
# ggsave(filename = "./2024/20240118/v06-42.png", 
#        device = grDevices::png,
#        width = 6, height = 1.8, dpi = 180, units = "in")

