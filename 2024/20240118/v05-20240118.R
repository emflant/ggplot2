library(scales)

demo_continuous(c(-1e6, 1e6))
#> scale_x_continuous()

#>
#>
#>
#>

gg = function(data = vdata, title = "", ...){
  ggplot(tibble(x = vdata)) +
    geom_blank(aes(x,1)) +
    scale_x_continuous(name = NULL,
                       expand = expansion(c(0.1, 0.1)),
                       ...) +
    scale_y_continuous(name = NULL, breaks = NULL) +
    annotate("text", x = vdata[1], y = 1.6, hjust = 0, 
             label = vtitle, size = 5,
             family = v_font_bold) +
    theme_bw() +
    theme(aspect.ratio = 1/4) 
}

# case1 - nothing
ggplot(tibble(x = c(-1e6, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL,
                     expand = expansion(c(0.1, 0.1))) +
  scale_y_continuous(name = NULL, breaks = NULL) +
  theme_bw() +
  theme(aspect.ratio = 1/4) 

ggsave(filename = "./2024/20240118/v05-01.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")


# case1-1. label_number
ggplot(tibble(x = c(-1e6, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number()) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  annotate("text", x = -1e6, y = 1.6, hjust = 0, 
           label = "label_number()", size = 5,
           family = v_font_bold) +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4) 

ggsave(filename = "./2024/20240118/v05-02.png", 
       device = grDevices::png,
       width = 6, height = 1.8, dpi = 180, units = "in")

#case1-2. label_comma
ggplot(tibble(x = c(-1e6, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_comma()) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  annotate("text", x = -1e6, y = 1.6, hjust = 0, 
           label = "label_comma()", size = 5,
           family = v_font_bold) +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4) 


# case 2-1 nothing
ggplot(tibble(x = c(0, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1))) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "no setting") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4) 

# case 2-2 label_number
ggplot(tibble(x = c(0, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number()) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number()") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20)) 


# case 2-3 1/1000
ggplot(tibble(x = c(0, 1e6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number(scale = 1/1000)) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number()") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20)) 



# case 3-1 
ggplot(tibble(x = c(0, 1e-6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1))) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "no setting") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20)) 

# case 3-2
ggplot(tibble(x = c(0, 1e-6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number()) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number()") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20)) 

# case 3-3
ggplot(tibble(x = c(0, 1e-6))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number(scale = 1e6)) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(scale = 1e6)") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20)) 


#case4-1. 
ggplot(tibble(x = c(1, 1e9))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     # breaks = log_breaks(10),
                     labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(scale_cut = cut_short_scale())") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))

#case 4-2
ggplot(tibble(x = c(1, 1e9))) +
  geom_blank(aes(x,1)) +
  scale_x_log10(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     breaks = log_breaks(10),
                     labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "breaks = log_breaks(10)") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))




#case 4-3
ggplot(tibble(x = c(1, 1e9))) +
  geom_blank(aes(x,1)) +
  scale_x_log10(name = NULL, 
                expand = expansion(c(0.1, 0.1)),
                breaks = log_breaks(n = 10),
                labels = label_number(scale_cut = cut_si('g'))) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(scale_cut = cut_si('g'))") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))

ggsave(filename = "./2024/20240118/v05-01.png", 
       device = grDevices::png,
       width = 6, height = 1.6, dpi = 180, units = "in")

#case 4-4
ggplot(tibble(x = c(1, 1e9))) +
  geom_blank(aes(x,1)) +
  scale_x_log10(name = NULL, 
                expand = expansion(c(0.1, 0.1)),
                breaks = log_breaks(n = 10),
                labels = label_number(scale_cut = cut_si('m'))) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(scale_cut = cut_si('m'))") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))



#case 4-5
ggplot(tibble(x = c(-1e3, 1e3))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                expand = expansion(c(0.1, 0.1)),
                labels = label_number()) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number()") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))



#case 4-6
ggplot(tibble(x = c(-1e3, 1e3))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number(style_positive = "plus")) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(style_positive = \"plus\")") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))



#case 4-7
ggplot(tibble(x = c(-1e3, 1e3))) +
  geom_blank(aes(x,1)) +
  scale_x_continuous(name = NULL, 
                     expand = expansion(c(0.1, 0.1)),
                     labels = label_number(style_negative = "parens")) +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(0,2)) +
  labs(title = "label_number(style_negative = \"parens\")") +
  theme_bw(base_family = v_font_bold) +
  theme(aspect.ratio = 1/4,
        plot.title = element_text(hjust = 0.1, vjust = -20))

