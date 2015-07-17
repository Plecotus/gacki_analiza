# Wykres wesołowskiego
theme_wesolowski <- function(){

theme_bw() %+replace%
    theme(panel.background = element_rect(colour = "white"),
                            panel.grid.major = element_line(colour = "white"),
                            axis.text = element_text(size = 14),
                            axis.title.x = element_text(size = 18),
                            axis.title.y = element_text(size = 18, angle = 90),
                            strip.text = element_text(size = 14))
}
