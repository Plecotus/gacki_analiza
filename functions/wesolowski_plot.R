# Wykres wesołowskiego
theme_wesolowski <- function(){

theme_bw() %+replace%
    theme(panel.background = element_rect(colour = "white"),
                            panel.grid.major = element_line(colour = "white"),
                            axis.text = element_text(size = 18),
                            axis.title.x = element_text(size = 22, vjust = -.5),
                            axis.title.y = element_text(size = 22, angle = 90, vjust = 1.5 ),
                            strip.text = element_text(size = 18))
}
