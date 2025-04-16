
#Theme for plots
theme_ju <- function(base_size = 12, base_family = "sans"){
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     ggplot2::theme(line = ggplot2::element_line(colour = "black"),
                    rect = ggplot2::element_rect(fill = 'gray90',
                                                 linetype = 0, colour = NA),
                    text = ggplot2::element_text(colour = 'gray20'),
                    axis.title = ggplot2::element_text(face = "bold"),
                    axis.text = ggplot2::element_text(),
                    axis.ticks = ggplot2::element_blank(),
                    axis.line = ggplot2::element_blank(),
                    legend.background = ggplot2::element_rect(),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    legend.title = ggplot2::element_text(face = "bold"),
                    panel.grid = ggplot2::element_line(colour = NULL),
                    panel.grid.major = ggplot2::element_line(colour = 'gray40'),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(hjust = 0, size = ggplot2::rel(1.5), face = "bold"),
                    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines"),
                    strip.background = ggplot2::element_rect(fill = 'gray20'),
                    strip.text = ggplot2::element_text(face = "bold", color = "white", size = 16)))
}
  
  
  #With a plot, add a horizontal line break and some captions
  caption_plot <- function(plot,
                           left_caption = "Source:",
                           right_caption = "Hartford Funds Data Science",
                           fontsize = 12){
    
    captioned_plot <- grid::grobTree(
      #make the background of the entire plot area match the color of theme_hf
      grid::rectGrob(gp = grid::gpar(fill = 'gray90', lwd = NA)),
      
      #Combine the plot, a line break and a caption
      gridExtra::arrangeGrob(
        plot,
        #line break
        grid::rectGrob(height = 0,
                       gp = grid::gpar(fill = 'gray90',
                                       lwd = 3,
                                       col = 'gray40')),
        
        #left caption; typically used to describe the data source
        grid::textGrob(left_caption, just = "left", x = 0.05, y = .5,
                       gp = grid::gpar(fontface = "bold",
                                       fontsize = fontsize,
                                       col = 'gray20',
                                       fontfamily = "sans")),
        
        #right caption, typically used to name the company or department
        grid::textGrob(right_caption, just = "right", x = 0.95, y = .5,
                       gp = grid::gpar(fontface = "bold",
                                       fontsize = fontsize,
                                       col = 'gray20',
                                       fontfamily = "sans")),
        
        #layout the plot, line break and captions, with the plot using 95% of the area
        layout_matrix = rbind(c(1, 1),
                              c(2, 2),
                              c(3, 4)),
        heights = c(.95, .01, .04)
      )
    )
    
    #draw the plot as part of the function, but return the grob
    grid::grid.draw(captioned_plot)
    captioned_plot
    
  }
