#' Generate a clean plot that is more-or-less publication ready
#' 
#' \code{cleanPlot} tidies up standard ggplot2 graphics.
#' 
#' @param p the ggplot plot to be processed
#' @param base.size the default text size in points
#' @return p the cleaned plot
#' @export
#'   
#' @seealso \code{\link{ggplot2}}

cleanPlot <- function(p,
                      base.size = 10,
                      code.dir = dirname(sys.frame(1)$ofile),
                      axis.colour = "grey50",
                      grid.colour = "grey90",
                      axis.text.colour = "grey33",
                      plot.background = "white",
                      panel.background = "white"){  
  
  # see if we can get the border themes
  load_ggplot2_border_themes(code.dir)
  
  # lineweight
  line.weight = min(4,max((base.size / 8) * 0.5,0.5))
  
  p <- p + theme_bw(base.size) + 
    theme(axis.title = element_text(face="plain",
                                    colour = axis.text.colour,
                                    size = base.size),          
          axis.text = element_text(size = round(base.size*0.8),
                                   colour = axis.text.colour),
          axis.line.x = element_line(size = line.weight,
                                     colour = axis.colour),          
          axis.line.y = element_line(size = line.weight,
                                     colour = axis.colour),
          axis.ticks = element_line(size = line.weight,
                                    colour = axis.colour),
          axis.ticks.length = unit(3,"points"),
          axis.ticks.margin = unit(2,"points"),
          panel.border = theme_border(c("left","bottom"), 
                                      colour = axis.colour,
                                      size = line.weight*2), # RNC hack, see above          
          legend.text = element_text(size = round(base.size*0.8),
                                     colour = axis.text.colour),
          legend.title = element_text(size = round(base.size*0.8),
                                     colour = axis.text.colour),
          legend.background = element_rect(fill = panel.background),
          legend.key.size = unit(1, "lines"),
          legend.key = element_blank(), # switch off the rectangle around symbols in the legend   
          legend.margin = unit(0, "line"),
          panel.background = element_rect(fill = panel.background),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(colour = grid.colour,
                                          size = line.weight/2),                    
          plot.background = element_rect(fill = plot.background,
                                         colour = plot.background),          
          plot.margin = unit(c(1,0.5,0,0.5), "lines"),
          strip.background = element_blank(), 
          strip.text = element_text(size = base.size,
                                    colour = axis.text.colour))
  return(p)
}

load_ggplot2_border_themes <- function(my.dir){
   
  result = tryCatch({
    # see if we can update them from online
    source("http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r")
    #cat("Loaded themes from web repository")
    return(1)
  }, warning = function(w) {
    # warning-handler-code
  }, error = function(e) {
    # use the local version
    source(file.path(my.dir,"rnc_ggplot2_border_themes_2013_01.r"))
    #cat("Loaded themes from local copy\n")
    return(-1)
  }, finally = {
    # cleanup-code
  })
  
}