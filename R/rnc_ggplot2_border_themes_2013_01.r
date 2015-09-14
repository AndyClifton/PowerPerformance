# Rudolf Cardinal, March 2011
# Simple extensions to ggplot2 (v0.8.7); see http://pobox.com/~rudolf/statistics/R
# Modified 5 Jan 2013 for ggplot2 0.9.3 (NB: use sessionInfo() to find current package versions)
# - fetch ggplot2 source with: git clone https://github.com/hadley/ggplot2.git
# Changes, because ggplot2 has changed its internal calling mechanisms:
# - opts() deprecated in favour of theme()
# - "Element panel.border must be an element_rect object" (error from validate_element() in theme-elements.r)
#   ... so change all class = "theme" to class = c("element_rect", "element")
# - "cannot coerce type 'closure' to vector of type 'list'"
#   ... a closure is a function (see ?typeof)
#   ... change class to be of class c("MYCLASS", "element_rect", "element")
# - then element_grob.MYCLASS not called by element_render()/element_grob()/UseMethod()... environment/namespace problem
#   tried setMethod("element_grob", "theme_border", function(STUFF) { STUFF} , where = as.environment("package:ggplot2")
#   but the environment is locked
#   ggplot2's theme-elements.r defines e.g. element_rect (exported) and element_grob.element_rect (not exported, does the work)
#   However, we can't override an internal function:
#       ... e.g. rewrite "validate_element" to crash
#           set environment(validate_element) <- as.environment("package:ggplot2") -- doesn't break the plotting.
# - Upshot: now impossible to hack through this way (locked environment).
# - http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
# - http://stackoverflow.com/questions/8204008/redirect-intercept-function-calls-within-a-package-function
# - These don't fix it:
#   library(proto)
#   theme <- with(proto(environment(ggplot2::theme), theme = ggplot2::theme, element_grob.theme_border = my.element_grob.theme_border), theme) --- doesn't work
#   ggplot <- with(proto(environment(ggplot2::ggplot), ggplot = ggplot2::ggplot, element_grob.theme_border = my.element_grob.theme_border), ggplot) --- breaks!
# - Fix by Baptiste Auguie 8/1/2013: inherit from element_blank instead; then it works fine.

#-------------------------------------------------------------------------------
# Requirements
#-------------------------------------------------------------------------------

library(grid) # for gpar

#-------------------------------------------------------------------------------
# Code duplicated from ggplot2 source (not exposed to wider namespace) for convenience
#-------------------------------------------------------------------------------

.pt <- 1 / 0.352777778
len0_null <- function(x) {
    if (length(x) == 0)  NULL
    else                 x
}

#-------------------------------------------------------------------------------
# Generic panel border (can set any combination of left/right/top/bottom)
#-------------------------------------------------------------------------------

theme_border <- function(
        type = c("left", "right", "bottom", "top", "none"),
        colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=c("bottom","left")) ) + ...
    type <- match.arg(type, several.ok=TRUE)
    structure(
        list(type = type, colour = colour, size = size, linetype = linetype),
        class = c("theme_border", "element_blank", "element")
    )
}
element_grob.theme_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        type = NULL,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    if (is.null(type)) type = element$type
    xlist <- c()
    ylist <- c()
    idlist <- c()
    if ("bottom" %in% type) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
    }
    if ("top" %in% type) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
    }
    if ("left" %in% type) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
    }
    if ("right" %in% type) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
    }
    if (length(type)==0 || "none" %in% type) { # blank; cannot pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
    }
    gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype)
    element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour, lty = element$linetype)
    polylineGrob(
        x = xlist, y = ylist, id = idlist, ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}

#-------------------------------------------------------------------------------
# For convenience: "L" (left + bottom) border
#-------------------------------------------------------------------------------

theme_L_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border=theme_L_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_L_border", "element_blank", "element")
    )
}
element_grob.theme_L_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype)
    element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour, lty = element$linetype)
    polylineGrob(
        x = c(x+width, x, x), y = c(y,y,y+height), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}

#-------------------------------------------------------------------------------
# For convenience: bottom border only
#-------------------------------------------------------------------------------

theme_bottom_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border=theme_bottom_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_bottom_border", "element_blank", "element")
    )
}
element_grob.theme_bottom_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype)
    element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour, lty = element$linetype)
    polylineGrob(
        x = c(x, x+width), y = c(y,y), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}

#-------------------------------------------------------------------------------
# For convenience: left border only
#-------------------------------------------------------------------------------

theme_left_border <- function(colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border=theme_left_border() ) + ...
    structure(
        list(colour = colour, size = size, linetype = linetype),
        class = c("theme_left_border", "element_blank", "element")
    )
}
element_grob.theme_left_border <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype)
    element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour, lty = element$linetype)
    polylineGrob(
        x = c(x, x), y = c(y, y+height), ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}



#-------------------------------------------------------------------------------
# Border selection by number
#-------------------------------------------------------------------------------

theme_border_numerictype <- function(type, colour = "black", size = 1, linetype = 1) {
    # use with e.g.: ggplot(...) + theme( panel.border=theme_border(type=9) ) + ...
    structure(
        list(type = type, colour = colour, size = size, linetype = linetype),
        class = c("theme_border_numerictype", "element_blank", "element")
    )
}
element_grob.theme_border_numerictype <- function(
        element, x = 0, y = 0, width = 1, height = 1,
        type = NULL,
        colour = NULL, size = NULL, linetype = NULL,
        ...) {
    if (is.null(type)) type = element$type
    # numerical types from: library(gridExtra); example(borderGrob)
    # 1=none, 2=bottom, 3=right, 4=top, 5=left, 6=B+R, 7=T+R, 8=T+L, 9=B+L, 10=T+B, 11=L+R, 12=T+B+R, 13=T+L+R, 14=T+B+L, 15=B+L+R, 16=T+B+L+R
    xlist <- c()
    ylist <- c()
    idlist <- c()
    if (type==2 || type==6 || type==9 || type==10 || type==12 || type==14 || type==15 || type==16) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
    }
    if (type==4 || type==7 || type==8 || type==10 || type==12 || type==13 || type==14 || type==16) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
    }
    if (type==5 || type==8 || type==9 || type==11 || type==13 || type==14 || type==15 || type==16) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
    }
    if (type==3 || type==6 || type==7 || type==11 || type==12 || type==13 || type==15 || type==16) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
    }
    if (type==1) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
    }
    gp <- gpar(lwd = len0_null(size * .pt), col = colour, lty = linetype)
    element_gp <- gpar(lwd = len0_null(element$size * .pt), col = element$colour, lty = element$linetype)
    polylineGrob(
        x = xlist, y = ylist, id = idlist, ..., default.units = "npc",
        gp = modifyList(element_gp, gp),
    )
}

#-------------------------------------------------------------------------------
# Examples
#-------------------------------------------------------------------------------

rnc_ggplot2_border_themes_example_script = '
    library(ggplot2)
    df = data.frame( x=c(1,2,3), y=c(4,5,6) )
    source("http://egret.psychol.cam.ac.uk/statistics/R/extensions/rnc_ggplot2_border_themes_2013_01.r")
    ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + theme( panel.border = theme_border( c("bottom","left") ) )
    ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + theme( panel.border = theme_left_border() )
    ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + theme( panel.border = theme_bottom_border() )
    ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + theme( panel.border = theme_L_border() )
    ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + theme( panel.border = theme_border_numerictype(12) ) # use 1:16 as possible values
'
