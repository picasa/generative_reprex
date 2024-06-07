
# packages ####
library(dplyr)   # table operations with SQL-like grammar
library(purrr)   # Functional Programming tools
library(ggplot2) # graphical operations with a layer by layer grammar
library(ggforce) # additional graphical operations in the ggplot frame
library(cowplot) # arrange graphical elements into complex compound figures

# functions ####
#' Sample n points in an elliptic area
#' @description Point are sampled from uniform distribution in polar coordinates. The point set is then transformed to cartesian coordinates, scaled, and rotated.
#' @param n number of points
#' @param x0,y0 coordinates of the center of the sampling area
#' @param r radius of the sampling area
#' @param a rotation angle of the point set (radians)
#' @param scale_x scaling coefficient applied on the x axis
#' @return a dataframe with point rank, and xy coordinates columns

sample_ellipse <- function(
    n = 7, x0 = 0, y0 = 0, r = 1, a = -pi/6, scale_x = 0.5
  ) {
  r = sqrt(stats::runif(n, 0, r))
  theta = stats::runif(n, 0, 2*pi)

  # scale and rotate layout
  layout <- dplyr::tibble(
    n = 1:n,
    x0 = x0 + r * cos(theta),
    y0 = y0 + r * sin(theta)) |> 
    dplyr::mutate(x0 = x0 * scale_x) |> 
    dplyr::mutate(
      x = x0*cos(a) - y0*sin(a),
      y = x0*sin(a) + y0*cos(a)) |>
    dplyr::select(n,x,y)

  return(layout)
}

#' Render a spline curve from control points
#' @param data a dataframe with x and y columns defining control points.
#' @param type control if the spline starts and ends at the terminal control points.
#' @param n number of points generated for the spline
#' @param width,alpha arguments passed to `geom_bspline()`
#' @param coord coordinate system passed to `ggplot()`
#' @return a ggplot object
#'
render_spline <- function(
    data, n = 50, width = 0.5, alpha = 1, coord = NULL) {
  
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggforce::geom_bspline(
      lineend = "round", n = n, linewidth = width, alpha = alpha)
  
  return(plot + coord + ggplot2::theme_void())
  
}

# output ####

# generate individual glyphs shapes by sampling splines control points 
# adapted from https://inconvergent.net/2017/spline-script/

# parameters
set.seed(1)
n_sample = 100 # number of sampled glyphs
n_control = 6  # number of sampled control points

# create a set of 100 glyphs by iteratively sampling layouts and fitting a spline.
data_glyphs <- tibble(pattern = 1:100) |>  
  mutate(
    layout = map(pattern, ~ sample_ellipse(n = n_control)),
    plot = map(layout, ~ render_spline(..1))
  )

# layout the individuals plots on a 10x10 grid, with margin.
plot_glyphs <- plot_grid(plotlist = data_glyphs$plot, ncol = 10, scale = 0.8) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 

# export plot in the svg format
ggsave(
  plot_glyphs,
  file = "set_glyphs.svg",
  width = 148, height = 210, scale = 1, units = "mm")



