
#'GeomTimeline
#'
#'this geom is used by the function geom_Timeline and shuld not be used directly
#'
#'@import ggplot2
#'@import grid
GeomTimeline<- ggproto("GeomTimeline", Geom,
                         required_aes =c("x"),
                         default_aes = aes(colour="black",fill="black",size=2,y=0,alpha=0.75,stroke=1,shape=19),

                       draw_key = function(data, params, size) {
                         pointsGrob(0.5, 0.5,
                                    pch = data$shape,
                                    gp = gpar(
                                      col = alpha(data$colour, data$alpha),
                                      fill = alpha(data$fill, data$alpha),
                                      fontsize = data$size  *data$size *data$size /10,
                                      lwd = data$stroke * .stroke / 2
                                    )
                         )
                       },

                         draw_panel = function(data, panel_scales, coord) {

                           coords <- coord$transform(data, panel_scales)

                           points <- grid::pointsGrob(

                             x = coords$x,
                             y = coords$y,
                             pch = coords$shape,
                             gp = grid::gpar(
                                col = ggplot2::alpha(coords$colour, coords$alpha),
                                fill = ggplot2::alpha(coords$fill, coords$alpha),
                                fontsize = coords$size  *coords$size *coords$size /10
                             )
                           )

                           grobTree(points)
                         }
)

#' geom_timeline
#'
#' this function create the geom_timeline for a given NOAA cleaned data.frame
#'
#' @param data a data.frame as returned by storm_observation
#' @param mapping date
#'
#' color and size, as inerited by Point
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return a plot
#' @export
#'
#' @examples
#'

geom_timeline <- function(
  mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


