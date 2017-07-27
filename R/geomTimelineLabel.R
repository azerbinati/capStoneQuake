

draw_timeline_label <- function (data, panel_params, coord) {

  coords <- coord$transform(data, panel_params)

  points <- unique(coords$x)

  ys <- unique(coords$y)

  line_coords = expand.grid(x = points, y = c(ys, ys + 0.1))
  line_coords$id = rep(1:(nrow(line_coords) / 2), 2)

  text <- textGrob(
    label = coords$label,
    x = coords$x,
    y = coords$y + 0.15,
    rot = coords$angle,
    just = c("left", "center"),
    gp = gpar(
      col = coords$colour,
      fontsize = 3.25 * .pt
    ),
      )
  lines <- polylineGrob(
    x = line_coords$x,
    y = line_coords$y,
    id = line_coords$id,
    gp = gpar(
      col = alpha(coords$colour, 0.25),
      lwd = 0.5 * .pt
    )
  )

  grobTree(lines, text)
}


GeomTimelineLabel <- ggproto(
  "GeomTimelineLabel",
  ggplot2::Geom,
  required_aes = c("x", "label", "by"),
  default_aes = aes(colour="gray", y = 0, angle = 30),
  draw_group = draw_timeline_label
)


StatTimelineLabel <- ggproto(
  "StatTimelineLabel",
  ggplot2::Stat,
  setup_data = function(data, params) {

    data %>%dplyr::mutate(date=lubridate::as_date(x) ) %>%
      dplyr::filter(lubridate::year(date) >= params$xmin & lubridate::year(date) <= params$xmax)

  },

  compute_group = function (data, scales, params, n_max = 5,xmin,xmax) {
  stopifnot(n_max>0)

    data %>%arrange(desc(by)) %>% slice(1:n_max)
  }
)

geom_timeline_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ..., show.legend = NA, inherit.aes = TRUE,
  n_max = 5,  na.rm = FALSE, xmin=2000, xmax=3000
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTimelineLabel,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n_max = n_max,
      na.rm = na.rm,
      xmin=xmin,
      xmax=xmax,

      ...
    )
  )
}
