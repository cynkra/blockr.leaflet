#' @importFrom blockr.core register_blocks
register_leaflet_blocks <- function() {
  register_blocks(
    c("new_leaflet_route_block", "new_leaflet_markers_block"),
    name = c("Leaflet Route Map", "Leaflet Markers Map"),
    description = c(
      "Plot GPS route on an interactive Leaflet map",
      "Drop one marker per row on an interactive Leaflet map"
    ),
    category = c("plot", "plot"),
    icon = c("geo-alt", "pin-map"),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
