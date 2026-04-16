#' @importFrom blockr.core register_blocks
register_leaflet_blocks <- function() {
  register_blocks(
    "new_leaflet_route_block",
    name = "Leaflet Route Map",
    description = "Plot GPS route on an interactive Leaflet map",
    category = "plot",
    icon = "geo-alt",
    package = utils::packageName(),
    overwrite = TRUE
  )
}
