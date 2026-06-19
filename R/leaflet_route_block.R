#' Create a Leaflet Route Map Block
#'
#' A block that renders GPS route data on an interactive Leaflet map.
#' Expects a data frame with `lat` and `lng` columns.
#'
#' @param color Line colour (hex string). Defaults to `"#000000"`.
#' @param weight Line weight in pixels. Defaults to `3`.
#' @param opacity Line opacity between 0 and 1. Defaults to `0.8`.
#' @param pois Optional gazetteer of points of interest with `name`,
#'   `lat`, `lng`, and (optionally) `type` columns. Each is snapped to
#'   the nearest point of the route and shown as a coloured marker;
#'   those further than `snap_tol_m` are dropped (see [snap_pois()]).
#' @param snap_tol_m Maximum distance (m) from the route to keep a POI.
#'   Defaults to `1200`.
#' @param ... Additional arguments passed to [blockr.core::new_block()].
#'
#' @return A block object of class `leaflet_route_block`.
#'
#' @importFrom blockr.core new_block
#' @importFrom shiny NS moduleServer numericInput sliderInput tagList
#'   reactiveVal observeEvent reactive
#'
#' @export
new_leaflet_route_block <- function(
  color = "#000000",
  weight = 3,
  opacity = 0.8,
  pois = NULL,
  snap_tol_m = 1200,
  ...
) {
  ui_fn <- function(id) {
    ns <- NS(id)
    tagList(
      colourpicker::colourInput(ns("color"), "Line colour", value = color),
      numericInput(
        ns("weight"),
        "Line weight",
        value = weight,
        min = 1,
        max = 10
      ),
      sliderInput(
        ns("opacity"),
        "Opacity",
        min = 0,
        max = 1,
        value = opacity,
        step = 0.1
      )
    )
  }

  server_fn <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_color <- reactiveVal(color)
      r_weight <- reactiveVal(weight)
      r_opacity <- reactiveVal(opacity)
      # Non-UI parameters, kept in state so the block round-trips.
      r_pois <- reactiveVal(pois)
      r_tol <- reactiveVal(snap_tol_m)

      observeEvent(input$color, r_color(input$color))
      observeEvent(input$weight, r_weight(input$weight))
      observeEvent(input$opacity, r_opacity(input$opacity))

      list(
        expr = reactive({
          col <- r_color()
          wt <- r_weight()
          op <- r_opacity()
          pois <- r_pois()
          snap_tol_m <- r_tol()
          bquote({
            .map <- leaflet::leaflet() |>
              leaflet::addTiles() |>
              leaflet::addPolylines(
                data = data[, c("lat", "lng")],
                lat = ~lat,
                lng = ~lng,
                color = .(col),
                weight = .(wt),
                opacity = .(op)
              )
            .pts <- blockr.leaflet::snap_pois(.(pois), data, tol_m = .(tol))
            if (!is.null(.pts)) {
              .map <- leaflet::addCircleMarkers(
                .map,
                data = .pts,
                lat = ~lat,
                lng = ~lng,
                radius = 6,
                stroke = TRUE,
                color = "#ffffff",
                weight = 2,
                fillColor = ~color,
                fillOpacity = 1,
                popup = ~label,
                label = ~label
              )
            }
            .map
          }, list(col = col, wt = wt, op = op, pois = pois, tol = snap_tol_m))
        }),
        state = list(
          color = r_color,
          weight = r_weight,
          opacity = r_opacity,
          pois = r_pois,
          snap_tol_m = r_tol
        )
      )
    })
  }

  new_block(
    server = server_fn,
    ui = ui_fn,
    class = "leaflet_route_block",
    dat_valid = function(data) {
      stopifnot(
        is.data.frame(data),
        "lat" %in% names(data),
        "lng" %in% names(data)
      )
    },
    ...
  )
}

#' @importFrom blockr.core block_output
#' @importFrom leaflet renderLeaflet
#' @exportS3Method
block_output.leaflet_route_block <- function(x, result, session) {
  renderLeaflet(result)
}

#' @importFrom blockr.core block_ui
#' @importFrom leaflet leafletOutput
#' @exportS3Method
block_ui.leaflet_route_block <- function(id, x, ...) {
  tagList(leafletOutput(NS(id, "result"), height = "400px"))
}
