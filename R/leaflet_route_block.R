#' Create a Leaflet Route Map Block
#'
#' A block that renders GPS route data on an interactive Leaflet map.
#' Expects a data frame with `lat` and `lng` columns.
#'
#' @param color Line colour (hex string). Defaults to `"#000000"`.
#' @param weight Line weight in pixels. Defaults to `3`.
#' @param opacity Line opacity between 0 and 1. Defaults to `0.8`.
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

      observeEvent(input$color, r_color(input$color))
      observeEvent(input$weight, r_weight(input$weight))
      observeEvent(input$opacity, r_opacity(input$opacity))

      list(
        expr = reactive({
          col <- r_color()
          wt <- r_weight()
          op <- r_opacity()
          bquote(
            leaflet::leaflet() |>
              leaflet::addTiles() |>
              leaflet::addPolylines(
                data = data[, c("lat", "lng")],
                lat = ~lat,
                lng = ~lng,
                color = .(col),
                weight = .(wt),
                opacity = .(op)
              )
          )
        }),
        state = list(
          color = r_color,
          weight = r_weight,
          opacity = r_opacity
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
