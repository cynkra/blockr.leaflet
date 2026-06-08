#' Create a Leaflet Markers Map Block
#'
#' A block that drops one circle marker per row of the input data on an
#' interactive Leaflet map. Expects a data frame with `lat` and `lng` columns
#' and labels each marker with the `label` column (e.g. put each cat breed on
#' its country of origin). Overlapping points can be clustered.
#'
#' @param label Name of the column used to label markers. Defaults to `"name"`.
#' @param color Marker colour (hex string). Defaults to `"#2c7fb8"`.
#' @param radius Marker radius in pixels. Defaults to `7`.
#' @param cluster Cluster overlapping markers? Defaults to `TRUE`.
#' @param ... Additional arguments passed to [blockr.core::new_block()].
#'
#' @return A block object of class `leaflet_markers_block`.
#'
#' @importFrom blockr.core new_block
#' @importFrom shiny NS moduleServer numericInput sliderInput checkboxInput
#'   selectInput tagList reactiveVal observeEvent reactive updateSelectInput
#'
#' @export
new_leaflet_markers_block <- function(
  label = "name",
  color = "#2c7fb8",
  radius = 7,
  cluster = TRUE,
  ...
) {
  ui_fn <- function(id) {
    ns <- NS(id)
    tagList(
      selectInput(ns("label"), "Label column", choices = label, selected = label),
      colourpicker::colourInput(ns("color"), "Marker colour", value = color),
      sliderInput(
        ns("radius"),
        "Marker radius",
        min = 2,
        max = 20,
        value = radius,
        step = 1
      ),
      checkboxInput(ns("cluster"), "Cluster markers", value = cluster)
    )
  }

  server_fn <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      r_label <- reactiveVal(label)
      r_color <- reactiveVal(color)
      r_radius <- reactiveVal(radius)
      r_cluster <- reactiveVal(cluster)

      observeEvent(data(), {
        cols <- names(data())
        updateSelectInput(
          session,
          "label",
          choices = cols,
          selected = if (r_label() %in% cols) r_label() else cols[1]
        )
      })

      observeEvent(input$label, r_label(input$label))
      observeEvent(input$color, r_color(input$color))
      observeEvent(input$radius, r_radius(input$radius))
      observeEvent(input$cluster, r_cluster(input$cluster))

      list(
        expr = reactive({
          col <- r_color()
          rad <- r_radius()
          lab <- as.name(r_label())
          clus <- if (isTRUE(r_cluster())) {
            quote(leaflet::markerClusterOptions())
          } else {
            NULL
          }
          bquote(
            leaflet::leaflet(data) |>
              leaflet::addTiles() |>
              leaflet::addCircleMarkers(
                lat = ~lat,
                lng = ~lng,
                label = ~as.character(.(lab)),
                radius = .(rad),
                color = .(col),
                stroke = FALSE,
                fillOpacity = 0.8,
                clusterOptions = .(clus)
              )
          )
        }),
        state = list(
          label = r_label,
          color = r_color,
          radius = r_radius,
          cluster = r_cluster
        )
      )
    })
  }

  new_block(
    server = server_fn,
    ui = ui_fn,
    class = "leaflet_markers_block",
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
block_output.leaflet_markers_block <- function(x, result, session) {
  renderLeaflet(result)
}

#' @importFrom blockr.core block_ui
#' @importFrom leaflet leafletOutput
#' @exportS3Method
block_ui.leaflet_markers_block <- function(id, x, ...) {
  tagList(leafletOutput(NS(id, "result"), height = "400px"))
}
