#' Create a Leaflet Markers Map Block
#'
#' A block that drops one circle marker per row of the input data on an
#' interactive Leaflet map. Expects a data frame with `lat` and `lng` columns
#' and labels each marker with the `label` column (e.g. put each cat breed on
#' its country of origin). Overlapping points can be clustered, or coloured by a
#' grouping column (`color_by`) to make one group stand out among the others.
#'
#' @param label Name of the column used to label markers. Defaults to `"name"`.
#' @param color Marker colour (hex string), used when `color_by` is unset.
#'   Defaults to `"#2c7fb8"`.
#' @param radius Marker radius in pixels. Defaults to `7`.
#' @param cluster Cluster overlapping markers? Ignored when `color_by` is set.
#'   Defaults to `TRUE`.
#' @param color_by Optional column whose values colour the markers, with a
#'   legend. A categorical column uses a discrete palette (and the smallest
#'   groups are drawn on top, so a highlighted breed is not hidden under the
#'   others); a numeric column uses a continuous `viridis` scale, so a value
#'   (life span, weight, ...) can be read off the map by position. Defaults to
#'   none.
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
  color_by = character(),
  ...
) {
  color_by <- if (length(color_by)) color_by else "(none)"

  ui_fn <- function(id) {
    ns <- NS(id)
    tagList(
      selectInput(ns("label"), "Label column", choices = label, selected = label),
      selectInput(ns("color_by"), "Colour by", choices = c("(none)", color_by),
                  selected = color_by),
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
      r_color_by <- reactiveVal(color_by)

      observeEvent(data(), {
        cols <- names(data())
        updateSelectInput(
          session,
          "label",
          choices = cols,
          selected = if (r_label() %in% cols) r_label() else cols[1]
        )
        updateSelectInput(
          session,
          "color_by",
          choices = c("(none)", cols),
          selected = if (r_color_by() %in% cols) r_color_by() else "(none)"
        )
      })

      observeEvent(input$label, r_label(input$label))
      observeEvent(input$color, r_color(input$color))
      observeEvent(input$radius, r_radius(input$radius))
      observeEvent(input$cluster, r_cluster(input$cluster))
      observeEvent(input$color_by, r_color_by(input$color_by))

      list(
        expr = reactive({
          col <- r_color()
          rad <- r_radius()
          lab <- as.name(r_label())
          cby <- r_color_by()

          use_col <- isTruthy_col(cby) && cby %in% names(data())
          # A binary 0/1 flag is stored as numeric but is really categorical, so
          # only treat a numeric column with more than two distinct values as
          # continuous; everything else gets the discrete palette.
          is_num <- use_col && is.numeric(data()[[cby]]) &&
            length(unique(stats::na.omit(data()[[cby]]))) > 2

          if (is_num) {
            # numeric column: continuous viridis scale, gradient legend
            bquote(
              local({
                .d <- data
                .v <- .d[[.(cby)]]
                .pal <- leaflet::colorNumeric("viridis", .v,
                                              na.color = "#cccccc")
                leaflet::leaflet(.d) |>
                  leaflet::addTiles() |>
                  leaflet::addCircleMarkers(
                    lat = ~lat,
                    lng = ~lng,
                    label = ~paste0(as.character(.(lab)), ": ", .v),
                    radius = .(rad),
                    color = .pal(.v),
                    stroke = FALSE,
                    fillOpacity = 0.85
                  ) |>
                  leaflet::addLegend(pal = .pal, values = .v, title = .(cby))
              })
            )
          } else if (use_col) {
            bquote(
              local({
                .d <- data
                .raw <- .d[[.(cby)]]
                # a 0/1 numeric flag reads better as TRUE/FALSE in the legend
                .g <- if (is.numeric(.raw) && all(.raw %in% c(0, 1, NA))) {
                  factor(.raw == 1, levels = c(FALSE, TRUE))
                } else {
                  as.factor(.raw)
                }
                # draw the smallest groups last, so a highlighted breed sits
                # on top of the crowd sharing its country
                .ord <- order(stats::ave(seq_len(nrow(.d)), .g, FUN = length),
                              decreasing = TRUE)
                .d <- .d[.ord, ]
                .g <- .g[.ord]
                .pal <- leaflet::colorFactor("Set1", .g)
                leaflet::leaflet(.d) |>
                  leaflet::addTiles() |>
                  leaflet::addCircleMarkers(
                    lat = ~lat,
                    lng = ~lng,
                    label = ~as.character(.(lab)),
                    radius = .(rad),
                    color = .pal(.g),
                    stroke = FALSE,
                    fillOpacity = 0.85
                  ) |>
                  leaflet::addLegend(pal = .pal, values = .g, title = .(cby))
              })
            )
          } else {
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
          }
        }),
        state = list(
          label = r_label,
          color = r_color,
          radius = r_radius,
          cluster = r_cluster,
          color_by = r_color_by
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

# A colour-by selection is active only when it names a real column.
isTruthy_col <- function(x) {
  length(x) == 1L && nzchar(x) && !identical(x, "(none)")
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
