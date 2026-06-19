# Helpers for snapping points-of-interest onto a route. Used by the
# route block at expression-eval time, so snap_pois is exported.

haversine_m <- function(lat1, lng1, lat2, lng2) {
  r <- 6371000
  d <- pi / 180
  a <- sin((lat2 - lat1) * d / 2)^2 +
    cos(lat1 * d) * cos(lat2 * d) * sin((lng2 - lng1) * d / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

poi_color <- function(type) {
  vapply(
    type,
    function(t) {
      switch(t,
        col = "#CC3311",
        berg = "#EE7733",
        cobble_sector = "#8856A7",
        gravel_sector = "#B07A3C",
        finish = "#222222",
        "#666666"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Snap points of interest onto a route
#'
#' Snaps each point of interest to the nearest point of a route track,
#' keeping only those within `tol_m`. Useful for overlaying named course
#' features on a GPS route regardless of which activity is shown: the
#' same gazetteer can be passed everywhere and only the features the
#' route actually passes are returned.
#'
#' @param pois A data frame with `name`, `lat`, `lng`, and (optionally)
#'   `type` columns, or `NULL`.
#' @param route A data frame with `lat` and `lng` columns (the track).
#' @param tol_m Maximum distance (m) from the track to keep a feature.
#'   Default `1200`.
#'
#' @return A data frame with `lat`, `lng` (snapped to the track),
#'   `label`, and `color`, or `NULL` if nothing matches.
#'
#' @export
snap_pois <- function(pois, route, tol_m = 1200) {
  if (is.null(pois) || nrow(pois) == 0L) return(NULL)
  stopifnot(is.data.frame(route), all(c("lat", "lng") %in% names(route)))
  r <- route[!is.na(route$lat) & !is.na(route$lng), , drop = FALSE]
  if (nrow(r) == 0L) return(NULL)
  type <- if ("type" %in% names(pois)) pois$type else rep("", nrow(pois))
  out <- list()
  for (j in seq_len(nrow(pois))) {
    d <- haversine_m(pois$lat[j], pois$lng[j], r$lat, r$lng)
    k <- which.min(d)
    if (d[k] > tol_m) next
    out[[length(out) + 1L]] <- data.frame(
      lat = r$lat[k], lng = r$lng[k],
      label = pois$name[j], color = poi_color(type[j]),
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}
