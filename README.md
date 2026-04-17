# blockr.leaflet

Leaflet map blocks for [blockr](https://github.com/BristolMyersSquibb/blockr). Renders GPS route data on interactive maps.

## Installation

```r
pak::pkg_install("cynkra/blockr.leaflet")
```

## Usage

```r
library(blockr)
library(blockr.leaflet)

serve(
  new_board(
    blocks = list(
      data = new_read_block(path = "gps_data.rds"),
      select = new_select_block(columns = c("lat", "lng")),
      map = new_leaflet_route_block()
    ),
    links = list(
      new_link("data", "select", "data"),
      new_link("select", "map", "data")
    )
  )
)
```

## Block

### `new_leaflet_route_block()`

Plots a GPS route on an interactive Leaflet map. Expects a data frame with `lat` and `lng` columns.

**Parameters:**

| Parameter | Default | Description |
|-----------|---------|-------------|
| `color` | `"#000000"` | Line colour (hex) |
| `weight` | `3` | Line weight in pixels |
| `opacity` | `0.8` | Line opacity (0–1) |

All parameters are configurable via the block UI at runtime.
