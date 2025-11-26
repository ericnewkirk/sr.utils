#' @title Contour Viewer App
#'
#' @description A \code{shiny} app and components for comparing rasters at
#'   different contour levels
#'
#' @name contour_viewer
#'
#' @param inputId id of the \code{shiny} input, namespaced if needed
#' @param label label for the \code{shiny} input
#' @param ... other arguments passed to \code{shiny} input function
#' @param id \code{shiny} module id for connecting ui and server functions
#' @param raster_paths full paths to raster files to be selected
#' @param sr \code{sf} object containing seasonal range polygons
#' @param hu \code{sf} object containing herd unit polygons
#' @param cache_dir a directory for saving contour polygons as they are
#'   generated. If \code{cache_dir} is provided each contour polygon displayed
#'   is saved as a geojson file in \code{cache_dir} so it can be rendered more
#'   quickly in the future.
#'
#' @examples
#' \dontrun{
#'
#' devtools::install_github("https://github.com/WyoGFD/sr.utils")
#' library(sr.utils)
#'
#' raster_paths <- list.files(
#'   "M:/datacrunch/Newkirk/Sublette Pronghorn/output",
#'   pattern = "_avg_(core|full).tif",
#'   full.names = TRUE
#' )
#'
#' sr <- wgfd_agol_data("Antelope_Seasonal_Range")
#'
#' hu <- wgfd_agol_data("AntelopeHerdUnits")
#'
#' td <- tempdir()
#'
#' contour_viewer(raster_paths, sr, hu, td)
#' }
#'
NULL

#' @param min minimum slider value
#' @param max maximum slider value
#' @param value initial slider value
#' @param step interval between allowable slider values
#'
#' @return \code{shiny::sliderInput}
#' @export
#'
#' @seealso shiny::sliderInput
#'
#' @describeIn contour_viewer Wrapper for a slider input for selecting a contour
#'  value from 0 to 1. Just \code{shiny::sliderInput} with helpful default
#'  arguments.
#'
contour_slider <- function(
  inputId,
  label = "Contour Level:",
  min = .05,
  max = .95,
  value = 0.9,
  step = .05,
  ...
) {
  shiny::sliderInput(
    inputId,
    label = NULL,
    min = min,
    max = max,
    value = value,
    step = step,
    width = "100%",
    ...
  )
}

#' @param choice_names vector of names for the \code{choices} argument in
#'   \code{shiny::selectInput}. The values returned to the server are defined
#'   by the \code{raster_paths} argument, but names can be provided for display
#'   in the dropdown. Defaults to \code{basename(raster_paths)}.
#'
#' @return \code{shiny::selectInput}
#' @export
#'
#' @seealso shiny::selectInput
#'
#' @describeIn contour_viewer Wrapper for a select input for selecting a raster
#'  file to view. Just \code{shiny::selectInput} with helpful default arguments.
#'
raster_select <- function(
    inputId,
    raster_paths,
    label = "Raster File:",
    choice_names = basename(raster_paths),
    ...
) {
  shiny::selectInput(
    inputId,
    label = label,
    choices = stats::setNames(raster_paths, choice_names),
    width = "100%",
    ...
  )
}

#' @param title title for \code{bslib::card}
#' @param r_input logical specifying whether to include an input for selecting a
#'   raster file
#' @param c_input logical specifying whether to include an input for selecting a
#'   contour level
#'
#' @return \code{bslib::card}
#' @export
#'
#' @describeIn contour_viewer UI definition consisting of a
#'   \code{bslib::card} containing a \code{leaflet} map and optional
#'   \code{shiny} inputs for a \code{shiny} module, paired with
#'   \code{cv_map_server}
#'
cv_map_ui <- function(
  id,
  title = "Contour Viewer",
  r_input = TRUE,
  c_input = TRUE,
  raster_paths = NULL
) {
  ns <- shiny::NS(id)
  tl <- shiny::tagList()
  if (r_input) {
    tl <- shiny::tagAppendChild(tl, raster_select(ns("r"), raster_paths))
  }
  if (c_input) {
    tl <- shiny::tagAppendChild(tl, contour_slider(ns("c")))
  }
  tl <- shiny::tagAppendChild(tl, leaflet::leafletOutput(ns("map")))
  bslib::card(bslib::card_header(title), tl)
}

#' @param sr \code{sf} object containing seasonal range polygons
#' @param hu \code{sf} object containing herd unit polygons
#' @param rct_tab \code{shiny} reactive expression for passing tab change events
#'   into module. Needed for multiple-tab contexts because reactive updates via
#'   \code{leaflet::leafletProxy} only succeed when map is on screen.
#' @param rct_r \code{shiny} reactive expression for passing selected raster
#'   into module
#' @param rct_c \code{shiny} reactive expression for passing selected contour
#'   level into module
#' @param cache_dir a directory for saving contour polygons as they are
#'   generated. If \code{cache_dir} is provided each contour polygon displayed
#'   is saved as a geojson file in \code{cache_dir} so it can be rendered more
#'   quickly in the future.
#' @param sync_group character group name for syncing zoom and pan
#' @param smooth logical specifying whether to smooth contour polygons using
#'   \code{smoothr::smooth}
#'
#' @return \code{shiny::moduleServer}
#' @export
#'
#' @describeIn contour_viewer Server function for a \code{shiny} module, paired
#'   with \code{cv_map_ui}
#'
cv_map_server <- function(
  id,
  sr = NULL,
  hu = NULL,
  rct_tab = NULL,
  rct_r = NULL,
  rct_c = NULL,
  cache_dir = NULL,
  sync_group = NULL,
  smooth = FALSE
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      out <- shiny::reactiveValues(r = NULL, c = NULL)

      raster <- shiny::reactive({

        if (inherits(rct_r, "reactive")) {
          rct_r()
        } else {
          input$r
        }

      })

      contour <- shiny::reactive({

        if (inherits(rct_c, "reactive")) {
          rct_c()
        } else {
          input$c
        }

      })

      contour_data <- shiny::reactive({
        shiny::req(raster(), contour())

        if (!is.null(cache_dir)) {
          filepath <- file.path(
            cache_dir,
            sprintf(
              "%s_%02d.geojson",
              tools::file_path_sans_ext(basename(raster())),
              as.integer(contour() * 100)
            )
          )
          if (file.exists(filepath)) {
            x <- sf::read_sf(filepath)
          } else {
            x <- sf::write_sf(get_contour(raster(), contour()), filepath)
          }
        } else {
          x <- get_contour(raster(), contour())
        }

        if (smooth) {
          x <- smoothr::smooth(x, method = "ksmooth", smoothness = 5)
        }

        x

      })

      update_map <- shiny::reactive({
        if (inherits(rct_tab, "reactive")) {
          list(contour_data(), rct_tab())
        } else {
          contour_data()
        }
      })

      if (!is.null(rct_r)) {
        shiny::observeEvent(rct_r(), {
          shiny::updateSelectInput(
            session = session,
            "r",
            selected = rct_r()
          )
        })
      }

      if (!is.null(rct_c)) {
        shiny::observeEvent(rct_c(), {
          shiny::updateSliderInput(
            session = session,
            "c",
            value = rct_c()
          )
        })
      }

      output$map <- leaflet::renderLeaflet({

        og <- character(0)

        lf <- leaflet::leaflet() |>
          leaflet::addProviderTiles(
            leaflet::providers$Esri.WorldTopoMap, group = "Topo"
          ) |>
          leaflet::addProviderTiles(
            leaflet::providers$Esri.WorldImagery, group = "Aerial"
          ) |>
          leaflet::addProviderTiles(
            leaflet::providers$Esri.NatGeoWorldMap, group = "NatGeo"
          )

        if (!is.null(sr)) {
          og <- c(og, "Existing Seasonal Range")
          lf <- lf |>
            addSR(sr, hide = FALSE)
        }

        if (!is.null(hu)) {
          og <- c(og, "Herd Unit Boundaries")
          lf <- lf |>
            leaflet::addPolygons(
              data = hu,
              group = "Herd Unit Boundaries",
              color = "#000000",
              weight = 2,
              opacity = 1,
              fillOpacity = 0
            )
        }

        if (!is.null(sync_group)) {
          lf <- lf |>
            leaflet.minicharts::syncWith(sync_group)
        }

        lf |>
          leaflet::addLayersControl(
            baseGroups = c("Topo", "Aerial", "NatGeo"),
            overlayGroups = og,
            options = leaflet::layersControlOptions(collapsed = TRUE)
          ) |>
          leaflet::addLegend(
            colors = "#C72C41",
            opacity = 0.5,
            labels = "Contour"
          ) |>
          leaflet::hideGroup(og)

      })

      lf_prx <- leaflet::leafletProxy("map", session)

      shiny::observeEvent(update_map(), {

        lf_prx |>
          leaflet::clearGroup("contours") |>
          leaflet::addPolygons(
            data = contour_data(),
            color = "#C72C41",
            fill = "#C72C41",
            opacity = 0.5,
            group = "contours"
          ) |>
          fitRaster(raster())

      })

      shiny::observeEvent(input$r, out$r <- input$r)
      shiny::observeEvent(input$c, out$c <- input$c)

      return(out)

    }
  )
}

#' @return \code{shiny::shinyApp}
#' @export
#'
#' @describeIn contour_viewer \code{shiny} app for comparing UD rasters
#'
contour_viewer <- function(
  raster_paths,
  sr = NULL,
  hu = NULL,
  cache_dir = NULL,
  smooth = FALSE
) {

  ui <- function(request) {
    bslib::page_navbar(
      id = "cvtab",
      title = shiny::tags$h1(
        shiny::tags$span(
          shiny::tags$img(
            src = "https://wgfd.wyo.gov/themes/custom/wgfd/images/logo.png",
            width = "60px",
            height = "auto",
            class = "me-3",
            alt = "WGFD logo"
          ),
          "Contour Viewer"
        )
      ),
      footer = shiny::div(
        shiny::helpText("\u00A9 2025 WGFD - SRA Unit"),
        style = "text-align: center;"
      ),
      theme = bslib::bs_theme(
        fg = "#00374D",
        bg = "#FFFFFF",
        base_font = "'Source Sans Pro', Helvetica, sans-serif;",
        heading_font = "Oswald, 'Open Sans', Helvetica, sans-serif;",
        primary = "#00374D"
      ),
      navbar_options = bslib::navbar_options(collapsible = FALSE),
      bslib::nav_spacer(),
      # shared contour, different rasters
      bslib::nav_panel(
        title = "Compare Rasters",
        bslib::card(
          bslib::card_header("Contour level:"),
          contour_slider("cfx", label = NULL)
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          cv_map_ui(
            "fc1",
            "Raster 1",
            c_input = FALSE,
            raster_paths = raster_paths
          ),
          cv_map_ui(
            "fc2",
            "Raster 2",
            c_input = FALSE,
            raster_paths = raster_paths
          )
        )
      ),
      # shared raster, different contours
      bslib::nav_panel(
        title = "Compare Contours",
        bslib::card(
          bslib::card_header("Raster file:"),
          raster_select("rfx", raster_paths, label = NULL)
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          cv_map_ui(
            "fr1",
            "Contour 1",
            r_input = FALSE,
            raster_paths = raster_paths
          ),
          cv_map_ui(
            "fr2",
            "Contour 2",
            r_input = FALSE,
            raster_paths = raster_paths
          )
        )
      ),
      # independent raster and contours
      bslib::nav_panel(
        title = "Free Compare",
        bslib::layout_columns(
          col_widths = c(6, 6),
          cv_map_ui(
            "free1",
            raster_paths = raster_paths
          ),
          cv_map_ui(
            "free2",
            raster_paths = raster_paths
          )
        )
      )
    )
  }

  server <- function(input, output, session) {

    rct_tab <- shiny::reactive(input$cvtab)
    rct_r <- shiny::reactive(input$rfx)
    rct_c <- shiny::reactive(input$cfx)

    cv_map_server(
      "fc1", sr, hu,
      rct_tab = rct_tab, rct_c = rct_c,
      cache_dir = cache_dir, sync_group = "fc", smooth = TRUE
    )
    cv_map_server(
      "fc2", sr, hu,
      rct_tab = rct_tab, rct_c = rct_c,
      cache_dir = cache_dir, sync_group = "fc", smooth = TRUE
    )

    cv_map_server(
      "fr1", sr, hu,
      rct_tab = rct_tab, rct_r = rct_r,
      cache_dir = cache_dir, sync_group = "fr", smooth = TRUE
    )
    cv_map_server(
      "fr2", sr, hu,
      rct_tab = rct_tab, rct_r = rct_r,
      cache_dir = cache_dir, sync_group = "fr", smooth = TRUE
    )

    cv_map_server(
      "free1", sr, hu,
      rct_tab = rct_tab,
      cache_dir = cache_dir, smooth = TRUE
    )
    cv_map_server(
      "free2", sr, hu,
      rct_tab = rct_tab,
      cache_dir = cache_dir, smooth = TRUE
    )

  }

  shiny::shinyApp(ui, server)

}
