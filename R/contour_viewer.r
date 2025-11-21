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

#' @param title title for \code{shinydashboard::box}
#' @param r_input logical specifying whether to include an input for selecting a
#'   raster file
#' @param c_input logical specifying whether to include an input for selecting a
#'   contour level
#'
#' @return \code{shinydashboard::box}
#' @export
#'
#' @describeIn contour_viewer UI definition consisting of a
#'   \code{shinydashboard::box} containing a \code{leaflet} map and optional
#'   \code{shiny} inputs for a \code{shiny} module, paired with
#'   \code{cv_map_server}
#'
cv_map_ui <- function(
  id,
  title = "Contour Viewer",
  r_input = TRUE,
  c_input = FALSE,
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
  shinydashboard::box(tl, width = 12, title = title)
}

#' @param sr \code{sf} object containing seasonal range polygons
#' @param hu \code{sf} object containing herd unit polygons
#' @param rct_r \code{shiny} reactive expression for passing selected raster
#'   into module
#' @param rct_c \code{shiny} reactive expression for passing selected contour
#'   level into module
#' @param cache_dir a directory for saving contour polygons as they are
#'   generated. If \code{cache_dir} is provided each contour polygon displayed
#'   is saved as a geojson file in \code{cache_dir} so it can be rendered more
#'   quickly in the future.
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
  rct_r = NULL,
  rct_c = NULL,
  cache_dir = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      out <- shiny::reactiveValues(r = NULL, c = NULL)

      wr_data <- shiny::reactive({

        r <- if (inherits(rct_r, "reactive")) {
          rct_r()
        } else {
          input$r
        }

        c <- if (inherits(rct_c, "reactive")) {
          rct_c()
        } else {
          input$c
        }

        shiny::req(r, c)

        if (!is.null(cache_dir)) {
          filepath <- file.path(
            cache_dir,
            sprintf(
              "%s_%02d.geojson",
              tools::file_path_sans_ext(basename(r)),
              as.integer(c * 100)
            )
          )
          if (file.exists(filepath)) {
            x <- sf::st_read(filepath)
          } else {
            x <- sf::st_write(get_contour(r, c), filepath)
          }
        } else {
          x <- get_contour(r, c)
        }

        x

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

      shiny::observeEvent(wr_data(), {

        bounds <- wr_data() |>
          sf::st_bbox() |>
          as.character()

        lf_prx |>
          leaflet::clearGroup("wr") |>
          leaflet::addPolygons(
            data = wr_data(),
            color = "#C72C41",
            fill = "#C72C41",
            opacity = 0.5,
            group = "wr"
          ) |>
          leaflet::fitBounds(
            bounds[1], bounds[2], bounds[3], bounds[4]
          )

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
  cache_dir = NULL
) {

  ui <- function(request) {
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = "sr.utils"),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            "Compare Rasters",
            icon = shiny::icon("map", lib = "font-awesome"),
            tabName = "fixed_c",
            selected = TRUE
          ),
          shinydashboard::menuItem(
            "Compare Contours",
            icon = shiny::icon("percent", lib = "font-awesome"),
            tabName = "fixed_r"
          ),
          shinydashboard::menuItem(
            "Free Compare",
            icon = shiny::icon("pencil", lib = "font-awesome"),
            tabName = "free"
          )
        )
      ),
      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "fixed_c",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Contour level:",
                  contour_slider("cfx", label = NULL)
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                cv_map_ui("fc1", "Raster 1", raster_paths = raster_paths)
              ),
              shiny::column(
                width = 6,
                cv_map_ui("fc2", "Raster 2", raster_paths = raster_paths)
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "fixed_r",
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinydashboard::box(
                  width = 12,
                  title = "Raster file:",
                  raster_select("rfx", raster_paths, label = NULL)
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                cv_map_ui(
                  "fr1",
                  "Contour 1",
                  c_input = TRUE,
                  r_input = FALSE,
                  raster_paths = raster_paths
                )
              ),
              shiny::column(
                width = 6,
                cv_map_ui(
                  "fr2",
                  "Contour 2",
                  c_input = TRUE,
                  r_input = FALSE,
                  raster_paths = raster_paths
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "free",
            shiny::fluidRow(
              shiny::column(
                width = 6,
                cv_map_ui("free1", c_input = TRUE, raster_paths = raster_paths)
              ),
              shiny::column(
                width = 6,
                cv_map_ui("free2", c_input = TRUE, raster_paths = raster_paths)
              )
            )
          )
        ),
        shiny::tags$head(
          shiny::tags$style(shiny::HTML("
            .box-title {
              width: 100%;
            }
          "))
        )
      )
    )
  }

  server <- function(input, output, session) {

    rct_r <- shiny::reactive(input$rfx)
    rct_c <- shiny::reactive(input$cfx)

    cv_map_server("fc1", sr, hu, rct_c = rct_c, cache_dir = cache_dir)
    cv_map_server("fc2", sr, hu, rct_c = rct_c, cache_dir = cache_dir)

    cv_map_server("fr1", sr, hu, rct_r = rct_r, cache_dir = cache_dir)
    cv_map_server("fr2", sr, hu, rct_r = rct_r, cache_dir = cache_dir)

    cv_map_server("free1", sr, hu, cache_dir = cache_dir)
    cv_map_server("free2", sr, hu, cache_dir = cache_dir)

  }

  shiny::shinyApp(ui, server)

}
