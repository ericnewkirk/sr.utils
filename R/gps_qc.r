#' @title GPS Quality Control App
#'
#' @description A \code{shiny} app and components for checking for issues in
#'   GPS data
#'
#' @name gps_qc
#'
#' @param lf \code{leaflet} object to be modified
#' @param pal palette for fix color based on timestamp
#' @param fix,fixes gps locations to be added to \code{leaflet} map
#'
#' @examples
#' \dontrun{
#'
#' devtools::install_github("https://github.com/WyoGFD/sr.utils")
#' library(sr.utils)
#'
#' gps_path <- file.path(
#'   "M:/SeasonalRangeUpdates/Data/BS516",
#'   "bs516_ctmm_data.rds"
#' )
#'
#' gps_qc(gps_path)
#' 
#' }
#'
NULL

load_raw_data <- function(gps_file_path) {

  ex <- tools::file_ext(gps_file_path)

  if (ex == "rds") {
    invisible(sf::st_crs(4326))
    read_fn <- readRDS
  } else {
    read_fn <- sf::read_sf
  }

  suppressWarnings(try(read_fn(gps_file_path), silent = TRUE))

}

get_data_by_id <- function(gps_data, filter_id) {

  gps_data |>
    dplyr::filter(.data$id == .env$filter_id) |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(
      dist_m = as.numeric(sf::st_distance(
        geometry,
        dplyr::lag(geometry),
        by_element = TRUE
      )),
      elapsed = as.numeric(
        difftime(timestamp, dplyr::lag(timestamp), units = "secs")
      ),
      speed_mps = dist_m / elapsed
    ) |>
    sf::st_transform(4326)

}

addAllFixes <- function(lf, pal, fixes) {

  lf |>
    leaflet::clearMarkers() |>
    leaflet::addCircleMarkers(
      data = fixes,
      weight = 1,
      radius = 3,
      popup = ~ sprintf(
        "<strong>%s</strong><br>
          <strong>%s</strong><br><br>
          Distance: %s<br>
          Speed: %s",
        id,
        timestamp,
        format(round(dist_m, 2), big.mark = ","),
        format(round(speed_mps * 2.236936, 6))
      ),
      color = ~ pal(timestamp),
      fillColor = ~ pal(timestamp),
      opacity = 1,
      fillOpacity = 0.5
    )

}

addSingleFix <- function(lf, pal, fix) {

  lf |>
    leaflet::addCircleMarkers(
      data = fix,
      weight = 1,
      radius = 3,
      popup = ~ sprintf(
        "<strong>%s</strong><br>
          <strong>%s</strong><br><br>
          Distance: %s<br>
          Speed: %s",
        id,
        timestamp,
        format(round(dist_m, 2), big.mark = ","),
        format(round(units::set_units(speed_mps, mi/h), 6))
      ),
      color = ~ pal(timestamp),
      fillColor = ~ pal(timestamp),
      opacity = 1,
      fillOpacity = 0.5
    )

}

addFixLine <- function(lf, fixes, color = "#BBBBBB") {

  lf |>
    leaflet::clearShapes() |>
    leaflet::addPolylines(
      data = fixes |>
        dplyr::summarize(do_union = FALSE) |>
        sf::st_cast("LINESTRING"),
      weight = 1,
      color = color
    )

}

clearFixes <- function(lf) {

  lf |>
    leaflet::clearMarkers()

}

updateLegend <- function(lf, pal, gps_data) {

  if (nrow(gps_data) > 0) {
    lf |>
      leaflet::removeControl("lgnd") |>
      leaflet::addLegend(
        data = gps_data,
        pal = pal,
        values = ~ as.numeric(timestamp),
        labFormat = function(type, x) {
          as.Date(as.POSIXct(x, origin = "1970-01-01"))
        },
        title = "Timestamp",
        layerId = "lgnd"
      )
  } else {
    lf |>
      leaflet::removeControl("lgnd")
  }

}

anim_icon <- leaflet::makeIcon(
  "https://cdn-icons-png.flaticon.com/512/865/865405.png",
  iconWidth = 41,
  iconHeight = 41,
  iconAnchorX = 21,
  iconAnchorY = 21
)

gps_qc <- function(gps_file_path) {

  raw_data <- load_raw_data(gps_file_path)

  ui <- function(request) {
    shiny::fluidPage(
      bslib::page_sidebar(
        title = shiny::tags$h1(
          shiny::tags$span(
            shiny::tags$img(
              src = "https://wgfd.wyo.gov/themes/custom/wgfd/images/logo.png",
              width = "60px",
              height = "auto",
              class = "me-3",
              alt = "WGFD logo"
            ),
            "GPS Quality Control"
          )
        ),
        theme = bslib::bs_theme(
          fg = "#00374D",
          bg = "#FFFFFF",
          base_font = "'Source Sans Pro', Helvetica, sans-serif;",
          heading_font = "Oswald, 'Open Sans', Helvetica, sans-serif;",
          primary = "#00374D"
        ),
        sidebar = bslib::sidebar(
          bslib::accordion(
            bslib::accordion_panel(
              "Current Track",
              shinyjs::disabled(shiny::selectInput("id", "ID:", "")),
              shiny::uiOutput("gps_info")
            ),
            bslib::accordion_panel(
              "Display Settings",
              shinyjs::disabled(shiny::selectInput(
                "pal",
                "Palette:",
                c(
                  "viridis", "magma", "inferno", "plasma",
                  RColorBrewer::brewer.pal.info |>
                    dplyr::filter(.data$category == "seq") |>
                    rownames()
                )
              )),
              shinyjs::disabled(shiny::checkboxInput(
                "anim",
                "Animation",
                FALSE
              ))
            ),
            bslib::accordion_panel(
              "Stationary Periods",
              shinyjs::disabled(
                shiny::actionButton(
                  "check_stat",
                  "Check",
                  style = "width: 100%;"
                )
              ),
              shiny::uiOutput("stat_results")
            ),
            bslib::accordion_panel(
              "Outliers",
              shinyjs::disabled(
                shiny::actionButton("check_ol", "Check", style = "width: 100%;")
              ),
              shiny::uiOutput("ol_results")
            )
          )
        ),
        leaflet::leafletOutput("gps_map", height = 600),
        shiny::div(
          shinyjs::disabled(shiny::sliderInput(
            "view_ts",
            "Display:",
            min = Sys.Date() - lubridate::days(30),
            max = Sys.Date() + lubridate::days(30),
            value = Sys.Date() + lubridate::days(c(-7, 7)),
            step = 1,
            width = "100%"
          )),
          style = "width: 80%; margin-left: 10%"
        )
      ),
      shiny::tags$footer(
        shinyjs::useShinyjs(),
        shinyWidgets::chooseSliderSkin("Modern", color = "#00374D"),
        shiny::helpText("\u00A9 2025 WGFD - SRA Unit"),
        style = "text-align: center; position: fixed; bottom: 0; width: 100%;"
      )
    )
  }

  server <- function(input, output, session) {

    # reactive values
    rct <- shiny::reactiveValues(
      # TODO need setup ui to convert raw_data to gps_data
      gps_data = NULL
    )

    # load data
    rct$gps_data <- raw_data |>
      sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
      dplyr::select(dplyr::all_of(c("id", timestamp = "DateTimeUTC")))

    # populate id select input
    shiny::observe({

      shiny::updateSelectInput(
        session = session,
        "id",
        choices = sort(unique(rct$gps_data$id))
      )
      
      enable <- inherits(rct$gps_data, "sf") && nrow(rct$gps_data) > 0
      shinyjs::toggleState("id", condition = enable)
      shinyjs::toggleState("pal", condition = enable)
      shinyjs::toggleState("anim", condition = enable)
      shinyjs::toggleState("view_ts", condition = enable)

    })

    # base map
    output$gps_map <- leaflet::renderLeaflet({

      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTopoMap, group = "Topo"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery, group = "Aerial"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.NatGeoWorldMap, group = "NatGeo"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("Topo", "Aerial", "NatGeo"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )

    })

    # proxy for updating map
    lf_prx <- leaflet::leafletProxy("gps_map")

    # update fix data
    map_data <- shiny::reactive({
      shiny::req(input$id)

      get_data_by_id(rct$gps_data, input$id)

    })

    # update date range when new id selected
    se_dates <- shiny::reactive({
      shiny::req(map_data())

      out <- c(
        min(map_data()$timestamp),
        max(map_data()$timestamp)
      )
      
      shiny::updateSliderInput(
        session = session,
        "view_ts",
        value = as.Date(out) + c(0, 1),
        min = as.Date(out[1]),
        max = as.Date(out[2]) + 1
      )

      out

    })

    # update map when id changes
    shiny::observeEvent(map_data(), {

      lf_prx |>
        clearFixes() |>
        spatial.utils::zoom_to_data(map_data()) |>
        addFixLine(map_data())

    })

    # reactive palette based on fix times and input selection
    # TODO make this align with OE above that updates slider
    rpal <- shiny::reactive({
      shiny::req(se_dates(), input$pal)

      leaflet::colorNumeric(
        palette = input$pal,
        domain = as.numeric(seq(
          from = se_dates()[1],
          to = se_dates()[2] + lubridate::hours(1),
          by = "hour"
        ))
      )

    })

    # filter fixes based on slider
    filtered_data <- shiny::reactive({
      shiny::req(input$view_ts)

      map_data() |>
        dplyr::filter(
          dplyr::between(timestamp, input$view_ts[1], input$view_ts[2])
        )

    })

    # update legend when data or palette changes and redraw points
    shiny::observe({
      shiny::req(rpal(), filtered_data())

      lf_prx |>
        updateLegend(rpal(), filtered_data())

      lf_prx |>
        addAllFixes(rpal(), filtered_data())

    })

    # animation
    shiny::observe({

      lf_prx |>
          leaflet.extras2::removePlayback()

      if (input$anim) {

        lf_prx |>
          leaflet.extras2::addPlayback(
            data = filtered_data(),
            time = "timestamp",
            name = ~ id,
            icon = anim_icon,
            options = leaflet.extras2::playbackOptions(
              tickLen = 60 * 60 * 1000,
              speed = 24 * 60 * 60,
              maxInterpolationTime = 12 * 60 * 60 * 1000,
              orientIcons = FALSE,
              tracksLayer = FALSE
            )
          )

      }

    })

  }

  shiny::shinyApp(ui, server)

}
