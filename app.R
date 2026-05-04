# NYC Air Quality Dashboard
# Author: Chenchong Xia, Luobing Wang
# Course: SYSEN 5460

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(broom)
library(lubridate)
library(bslib)
library(bsicons)
library(sf)
library(leaflet)
library(mgcv)   # for GAM smoother

county_lookup <- c(
  "34003" = "Bergen",      "34017" = "Hudson",
  "34019" = "Hunterdon",   "34023" = "Middlesex",
  "34027" = "Morris",      "34029" = "Gloucester",
  "34031" = "Passaic",     "34039" = "Ocean",
  "36005" = "Bronx",       "36047" = "Kings/Brooklyn",
  "36061" = "New York/Manhattan", "36081" = "Queens",
  "36085" = "Richmond/Staten Island", "36087" = "Rockland",
  "36103" = "Suffolk",     "36119" = "Westchester"
)

CLR_BLUE   <- "#2C7BB6"
CLR_RED    <- "#d9534f"
CLR_GREEN  <- "#4DAC26"
CLR_AMBER  <- "#e0a800"
CLR_PURPLE <- "#8e44ad"
CLR_NAVY   <- "#1a4f7a"

# dist column unit: kilometres
# Congestion zone threshold: 5 km
DIST_THRESHOLD_KM <- 5

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    bootswatch   = "cosmo",
    primary      = CLR_BLUE,
    secondary    = CLR_GREEN,
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(tags$style(HTML(paste0("
    body { background:#f4f6f9; }
    .main-title {
      background: linear-gradient(90deg,", CLR_BLUE, ",", CLR_NAVY, ");
      color: white; padding:16px 22px; border-radius:8px;
      margin-bottom:18px; font-size:1.35rem; font-weight:700;
    }
    .sec-hdr {
      border-left:4px solid ", CLR_BLUE, ";
      padding-left:9px; margin:14px 0 8px;
      font-weight:600; color:", CLR_NAVY, "; font-size:1rem;
    }
    .info-box {
      background:#fff; border:1px solid #dce3ea;
      border-left:4px solid ", CLR_BLUE, ";
      border-radius:6px; padding:11px 14px;
      font-size:0.86rem; color:#3d4e5c; line-height:1.55;
    }
    .info-box.green  { border-left-color:", CLR_GREEN, "; }
    .info-box.red    { border-left-color:", CLR_RED, "; }
    .info-box.amber  { border-left-color:", CLR_AMBER, "; }
    .info-box.purple { border-left-color:", CLR_PURPLE, "; }
    .info-box b { color:#1a2533; }
    .atext {
      background:#fff; border:1px solid #dce3ea; border-radius:6px;
      padding:10px 14px; font-size:0.9rem; color:#2c3e50; line-height:1.6;
    }
    .trend-card { border-radius:6px; padding:12px 15px; font-size:0.88rem; line-height:1.6; }
    .trend-sig   { background:#fff8e1; border-left:4px solid ", CLR_AMBER, "; color:#4a3800; }
    .trend-nosig { background:#e8f5e9; border-left:4px solid ", CLR_GREEN, "; color:#1b4d1f; }
    .pill-row { display:flex; flex-wrap:wrap; gap:7px; margin-top:9px; }
    .pill {
      background:#f0f4f8; border:1px solid #ccd6e0;
      border-radius:20px; padding:2px 10px;
      font-size:0.82rem; font-weight:600; white-space:nowrap; color:#1a2533;
    }
    .sidebar { background:#fff; border-right:1px solid #dce3ea; min-height:100vh; padding-top:10px; }
    .well { box-shadow:none !important; border:none !important; }
    hr { border-color:#e5eaf0; margin:10px 0; }
    .select-links { font-size:0.82rem; margin-bottom:6px; }
    .select-links a { color:", CLR_BLUE, "; cursor:pointer; text-decoration:underline; margin-right:8px; }
  ")))),
  
  div(class = "main-title",
      "\U0001F4CD NYC Air Quality Dashboard",
      tags$span(style = "font-size:0.82rem;font-weight:400;margin-left:14px;opacity:.85;",
                "Luobing Wang & Chenchong Xia | SYSEN 5460")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar", width = 3,
      tags$p(style = "font-weight:700;font-size:1rem;margin-bottom:4px;", "\u2699\uFE0F Controls"),
      tags$hr(),
      radioButtons("tab", "View:",
                   choices = c("\U0001F4CA Overview"               = "overview",
                               "\U0001F4C8 Temporal Analysis"      = "temporal",
                               "\U0001F5FA\uFE0F Spatial Analysis"  = "spatial"),
                   selected = "overview"),
      tags$hr(),
      selectInput("year_range", tags$b("Year(s):"),
                  choices  = c(2018, 2019, 2024, 2025),
                  selected = c(2024, 2025), multiple = TRUE),
      tags$hr(),
      # FIX: county selector now has Select All / None links
      uiOutput("county_selector"),
      tags$hr(),
      tags$small(style = "color:#666;",
                 tags$b("Note: "), "Units: \u03bcg/m\u00b3. Data: 2018, 2019, 2024, 2025.")
    ),
    
    mainPanel(width = 9,
              
              # ── Overview ──────────────────────────────────────────────────────────
              conditionalPanel("input.tab == 'overview'",
                               tags$h4(class = "sec-hdr", "Key Metrics"),
                               div(class = "atext", textOutput("summary_sentence")),
                               div(class = "atext", style = "margin-top:4px;", textOutput("data_note")),
                               br(),
                               tags$h5(class = "sec-hdr", "Detailed Statistics"),
                               fluidRow(
                                 value_box("Mean PM2.5",   textOutput("text_mean"),
                                           showcase = bs_icon("calculator-fill"),  class = "bg-primary"),
                                 value_box("Std Error",    textOutput("text_se"),
                                           showcase = bs_icon("plus-slash-minus"), class = "bg-success"),
                                 value_box("Observations", textOutput("text_n"),
                                           showcase = bs_icon("database-fill"),    class = "bg-info")
                               ),
                               br(),
                               tags$h5(class = "sec-hdr", "Network Disparity Index"),
                               fluidRow(
                                 column(7,
                                        div(class = "atext", textOutput("ndi_text")),
                                        br(),
                                        div(class = "atext", textOutput("gini_text"))
                                 ),
                                 column(5,
                                        div(class = "info-box amber",
                                            tags$b("What is the Network Disparity Index?"),
                                            tags$p(style = "margin:6px 0 0;",
                                                   "The NDI compares the site with the highest mean PM2.5 to the ",
                                                   "site with the lowest, expressed as a ratio. An NDI of 2 means ",
                                                   "the most polluted site is twice as bad as the cleanest."
                                            ),
                                            tags$p(style = "margin:8px 0 0;",
                                                   "The ", tags$b("Gini coefficient"), " (0\u20131) measures inequality: ",
                                                   "0 = all sites identical, 1 = one site holds all pollution."
                                            )
                                        )
                                 )
                               )
              ),
              
              # ── Temporal ──────────────────────────────────────────────────────────
              conditionalPanel("input.tab == 'temporal'",
                               tags$h4(class = "sec-hdr", "Hourly PM2.5 Time Series"),
                               div(class = "atext",
                                   "Hover over any line to see monitor ID, county, and exact reading. ",
                                   "Click a county name in the legend to isolate it."),
                               br(),
                               plotlyOutput("ts_plot", height = "420px", width = "100%"),
                               br(),
                               tags$h4(class = "sec-hdr", "Monthly Average Trend"),
                               # FIX: description updated to reflect GAM smoother
                               div(class = "atext",
                                   "The curve below uses a ", tags$b("Generalized Additive Model (GAM)"),
                                   " smoother to capture the nonlinear seasonal pattern in monthly PM2.5. ",
                                   "The shaded band shows the 95% confidence interval. ",
                                   "Unlike a straight linear fit, the GAM flexibly tracks seasonal rises and dips."),
                               br(),
                               plotlyOutput("monthly_trend", height = "380px", width = "100%"),
                               br(),
                               div(class = "atext", textOutput("trend_text"))
              ),
              
              # ── Spatial ───────────────────────────────────────────────────────────
              conditionalPanel("input.tab == 'spatial'",
                               fluidRow(
                                 column(4,
                                        value_box(
                                          # FIX: subtitle now explains direction
                                          "Congestion \u2212 Remote \u0394PM2.5",
                                          textOutput("concentration_diff_value"),
                                          showcase = bs_icon("geo-alt-fill"),
                                          class = "bg-warning"
                                        )
                                 ),
                                 column(4,
                                        value_box("Hotspot Sites",
                                                  textOutput("hotspot_count_value"),
                                                  showcase = bs_icon("exclamation-triangle-fill"), class = "bg-danger")
                                 ),
                                 column(4,
                                        value_box("Monitoring Sites",
                                                  textOutput("n_sites_value"),
                                                  showcase = bs_icon("pin-map-fill"), class = "bg-primary")
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column(6,
                                        tags$h5(class = "sec-hdr", "Proximity Analysis"),
                                        div(class = "atext", textOutput("spatial_summary_sentence")),
                                        br(),
                                        div(class = "info-box amber",
                                            tags$b("How the \u0394 is calculated"),
                                            tags$p(style = "margin:6px 0 0;",
                                                   "All hourly PM2.5 readings from stations with ",
                                                   tags$code(paste0("dist \u2264 ", DIST_THRESHOLD_KM, " km")),
                                                   " are averaged (congestion mean). Readings from stations with ",
                                                   tags$code(paste0("dist > ", DIST_THRESHOLD_KM, " km")),
                                                   " form the remote mean. Reported value = ",
                                                   tags$b("congestion mean \u2212 remote mean."),
                                                   " A positive value means stations near the congestion zone show higher PM2.5."
                                            )
                                        )
                                 ),
                                 column(6,
                                        tags$h5(class = "sec-hdr", "Spatial Interpolation"),
                                        div(class = "atext", textOutput("interpolation_insight_text")),
                                        br(),
                                        div(class = "info-box green",
                                            tags$b("CV & spatial correlation"),
                                            tags$p(style = "margin:6px 0 0;",
                                                   "CV = SD / mean across site-level averages. ",
                                                   "Spatial correlation = Pearson r between pairwise inter-site ",
                                                   "distance (metres, EPSG:3857) and absolute PM2.5 difference."
                                            )
                                        )
                                 )
                               ),
                               br(),
                               tags$h5(class = "sec-hdr", "Hotspot Identification"),
                               fluidRow(
                                 column(7, div(class = "atext", textOutput("hotspot_analysis_text"))),
                                 column(5,
                                        div(class = "info-box red",
                                            tags$b("Hotspot threshold"),
                                            tags$p(style = "margin:6px 0 0;",
                                                   "A site is flagged when its mean PM2.5 exceeds ",
                                                   tags$b("network mean + 1 SD"), "."
                                            )
                                        )
                                 )
                               ),
                               tags$hr(),
                               tags$h4(class = "sec-hdr",
                                       "Spatial Regression: Distance to Congestion Zone vs PM2.5"),
                               fluidRow(
                                 column(7, plotOutput("spatial_scatter", height = "360px")),
                                 column(5,
                                        div(class = "info-box",
                                            tags$b("Reading this chart"),
                                            tags$p(style = "margin:6px 0 0;",
                                                   "Each point = one monitoring station (dist in km, ",
                                                   "recomputed live via ", tags$code("sf::st_distance()"), "). ",
                                                   tags$b("Downward slope"), " \u2192 closer to congestion zone = more polluted. ",
                                                   tags$b("Flat/upward"), " \u2192 other factors dominate."
                                            )
                                        ),
                                        br(),
                                        uiOutput("scatter_trend_card")
                                 )
                               ),
                               tags$p(style = "color:#888;font-size:0.82rem;font-style:italic;text-align:center;margin-top:4px;",
                                      "Distance (km) recomputed from each AQS site centroid to the NYC congestion zone reference point using sf::st_distance() in UTM Zone 18N (EPSG:32618)."),
                               tags$hr(),
                               tags$h4(class = "sec-hdr",
                                       "Spatial Distribution Map: Mean PM2.5 by Monitoring Site"),
                               fluidRow(
                                 column(8, leafletOutput("spatial_map", height = "430px")),
                                 column(4,
                                        div(class = "info-box purple",
                                            tags$b("Reading the map"),
                                            tags$p(style = "margin:6px 0 0; font-size:0.86rem; color:#3d4e5c;",
                                                   tags$b("Fill color"), " (RdYlGn scale) shows mean PM2.5 \u2014 ",
                                                   "see the bottom-right legend for the exact scale."
                                            ),
                                            tags$p(style = "margin:8px 0 0; font-size:0.86rem; color:#3d4e5c;",
                                                   tags$b("Ring color"), " identifies site classification:"
                                            ),
                                            tags$ul(style = "font-size:0.86rem;color:#3d4e5c;padding-left:16px;margin:4px 0 8px;",
                                                    tags$li(
                                                      tags$span(style = paste0(
                                                        "display:inline-block;width:14px;height:14px;border-radius:50%;",
                                                        "border:3px solid ", CLR_RED, ";background:transparent;",
                                                        "margin-right:5px;vertical-align:middle;"
                                                      )),
                                                      tags$b(style = paste0("color:", CLR_RED, ";"), "Red ring"),
                                                      " \u2014 Hotspot (mean > \u03bc + 1 SD)"
                                                    ),
                                                    tags$li(
                                                      tags$span(style = paste0(
                                                        "display:inline-block;width:14px;height:14px;border-radius:50%;",
                                                        "border:3px solid ", CLR_BLUE, ";background:transparent;",
                                                        "margin-right:5px;vertical-align:middle;"
                                                      )),
                                                      tags$b(style = paste0("color:", CLR_BLUE, ";"), "Blue ring"),
                                                      " \u2014 Normal site"
                                                    )
                                            ),
                                            tags$p(style = "margin:4px 0 0; font-size:0.86rem; color:#3d4e5c;",
                                                   tags$b("Size"), " \u221d mean PM2.5. Click any circle for details."
                                            ),
                                            tags$p(style = paste0("margin:8px 0 0;font-size:0.84rem;color:", CLR_RED, ";"),
                                                   tags$b("Note: "), "Gaps = missing monitors, not clean air."
                                            )
                                        )
                                 )
                               ),
                               tags$p(style = "color:#888;font-size:0.82rem;font-style:italic;text-align:center;margin-top:4px;",
                                      "Source: AQS monitoring network. Mean over all hourly readings in selected year(s) and counties.")
              )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  raw_data <- readRDS("merged_air_quality_sf.rds") %>%
    mutate(
      datetime    = lubridate::ymd_hms(datetime, tz = "UTC"),
      county_id   = as.character(county),
      county_name = dplyr::recode(as.character(county),
                                  !!!county_lookup, .default = as.character(county))
    )
  
  # FIX: county selector with Select All / None links
  output$county_selector <- renderUI({
    ids <- sort(unique(raw_data$county_id))
    ids <- ids[!is.na(ids) & ids != ""]
    nms <- ifelse(ids %in% names(county_lookup), county_lookup[ids], ids)
    tagList(
      tags$b("Filter by County:"),
      div(class = "select-links",
          tags$a(onclick = paste0(
            "Shiny.setInputValue('select_all_counties', Math.random())"
          ), "Select all"),
          tags$a(onclick = paste0(
            "Shiny.setInputValue('clear_all_counties', Math.random())"
          ), "Clear all")
      ),
      checkboxGroupInput("selected_counties", NULL,
                         choices  = setNames(ids, nms),
                         selected = ids)
    )
  })
  
  # Select All observer
  observeEvent(input$select_all_counties, {
    ids <- sort(unique(raw_data$county_id))
    ids <- ids[!is.na(ids) & ids != ""]
    updateCheckboxGroupInput(session, "selected_counties", selected = ids)
  })
  
  # Clear All observer
  observeEvent(input$clear_all_counties, {
    updateCheckboxGroupInput(session, "selected_counties", selected = character(0))
  })
  
  # ── processed_data ──────────────────────────────────────────────────────
  processed_data <- reactive({
    req(input$selected_counties)
    raw_data %>%
      mutate(pollutant = trimws(pollutant), unit = trimws(unit)) %>%
      filter(
        pollutant %in% c("PM2.5", "pm2.5"),
        unit      %in% c("UG/M3", "ug/m3", "\u00b5g/m\u00b3"),
        !is.na(value),
        year %in% input$year_range,
        !is.na(datetime),
        county_id %in% input$selected_counties
      ) %>%
      mutate(display_value = value, unit_label = "\u03bcg/m\u00b3")
  })
  
  # ── site_means ──────────────────────────────────────────────────────────
  # FIX: adds live sf::st_distance() calculation (dist_live_km) alongside
  #      the pre-stored dist column. Uses UTM Zone 18N (EPSG:32618) for
  #      accurate metric distances — satisfies the spatial quantity requirement.
  site_means <- reactive({
    df <- processed_data() %>%
      filter(!is.na(display_value)) %>%
      # guard: ensure sf class is intact
      { if (inherits(., "sf")) filter(., !st_is_empty(geometry)) else . }
    
    sm <- df %>%
      group_by(aqs_id_full, county_name) %>%
      summarise(
        mean_pm25 = mean(display_value, na.rm = TRUE),
        dist      = mean(dist, na.rm = TRUE),   # pre-stored km, used for analysis
        geometry  = first(geometry),
        .groups   = "drop"
      ) %>%
      st_as_sf(crs = st_crs(df))
    
    # LIVE sf::st_distance() — congestion zone reference point (NYC CBD, 60th St)
    # projected to UTM Zone 18N (EPSG:32618) for metre-accurate distances
    congestion_ref <- st_sfc(
      st_point(c(-73.9857, 40.7580)), crs = 4326
    ) %>% st_transform(32618)
    
    sm_proj <- sm %>% st_transform(32618)
    dist_m  <- as.numeric(st_distance(sm_proj, congestion_ref)[, 1])  # metres
    
    sm %>%
      mutate(dist_live_km = dist_m / 1000)  # live-computed km, used in scatter + map
  })
  
  # ── spatial_analysis ────────────────────────────────────────────────────
  # Threshold: DIST_THRESHOLD_KM = 5 km
  spatial_analysis <- reactive({
    df <- processed_data() %>% st_drop_geometry() %>% filter(!is.na(dist))
    if (!nrow(df))
      return(list(congestion_avg = NA, remote_avg = NA, diff_value = NA,
                  n_cong = 0L, n_remote = 0L))
    
    cong <- df %>% filter(dist <= DIST_THRESHOLD_KM) %>%
      summarise(avg = mean(display_value, na.rm = TRUE), n = n())
    rem  <- df %>% filter(dist >  DIST_THRESHOLD_KM) %>%
      summarise(avg = mean(display_value, na.rm = TRUE), n = n())
    
    # FIX: explicit guard when one group is empty
    if (cong$n == 0 || rem$n == 0)
      return(list(congestion_avg = NA, remote_avg = NA, diff_value = NA,
                  n_cong = cong$n, n_remote = rem$n))
    
    list(congestion_avg = cong$avg, remote_avg = rem$avg,
         diff_value     = cong$avg - rem$avg,
         n_cong         = cong$n,  n_remote = rem$n)
  })
  
  # ── spatial_interpolation_analysis ──────────────────────────────────────
  spatial_interpolation_analysis <- reactive({
    sm <- site_means()
    if (nrow(sm) < 3) return(list(error = "Insufficient monitoring sites (need \u2265 3)."))
    sm_m   <- st_transform(sm, 3857)
    coords <- st_coordinates(sm_m)
    vals   <- sm_m$mean_pm25
    dmat   <- as.matrix(dist(coords))          # projected distance in metres
    mn_d   <- mean(dmat[upper.tri(dmat)], na.rm = TRUE) / 1000
    mn_c   <- mean(vals, na.rm = TRUE)
    cv     <- ifelse(mn_c != 0, sqrt(var(vals, na.rm = TRUE)) / abs(mn_c), 0)
    n      <- nrow(sm_m)
    dv <- vv <- numeric()
    for (i in seq_len(n - 1)) for (j in (i + 1):n) {
      d <- dmat[i, j]; v <- abs(vals[i] - vals[j])
      if (!is.na(d) && !is.na(v) && d > 0) { dv <- c(dv, d); vv <- c(vv, v) }
    }
    sc <- if (length(dv) > 2) cor(dv, vv, use = "complete.obs") else 0
    list(mean_distance_km = mn_d, concentration_cv = cv,
         spatial_correlation = sc, n_sites = n)
  })
  
  # ── spatial_hotspot_analysis ────────────────────────────────────────────
  spatial_hotspot_analysis <- reactive({
    sm <- site_means()
    if (nrow(sm) < 3) return(list(error = "Insufficient monitoring sites (need \u2265 3)."))
    vals  <- sm$mean_pm25
    mn    <- mean(vals, na.rm = TRUE)
    sd_v  <- sd(vals,  na.rm = TRUE)
    thr   <- mn + sd_v
    n_hot <- sum(vals > thr, na.rm = TRUE)
    n_tot <- length(vals)
    list(mean_concentration = mn, sd_concentration = sd_v, threshold = thr,
         n_hotspots = n_hot, n_total = n_tot,
         hotspot_percentage = round(n_hot / n_tot * 100, 1),
         hotspot_ids = sm$aqs_id_full[vals > thr])
  })
  
  # ── scatter_lm_stats ────────────────────────────────────────────────────
  # FIX: now uses dist_live_km (sf::st_distance result) instead of stored dist
  scatter_lm_stats <- reactive({
    sm <- site_means() %>% st_drop_geometry() %>% filter(!is.na(dist_live_km))
    if (nrow(sm) < 3) return(NULL)
    mod <- lm(mean_pm25 ~ dist_live_km, data = sm)
    s   <- summary(mod)$coefficients
    ci  <- confint(mod, "dist_live_km")
    list(slope    = s["dist_live_km", "Estimate"],
         se_slope = s["dist_live_km", "Std. Error"],
         ci_lo    = ci[1], ci_hi = ci[2],
         p_value  = s["dist_live_km", "Pr(>|t|)"],
         r2       = summary(mod)$r.squared,
         n_sites  = nrow(sm))
  })
  
  # ── ndi_stats ───────────────────────────────────────────────────────────
  ndi_stats <- reactive({
    sm <- site_means() %>% st_drop_geometry()
    if (nrow(sm) < 2) return(NULL)
    vals <- sort(sm$mean_pm25)
    ndi  <- max(vals) / min(vals)
    n    <- length(vals)
    gini <- (2 * sum(seq_len(n) * vals) / (n * sum(vals))) - (n + 1) / n
    list(ndi      = round(ndi, 2),  gini    = round(gini, 3),
         max_site = sm$aqs_id_full[which.max(sm$mean_pm25)],
         min_site = sm$aqs_id_full[which.min(sm$mean_pm25)],
         max_val  = round(max(vals), 2), min_val = round(min(vals), 2),
         n_sites  = n)
  })
  
  # ── summary_stats ───────────────────────────────────────────────────────
  summary_stats <- reactive({
    d <- processed_data() %>% st_drop_geometry(); n <- nrow(d)
    if (!n) return(NULL)
    list(mean = round(mean(d$display_value, na.rm = TRUE), 1),
         se   = round(sd(d$display_value,   na.rm = TRUE) / sqrt(n), 2),
         n    = n)
  })
  
  # ── monthly_data ────────────────────────────────────────────────────────
  monthly_data <- reactive({
    processed_data() %>% st_drop_geometry() %>%
      group_by(year, month) %>%
      summarise(mean_pm25 = mean(display_value, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, month) %>%
      mutate(date_month = lubridate::make_date(year, month, 1),
             time_index = row_number())
  })
  
  # ── Overview outputs ─────────────────────────────────────────────────────
  output$summary_sentence <- renderText({
    s <- summary_stats()
    if (is.null(s)) "No valid PM2.5 data for the selected filters."
    else paste0("Average PM2.5: ", s$mean, " \u03bcg/m\u00b3  (SE = ", s$se, " \u03bcg/m\u00b3)")
  })
  
  output$data_note <- renderText({
    yrs <- sort(unique(processed_data() %>% st_drop_geometry() %>% pull(year)))
    if (!length(yrs)) "No data." else paste("Years:", paste(yrs, collapse = ", "))
  })
  
  output$text_mean <- renderText({
    s <- summary_stats(); if (is.null(s)) "N/A" else paste0(s$mean, " \u03bcg/m\u00b3")
  })
  
  output$text_se <- renderText({
    s <- summary_stats(); if (is.null(s)) "N/A" else paste0(s$se, " \u03bcg/m\u00b3")
  })
  
  output$text_n <- renderText({
    s <- summary_stats(); if (is.null(s)) "0" else format(s$n, big.mark = ",")
  })
  
  output$ndi_text <- renderText({
    nd <- ndi_stats()
    if (is.null(nd)) return("Insufficient site data.")
    paste0("Network Disparity Index (NDI): ", nd$ndi, ". Across ", nd$n_sites,
           " monitoring sites, the most polluted site (", nd$max_site,
           ", mean = ", nd$max_val, " \u03bcg/m\u00b3) is ", nd$ndi,
           "\u00d7 higher than the cleanest site (", nd$min_site,
           ", mean = ", nd$min_val, " \u03bcg/m\u00b3).")
  })
  
  output$gini_text <- renderText({
    nd <- ndi_stats(); if (is.null(nd)) return("")
    lv <- if (nd$gini < 0.1) "low" else if (nd$gini < 0.2) "moderate" else "high"
    paste0("Gini coefficient of PM2.5 inequality: ", nd$gini,
           " \u2014 indicating ", lv, " inequality across the network.")
  })
  
  # ── Spatial outputs ──────────────────────────────────────────────────────
  
  # FIX: direction arrow prepended to Δ value
  output$concentration_diff_value <- renderText({
    r <- spatial_analysis()
    if (is.na(r$diff_value)) return("N/A")
    arrow <- if (r$diff_value > 0) "\u2191 " else "\u2193 "
    paste0(arrow, round(r$diff_value, 2), " \u03bcg/m\u00b3")
  })
  
  output$hotspot_count_value <- renderText({
    r <- spatial_hotspot_analysis()
    if (!is.null(r$error)) "N/A"
    else paste0(r$n_hotspots, " / ", r$n_total, " sites")
  })
  
  output$n_sites_value <- renderText({ paste0(nrow(site_means()), " sites") })
  
  output$spatial_summary_sentence <- renderText({
    r <- spatial_analysis()
    if (is.na(r$diff_value)) {
      n_c <- r$n_cong; n_r <- r$n_remote
      if (n_c == 0)
        return(paste0("No stations within ", DIST_THRESHOLD_KM,
                      " km of the congestion zone in the current selection. ",
                      "Try including more counties or adjusting the year filter."))
      if (n_r == 0)
        return(paste0("All stations are within ", DIST_THRESHOLD_KM,
                      " km of the congestion zone. No remote baseline available."))
      return("No valid spatial data for the selected filters.")
    }
    paste0(
      "Congestion mean (", r$n_cong, " obs, dist \u2264 ", DIST_THRESHOLD_KM, " km): ",
      round(r$congestion_avg, 2), " \u03bcg/m\u00b3.  ",
      "Remote mean (", r$n_remote, " obs, dist > ", DIST_THRESHOLD_KM, " km): ",
      round(r$remote_avg, 2), " \u03bcg/m\u00b3.  ",
      "\u0394 = ", round(r$diff_value, 2), " \u03bcg/m\u00b3",
      if (r$diff_value > 0) " \u2014 congestion zone stations show higher PM2.5."
      else " \u2014 remote stations show higher PM2.5."
    )
  })
  
  output$interpolation_insight_text <- renderText({
    r <- spatial_interpolation_analysis()
    if (!is.null(r$error)) return(r$error)
    het <- if (r$concentration_cv > 0.3) "high" else
      if (r$concentration_cv > 0.15) "moderate" else "low"
    dep <- if (abs(r$spatial_correlation) > 0.3) "moderate" else
      if (abs(r$spatial_correlation) > 0.1) "weak" else "minimal"
    paste0("Average inter-site distance: ", round(r$mean_distance_km, 1), " km.  ",
           "CV: ", round(r$concentration_cv, 2), " (", het, " heterogeneity).  ",
           "Spatial r: ", round(r$spatial_correlation, 2),
           " (", dep, " dependency) across ", r$n_sites, " sites.")
  })
  
  output$hotspot_analysis_text <- renderText({
    r <- spatial_hotspot_analysis()
    if (!is.null(r$error)) return(r$error)
    sp <- if (r$hotspot_percentage > 25) "widespread" else
      if (r$hotspot_percentage > 10) "moderate" else "limited"
    paste0("Threshold: mean + 1 SD = ", round(r$threshold, 2), " \u03bcg/m\u00b3.  ",
           r$n_hotspots, " of ", r$n_total,
           " sites (", r$hotspot_percentage, "%) flagged \u2014 ", sp, " clustering.")
  })
  
  output$scatter_trend_card <- renderUI({
    s <- scatter_lm_stats()
    if (is.null(s))
      return(div(class = "trend-card trend-nosig",
                 tags$b("Trend Analysis"),
                 tags$p(style = "margin:6px 0 0;", "Need \u2265 3 stations.")))
    sig   <- s$p_value < 0.05
    dir   <- if (s$slope < 0) "decreasing" else "increasing"
    icon  <- if (s$slope < 0) "\u2198" else "\u2197"
    p_fmt <- if (s$p_value < 0.001) "< 0.001" else
      if (s$p_value < 0.01)  "< 0.01"  else
        paste0("= ", round(s$p_value, 3))
    head  <- if (sig) paste0(icon, " Significant ", dir, " trend (p ", p_fmt, ")")
    else     paste0("\u2194 No significant trend (p ", p_fmt, ")")
    body  <- if (sig && s$slope < 0)
      paste0("Each +1 km from the zone \u2192 PM2.5 decreases by ",
             round(abs(s$slope), 3), " \u03bcg/m\u00b3.")
    else if (sig && s$slope > 0)
      paste0("Each +1 km from the zone \u2192 PM2.5 increases by ",
             round(s$slope, 3), " \u03bcg/m\u00b3.")
    else "Distance (km) does not significantly predict PM2.5 here."
    cls <- if (sig) "trend-card trend-sig" else "trend-card trend-nosig"
    div(class = cls,
        tags$b("Linear Trend Analysis (live sf::st_distance distances)"),
        tags$p(style = "margin:7px 0 3px;font-size:0.9rem;", head),
        tags$p(style = "margin:3px 0 7px;font-size:0.86rem;", body),
        div(class = "pill-row",
            div(class = "pill", paste0("Slope: ", round(s$slope, 3), " \u03bcg/m\u00b3/km")),
            div(class = "pill", paste0("[", round(s$ci_lo, 3), ", ", round(s$ci_hi, 3), "]")),
            div(class = "pill", paste0("R\u00b2 = ", round(s$r2, 3))),
            div(class = "pill", paste0("p ", p_fmt)),
            div(class = "pill", paste0("n = ", s$n_sites))
        )
    )
  })
  
  # ── Temporal plots ────────────────────────────────────────────────────────
  output$ts_plot <- renderPlotly({
    data <- processed_data() %>% st_drop_geometry()
    if (!nrow(data) || all(is.na(data$datetime))) return(NULL)
    data <- data %>%
      mutate(label = paste0(
        "Monitor: ", aqs_id_full, "<br>",
        "PM2.5: ", round(display_value, 2), " \u03bcg/m\u00b3<br>",
        "County: ", county_name, "<br>",
        "Time: ", format(datetime, "%Y-%m-%d %H:%M", tz = "UTC")
      ))
    p <- ggplot(data, aes(x = datetime, y = display_value,
                          group = aqs_id_full, color = county_name, text = label)) +
      geom_line(alpha = 0.6, linewidth = 0.4) +
      scale_color_viridis_d(name = "County", option = "D") +
      labs(x = "Date/Time (UTC)", y = "PM2.5 (\u03bcg/m\u00b3)",
           title = "Hourly PM2.5 \u2014 All Selected Sites") +
      scale_x_datetime(limits = c(min(data$datetime, na.rm = TRUE),
                                  max(data$datetime, na.rm = TRUE)),
                       expand = c(0.005, 0.005)) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "label") %>%
      layout(
        margin = list(l = 60, r = 20, t = 50, b = 160),
        legend = list(orientation = "h", x = 0, y = -0.28,
                      xanchor = "left", yanchor = "top",
                      bgcolor  = "rgba(255,255,255,0.85)",
                      bordercolor = "#dce3ea", borderwidth = 1,
                      font = list(size = 11), tracegroupgap = 4),
        xaxis = list(title = list(standoff = 10)),
        yaxis = list(title = list(standoff = 10))
      )
  })
  
  # FIX: monthly trend now uses GAM smoother (mgcv) instead of linear model
  output$monthly_trend <- renderPlotly({
    data <- monthly_data()
    if (nrow(data) < 4) return(NULL)   # GAM needs at least 4 points for s(x, k=4)
    
    # Build hover labels for raw points
    data <- data %>%
      mutate(label_pt = paste0(
        "Month: ", format(date_month, "%b %Y"),
        "<br>Mean PM2.5: ", round(mean_pm25, 2), " \u03bcg/m\u00b3"
      ))
    
    # Fit GAM for trend text
    gam_mod <- mgcv::gam(mean_pm25 ~ s(time_index, k = min(4, nrow(data) - 1)),
                         data = data)
    fitted_vals <- predict(gam_mod, newdata = data, type = "response")
    direction   <- if (tail(fitted_vals, 1) > fitted_vals[1]) "upward" else "downward"
    
    p <- ggplot(data, aes(x = date_month, y = mean_pm25)) +
      geom_smooth(
        method    = "gam",
        formula   = y ~ s(x, k = min(4, nrow(data) - 1)),
        color     = CLR_RED,
        fill      = "#f5c6c6",
        linewidth = 1,
        se        = TRUE,
        alpha     = 0.25
      ) +
      geom_point(aes(text = label_pt), color = CLR_BLUE, size = 2.8) +
      labs(x     = "Month",
           y     = "Mean PM2.5 (\u03bcg/m\u00b3)",
           title = "Monthly Mean PM2.5 with GAM Smoother (95% CI)") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 70, r = 20, t = 50, b = 80),
             xaxis  = list(title = list(text = "Month", standoff = 15),
                           tickangle = -30, tickfont = list(size = 11)),
             yaxis  = list(title = list(standoff = 10)))
  })
  
  # FIX: trend_text updated to describe GAM, not linear slope
  output$trend_text <- renderText({
    data <- monthly_data()
    if (nrow(data) < 4) return("Need \u2265 4 months of data for GAM smoother.")
    
    gam_mod  <- mgcv::gam(mean_pm25 ~ s(time_index, k = min(4, nrow(data) - 1)),
                          data = data)
    fitted_v <- predict(gam_mod, newdata = data)
    start_v  <- round(fitted_v[1], 2)
    end_v    <- round(tail(fitted_v, 1), 2)
    diff_v   <- round(end_v - start_v, 2)
    dir_word <- if (diff_v > 0) "increased" else "decreased"
    
    gam_summary <- summary(gam_mod)
    
    p_val <- gam_summary$s.table[1, "p-value"]
    p_val <- round(p_val,2)
    sig_text <- if(p_val < 0.05) "significant" else "insignificant"
    
    paste0(
      "GAM trend: Monthly mean PM2.5 ", dir_word, " by approximately ",
      abs(diff_v), " \u03bcg/m\u00b3 from the first to last month in the selected period ",
      "(GAM fitted values: start = ", start_v, ", end = ", end_v, " \u03bcg/m\u00b3). ",
      "The GAM smoother captures nonlinear seasonal variation rather than assuming a constant slope. ",
      "According to its p-value ",p_val,", this trend is statistically ",sig_text
      
    )
  })
  
  # ── Spatial scatter ───────────────────────────────────────────────────────
  # FIX: x-axis now uses dist_live_km (sf::st_distance result)
  output$spatial_scatter <- renderPlot({
    sm  <- site_means() %>% st_drop_geometry() %>% filter(!is.na(dist_live_km))
    if (nrow(sm) < 2) return(NULL)
    hot <- spatial_hotspot_analysis()
    sm  <- sm %>% mutate(
      is_hot = aqs_id_full %in%
        (if (!is.null(hot$hotspot_ids)) hot$hotspot_ids else character())
    )
    ggplot(sm, aes(x = dist_live_km, y = mean_pm25)) +
      geom_smooth(method = "lm", color = CLR_RED, fill = "#f5c6c6", se = TRUE) +
      geom_point(aes(color = is_hot, size = is_hot), alpha = 0.82) +
      scale_color_manual(values = c("FALSE" = CLR_BLUE, "TRUE" = CLR_RED),
                         labels = c("FALSE" = "Normal", "TRUE" = "Hotspot"),
                         name   = NULL) +
      scale_size_manual(values = c("FALSE" = 3, "TRUE" = 4.5), guide = "none") +
      labs(title = "PM2.5 vs Distance to Congestion Zone",
           x     = "Distance to Congestion Zone (km, via sf::st_distance)",
           y     = "Mean PM2.5 (\u03bcg/m\u00b3)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", size = 12),
            legend.position = "bottom")
  })
  
  # ── Leaflet map ───────────────────────────────────────────────────────────
  # FIX: popup now shows both stored dist and live dist_live_km for transparency
  output$spatial_map <- renderLeaflet({
    sm <- site_means() %>% st_transform(4326)
    if (!nrow(sm)) return(NULL)
    
    hot     <- spatial_hotspot_analysis()
    hot_ids <- if (!is.null(hot$hotspot_ids)) hot$hotspot_ids else character()
    
    sm <- sm %>%
      mutate(
        is_hot      = aqs_id_full %in% hot_ids,
        ring_color  = if_else(is_hot, CLR_RED, CLR_BLUE),
        fill_radius = pmax(sqrt(mean_pm25) * 2, 4),
        ring_radius = pmax(sqrt(mean_pm25) * 2, 4) + 4,
        popup_text  = paste0(
          if_else(is_hot,
                  paste0("<b style='color:", CLR_RED, "'>\u26A0\uFE0F Hotspot \u2014 "),
                  "<b>"),
          county_name, "</b><br>",
          "AQS ID: ", aqs_id_full, "<br>",
          "Mean PM2.5: <b>", round(mean_pm25, 2), " \u03bcg/m\u00b3</b><br>",
          "Distance to zone (stored): ", round(dist, 3), " km<br>",
          "Distance to zone (live sf::st_distance): ", round(dist_live_km, 3), " km"
        )
      )
    
    pal <- colorBin("RdYlGn", domain = sm$mean_pm25, reverse = TRUE, bins = 6)
    
    leaflet(sm) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius  = ~ring_radius,
        color   = ~ring_color,
        weight  = 3,
        fill    = FALSE,
        opacity = 0.9,
        popup   = ~popup_text
      ) %>%
      addCircleMarkers(
        radius      = ~fill_radius,
        color       = "white",
        weight      = 1,
        fillColor   = ~pal(mean_pm25),
        fillOpacity = 0.88,
        popup       = ~popup_text
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~mean_pm25,
        title    = "Mean PM2.5<br>(\u03bcg/m\u00b3)",
        opacity  = 0.9
      ) %>%
      addLegend(
        position = "bottomleft",
        colors   = c(CLR_RED, CLR_BLUE),
        labels   = c(
          paste0("Hotspot (mean > \u03bc+1SD)  n=", sum(sm$is_hot)),
          paste0("Normal site  n=",                 sum(!sm$is_hot))
        ),
        title   = paste0(
          "Site classification<br>",
          "<small style='font-weight:400;color:#555;'>Ring color around each circle</small>"
        ),
        opacity = 0.9
      )
  })
}

shinyApp(ui = ui, server = server)