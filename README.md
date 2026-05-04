# NYC Air Quality Dashboard

**Authors:** Luobing Wang & Chenchong Xia | **Course:** SYSEN 5460 — Cornell University

An interactive Shiny dashboard exploring hourly PM2.5 air quality data across the NYC metropolitan area (NY + NJ counties), drawing from EPA AQS and NYC monitoring networks.

---

## Repository Structure

```
my-shiny-app/
├── app.R                        # Main Shiny application (UI + Server)
├── App.Rproj                    # RStudio project file
├── manifest.json                # ShinyApps.io deployment manifest
└── merged_air_quality_sf.rds    # Pre-processed spatial dataset (see below)
```

---

## How to Run

**Install dependencies:**

```r
install.packages(c(
  "shiny", "dplyr", "readr", "ggplot2", "plotly",
  "DT", "broom", "lubridate", "bslib", "bsicons",
  "sf", "leaflet", "mgcv"
))
```

**Run the app:**

1. Open `App.Rproj` in RStudio
2. Open `app.R` → click **Run App**

> `merged_air_quality_sf.rds` must remain in the same directory as `app.R`. Initial load may take 3–8 seconds (~40 MB).

---

## Dashboard Pages

| Page | Key Features |
|---|---|
| **Overview** | Mean PM2.5, SE, observation count; Network Disparity Index (NDI); Gini coefficient across monitoring sites |
| **Temporal** | Hourly PM2.5 time series (plotly, colored by county); Monthly trend with GAM smoother (95% CI) |
| **Spatial** | Congestion vs. remote ΔPM2.5; hotspot identification (mean + 1 SD); scatter regression using live `sf::st_distance()`; interactive Leaflet map |

**Sidebar controls:** Year(s) (2018, 2019, 2024, 2025) · County filter (16 NY/NJ counties) · Select all / Clear all

---

## Data: `merged_air_quality_sf.rds`

Built by inner-joining `air_quality.csv` and `sites.rds` on `aqs_id_full`. Only rows with matching site geometry are retained. Source datasets are documented in the [air_quality data README](../../docs).

| Column | Type | Description |
|---|---|---|
| `aqs_id_full` | double | Full AQS site ID (U.S. country code prefix `840`) |
| `datetime` | POSIXct | Observation timestamp (UTC) |
| `value` | double | PM2.5 concentration (µg/m³) |
| `unit` | character | Always `"UG/M3"` after filtering |
| `county` | character | 5-digit FIPS county code |
| `year`, `month`, `hour` | integer | Time components |
| `site_name` | character | Short EPA monitor name |
| `geometry` | POINT [°] | Monitor location — WGS84, from `sites.rds` |
| `dist` | double | Pre-stored distance to congestion zone boundary (km), from `sites.rds` |

---

## Key Technical Details

**Live spatial calculation (`sf::st_distance()`)** — Inside `site_means()`, each monitor's distance to the NYC CBD reference point (60th St, `−73.9857, 40.7580`) is recomputed in UTM Zone 18N (EPSG:32618) for metric accuracy. This `dist_live_km` column drives the spatial scatter plot and regression, independently of the pre-stored `dist` column.

**GAM smoother** — The monthly trend uses `mgcv::gam()` with a thin-plate spline `s(x, k=4)` to capture the nonlinear seasonal pattern in PM2.5, replacing the earlier linear model.

---

## Known Limitations

- Data available for **2018, 2019, 2024, 2025 only** — gaps in 2020–2023 cause visual discontinuities in the time series
- Congestion zone reference is approximated as a single point; `sites.rds` uses the actual zone boundary polygon for the pre-stored `dist`
- Spatial interpolation metrics (CV, Pearson r) are descriptive — no formal geostatistical model is applied
