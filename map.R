library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

script_file <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))][1])
script_dir <- if (is.na(script_file)) getwd() else dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))

setwd(script_dir)

output_file <- "index.html"
output_libdir <- "map_files"
model_boundary_path <- "C:/Users/csiemdata/Downloads/Boundary_Model.shp"
mangrove_path <- "S:/SEAF-MA/V0.2/GIS/dhiem-gis/DATA/DLI/Mangroves/RAW_20260414/OneDrive_1_14-04-2026/MAP_MangroveHabitat_EcOz_20210903.shp"
mangrove_cache_file <- "mangroves_model_boundary_simplified_15m.rds"
mangrove_simplify_tolerance_m <- 15

# --- Load data ---
study_area <- st_read("S:/SEAF-MA/V0.2/SATELLITE/virtual_sensor/DH_polygon/DH_polyons.shp") |>
  st_transform(4326)

model_boundary <- st_read(model_boundary_path) |>
  st_transform(4326)

point <- read.csv("S:/SEAF-MA/V0.2/SATELLITE/virtual_sensor/monitoring_sites.csv")

# Convert to sf (NOTE: your columns are lon, lat)
point_sf <- st_as_sf(point, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
sf::sf_use_s2(FALSE)

# --- Virtual sensor polygon attributes for map display ---
study_area$polygon_type <- ifelse(
  grepl("^Outer", study_area$Label, ignore.case = TRUE),
  "Outer extraction polygon",
  ifelse(
    grepl("^Inner", study_area$Label, ignore.case = TRUE),
    "Inner extraction polygon",
    "Extraction polygon"
  )
)

study_area$area_km2 <- round(as.numeric(st_area(st_transform(study_area, 3857))) / 1000000, 2)

# --- Crop mangroves to the model boundary ---
if (file.exists(mangrove_cache_file)) {
  mangroves <- readRDS(mangrove_cache_file)
} else {
  mangrove_fields <- c("COMMUNITY", "VEG_FORM", "ASSEMBLAGE")
  mangroves_raw <- st_read(mangrove_path)
  mangrove_boundary <- model_boundary |>
    st_transform(st_crs(mangroves_raw)) |>
    st_geometry() |>
    st_union()
  mangrove_hits <- lengths(st_intersects(mangroves_raw, mangrove_boundary)) > 0

  mangroves <- st_make_valid(mangroves_raw[mangrove_hits, mangrove_fields]) |>
    st_intersection(mangrove_boundary) |>
    st_simplify(dTolerance = mangrove_simplify_tolerance_m, preserveTopology = TRUE) |>
    st_transform(4326)

  saveRDS(mangroves, mangrove_cache_file)
}

# --- Model boundary extent ---
model_bbox <- st_bbox(model_boundary)
model_bounds <- as.numeric(model_bbox[c("xmin", "ymin", "xmax", "ymax")])

# --- Polygon label points ---
# Calculate label positions in a projected CRS, then transform back to WGS84
# for Leaflet. This avoids longitude/latitude centroid warnings.
centers <- study_area |>
  st_transform(3857) |>
  st_geometry() |>
  st_point_on_surface() |>
  st_as_sf() |>
  st_transform(4326)

coords <- st_coordinates(centers)
centers$lng <- coords[,1]
centers$lat <- coords[,2]

centers$label_name <- study_area$Label  

map_summary <- list(
  polygon_count = nrow(study_area),
  monitoring_site_count = nrow(point_sf),
  mangrove_count = nrow(mangroves)
)

map_title <- tags$div(
  style = paste(
    "background: rgba(255, 255, 255, 0.92);",
    "padding: 10px 12px;",
    "border-radius: 4px;",
    "box-shadow: 0 1px 5px rgba(0,0,0,0.35);",
    "font-family: Arial, sans-serif;",
    "line-height: 1.25;"
  ),
  tags$div(
    style = "font-size: 18px; font-weight: 700; color: #1f2933;",
    "Virtual Sensor in Darwin Harbour"
  ),
  tags$div(
    style = "font-size: 12px; color: #52606d; margin-top: 3px;",
    "Satellite extraction zones, monitoring sites, mangrove habitat, and model boundary"
  )
)

map_purpose <- tags$div(
  style = paste(
    "background: rgba(255, 255, 255, 0.92);",
    "padding: 10px 12px;",
    "border-radius: 4px;",
    "box-shadow: 0 1px 5px rgba(0,0,0,0.35);",
    "font-family: Arial, sans-serif;",
    "font-size: 12px;",
    "line-height: 1.45;",
    "max-width: 270px;"
  ),
  tags$details(
    open = NA,
    tags$summary(style = "font-weight: 700; cursor: pointer;", "Purpose and Summary"),
    tags$div(style = "margin-top: 6px;", "Virtual sensor polygons and monitoring sites used to extract satellite observations and compare them with model outputs."),
    tags$div(
      style = "margin-top: 7px;",
      tags$b("Summary:"),
      tags$br(),
      paste0(map_summary$polygon_count, " virtual sensor polygons"),
      tags$br(),
      paste0(map_summary$monitoring_site_count, " monitoring sites"),
      tags$br(),
      paste0(map_summary$mangrove_count, " cropped mangrove features")
    )
  )
)

map_legend <- tags$div(
  style = paste(
    "background: rgba(255, 255, 255, 0.92);",
    "padding: 10px 12px;",
    "border-radius: 4px;",
    "box-shadow: 0 1px 5px rgba(0,0,0,0.35);",
    "font-family: Arial, sans-serif;",
    "font-size: 12px;",
    "line-height: 1.45;"
  ),
  tags$details(
    open = NA,
    tags$summary(style = "font-weight: 700; cursor: pointer;", "Legend"),
    tags$div(
      style = "margin-top: 6px;",
      tags$span(style = "display:inline-block; width:18px; height:12px; background:transparent; border:2px solid blue; margin-right:7px; vertical-align:middle;"),
      "Virtual sensor polygons"
    ),
    tags$div(
      tags$span(style = "display:inline-block; width:18px; height:10px; background:rgba(46, 125, 50, 0.35); border:1px solid #2e7d32; margin-right:7px; vertical-align:middle;"),
      "Mangrove habitat"
    ),
    tags$div(
      tags$span(style = "display:inline-block; width:18px; height:12px; background:transparent; border:2px dashed black; margin-right:7px; vertical-align:middle;"),
      "Model boundary"
    ),
    tags$div(
      tags$img(
        src = file.path(output_libdir, "leaflet-1.3.1/images/marker-icon.png"),
        style = "width:13px; height:21px; margin:0 10px 0 3px; vertical-align:middle;"
      ),
      "Monitoring site"
    )
  )
)

site_marker_icon <- makeIcon(
  iconUrl = file.path(output_libdir, "leaflet-1.3.1/images/marker-icon.png"),
  iconRetinaUrl = file.path(output_libdir, "leaflet-1.3.1/images/marker-icon-2x.png"),
  shadowUrl = file.path(output_libdir, "leaflet-1.3.1/images/marker-shadow.png"),
  iconWidth = 25,
  iconHeight = 41,
  iconAnchorX = 12,
  iconAnchorY = 41,
  popupAnchorX = 1,
  popupAnchorY = -34,
  shadowWidth = 41,
  shadowHeight = 41
)

# --- Create map ---
map <- leaflet() %>%
  
  # Base maps
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "National Geographic") %>%
  addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Matter") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  fitBounds(
    lng1 = model_bounds[1],
    lat1 = model_bounds[2],
    lng2 = model_bounds[3],
    lat2 = model_bounds[4]
  ) %>%

  # --- Mangrove habitat ---
  addPolygons(
    data = mangroves,
    fillColor = "#2e7d32",
    fillOpacity = 0.3,
    color = "#1b5e20",
    weight = 0.6,
    opacity = 0.75,
    group = "Mangrove Habitat",
    label = ~COMMUNITY,
    labelOptions = labelOptions(
      direction = "auto",
      sticky = TRUE,
      style = list(
        "font-size" = "11px",
        "font-weight" = "bold",
        "color" = "#1b5e20"
      )
    ),
    popup = ~paste0(
      "<b>Mangrove habitat</b>",
      "<br><b>Community:</b> ", COMMUNITY,
      "<br><b>Vegetation form:</b> ", VEG_FORM,
      "<br><b>Assemblage:</b> ", ASSEMBLAGE
    )
  ) %>%
  
  # --- Polygons ---
  addPolygons(
    data = study_area,
    fillOpacity = 0,
    color = "blue",
    weight = 1,
    group = "Virtual Sensor Polygons",
    label = ~Label,
    labelOptions = labelOptions(
      direction = "auto",
      sticky = TRUE,
      style = list(
        "font-size" = "12px",
        "font-weight" = "bold",
        "color" = "#1f2933"
      )
    ),
    popup = ~paste0(
      "<b>Virtual sensor polygon:</b> ", Label,
      "<br><b>Type:</b> ", polygon_type,
      "<br><b>Area:</b> ", area_km2, " km²",
      "<br><b>Purpose:</b> Satellite data extraction zone",
      "<br><b>Use:</b> Model validation and comparison"
    )
  ) %>%

  # --- Model boundary ---
  addPolygons(
    data = model_boundary,
    fill = FALSE,
    fillOpacity = 0,
    color = "black",
    weight = 3,
    dashArray = "6,6",
    group = "Model Boundary",
    label = "Model Boundary",
    labelOptions = labelOptions(
      direction = "auto",
      sticky = TRUE,
      style = list(
        "font-size" = "12px",
        "font-weight" = "bold",
        "color" = "black"
      )
    ),
    options = pathOptions(interactive = FALSE)
  ) %>%
  
  # --- Monitoring site markers ---
  addMarkers(
    data = point_sf,
    group = "Monitoring Sites",
    icon = site_marker_icon,
    label = ~site_name,
    labelOptions = labelOptions(
      noHide = FALSE,
      direction = "auto",
      sticky = TRUE,
      style = list(
        "font-size" = "12px",
        "font-weight" = "bold",
        "color" = "red"
      )
    ),
    popup = ~paste0(
      "<b>Site:</b> ", site_name,
      "<br><b>Lat:</b> ", lat,
      "<br><b>Lon:</b> ", lon,
      "<br><b>Purpose:</b> Monitoring / validation location",
      "<br><b>Use:</b> Compare field observations, satellite data, and model outputs"
    )
  ) %>%
  
  # --- Polygon labels ---
  addLabelOnlyMarkers(
    data = centers,
    lng = ~lng,
    lat = ~lat,
    label = ~label_name,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'top',
      textOnly = TRUE,
      style = list(
        "font-size" = "11px",
        "font-weight" = "bold",
        "color" = "blue",
        "text-shadow" = "1px 1px 2px white"
      )
    ),
    group = "Labels"
  ) %>%
  
  # --- Layer control ---
  addLayersControl(
    position = "topright",
    baseGroups = c("National Geographic", "Ocean Basemap", "Dark Matter", "OpenStreetMap", "World Imagery"),
    overlayGroups = c("Virtual Sensor Polygons", "Mangrove Habitat", "Model Boundary", "Monitoring Sites", "Labels"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # --- Map tools and presentation controls ---
  addControl(html = map_title, position = "topleft") %>%
  addControl(html = map_purpose, position = "topleft") %>%
  addControl(html = map_legend, position = "bottomright") %>%
  addScaleBar(position = "bottomleft") %>%
  addFullscreenControl(position = "topleft") %>%
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "kilometers",
    secondaryLengthUnit = "meters",
    primaryAreaUnit = "sqkilometers",
    secondaryAreaUnit = "hectares"
  ) %>%
  addMiniMap(
    tiles = providers$OpenStreetMap,
    toggleDisplay = TRUE,
    minimized = FALSE,
    position = "bottomleft"
  ) %>%
  addSearchFeatures(
    targetGroups = "Monitoring Sites",
    options = searchFeaturesOptions(
      zoom = 13,
      openPopup = TRUE,
      firstTipSubmit = TRUE,
      autoCollapse = TRUE
    )
  ) %>%
  
  # --- Zoom-based label visibility ---
  onRender("
    function(el, x) {
      var map = this;
      var labelLayer = map.layerManager.getLayerGroup('Labels');

      function updateLabels() {
        var zoom = map.getZoom();

        if (!labelLayer) {
          return;
        }

        labelLayer.eachLayer(function(layer) {
          if (zoom > 8) {
            layer.setOpacity(1);
          } else {
            layer.setOpacity(0);
          }
        });
      }

      updateLabels();
      map.on('zoomend', updateLabels);

      var coordinates = L.control({position: 'bottomleft'});
      coordinates.onAdd = function() {
        var div = L.DomUtil.create('div', 'leaflet-control leaflet-bar');
        div.style.background = 'rgba(255,255,255,0.92)';
        div.style.padding = '4px 7px';
        div.style.font = '12px Arial, sans-serif';
        div.style.boxShadow = '0 1px 5px rgba(0,0,0,0.35)';
        div.innerHTML = 'Lat, Lon';
        return div;
      };
      coordinates.addTo(map);

      map.on('mousemove', function(e) {
        coordinates.getContainer().innerHTML =
          'Lat: ' + e.latlng.lat.toFixed(5) + ' | Lon: ' + e.latlng.lng.toFixed(5);
      });
    }
  ")

# --- Show map ---
map

# --- Save ---
if (dir.exists(output_libdir)) {
  unlink(output_libdir, recursive = TRUE, force = TRUE)
}

saveWidget(map, output_file, selfcontained = FALSE, libdir = output_libdir)
