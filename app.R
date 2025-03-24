
library(shiny)
library(soilDB)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Soil Properties Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Enter Location or Select Polygon"),
      
      radioButtons("selection_type", "Select AOI Type:",
                   choices = c("Single Point" = "point", "Bounding Box (Polygon)" = "polygon"),
                   selected = "point"),
      
      conditionalPanel(
        condition = "input.selection_type == 'point'",
        numericInput("longitude", "Longitude:", value = -96.668, min = -180, max = 180, step = 0.0001),
        numericInput("latitude", "Latitude:", value = 44.304, min = -90, max = 90, step = 0.0001)
      ),
      
      conditionalPanel(
        condition = "input.selection_type == 'polygon'",
        numericInput("min_lat", "Min Latitude:", value = 44.0, min = -90, max = 90, step = 0.0001),
        numericInput("max_lat", "Max Latitude:", value = 44.6, min = -90, max = 90, step = 0.0001),
        numericInput("min_lon", "Min Longitude:", value = -97.0, min = -180, max = 180, step = 0.0001),
        numericInput("max_lon", "Max Longitude:", value = -96.0, min = -180, max = 180, step = 0.0001)
      ),
      
      checkboxGroupInput("selected_props", "Select Soil Properties:", 
                         choices = c("Clay (%)" = "claytotal_r",
                                     "Sand (%)" = "sandtotal_r",
                                     "Silt (%)" = "silttotal_r",
                                     "Organic Matter (%)" = "om_r",
                                     "Available Water Content" = "awc_r",
                                     "pH" = "ph1to1h2o_r",
                                     "Bulk Density" = "dbthirdbar_r",
                                     "Electrical Conductivity (EC)" = "ec_r",
                                     "Elevation" = "elev_r",
                                     "Saturated Hydraulic Conductivity" = "ksat_r",
                                     "Water Content at -15 bar" = "wtenthbar_r",
                                     "Water Content at -1/3 bar" = "wthirdbar_r",
                                     "Depth to Water Table" = "wtdepannmin"),
                         selected = c("claytotal_r", "sandtotal_r", "silttotal_r")),
      
      actionButton("submit", "Submit"),
      downloadButton("downloadData", "Download CSV")
    ),
    
    mainPanel(
      h3("Soil Data Table"),
      tableOutput("soil_table"),
      
      h4(textOutput("error_message"), style = "color: red;")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  soil_data_reactive <- reactiveVal(NULL)  # Reactive variable to store soil data
  error_message_reactive <- reactiveVal("")  # Store error messages
  
  # Map selected properties to more user-friendly names
  property_map <- list(
    "claytotal_r" = "Clay (%)",
    "sandtotal_r" = "Sand (%)",
    "silttotal_r" = "Silt (%)",
    "om_r" = "Organic Matter (%)",
    "awc_r" = "Available Water Content (cm)",
    "ph1to1h2o_r" = "pH",
    "dbthirdbar_r" = "Bulk Density (g/cc)",
    "ec_r" = "Electrical Conductivity (dS/m)",
    "elev_r" = "Elevation (masl)",
    "ksat_r" = "Saturated Hydraulic Conductivity (um/s)",
    "wtenthbar_r" = "Water Content at -15 bar (%)",
    "wthirdbar_r" = "Water Content at -1/3 bar (%)",
    "wtdepannmin" = "Depth to Water Table (cm)"
  )
  
  observeEvent(input$submit, {
    
    error_message_reactive("")  # Clear previous errors
    
    # Determine AOI type and create appropriate geometry
    aoi <- NULL
    if (input$selection_type == "point") {
      # Define AOI as a single point
      aoi <- st_as_sf(st_sfc(st_point(c(input$longitude, input$latitude))), crs = 4326)
    } else if (input$selection_type == "polygon") {
      # Define AOI as a bounding box (polygon)
      min_lon <- input$min_lon
      max_lon <- input$max_lon
      min_lat <- input$min_lat
      max_lat <- input$max_lat
      
      # Create a polygon using the min/max coordinates
      polygon_coords <- matrix(c(
        min_lon, min_lat,  # Bottom-left corner
        max_lon, min_lat,  # Bottom-right corner
        max_lon, max_lat,  # Top-right corner
        min_lon, max_lat,  # Top-left corner
        min_lon, min_lat   # Closing the polygon
      ), ncol = 2, byrow = TRUE)
      
      # Create the polygon geometry
      aoi <- st_as_sf(st_sfc(st_polygon(list(polygon_coords))), crs = 4326)
    }
    
    if (is.null(aoi)) {
      error_message_reactive("Please select a valid AOI.")
      return()
    }
    
    # Query soil data using SDA_spatialQuery for the selected AOI
    soil_data <- tryCatch(
      SDA_spatialQuery(aoi, what = "mukey"),
      error = function(e) {
        error_message_reactive("Failed to retrieve mukey from SDA.")
        return(NULL)
      }
    )
    
    if (is.null(soil_data) || nrow(soil_data) == 0) {
      error_message_reactive("No soil data found for the given location.")
      return()
    }
    
    # Get mukey for the point
    mukeys <- paste(soil_data$mukey, collapse = ",")
    
    # Construct SQL query dynamically based on selected properties
    selected_columns <- paste(input$selected_props, collapse = ", ")
    query <- paste0("
  SELECT mu.mukey, c.compname, ch.hzname, 
         ch.claytotal_r, ch.sandtotal_r, ch.silttotal_r, 
         ch.om_r, ch.awc_r, ch.ph1to1h2o_r,ch.dbthirdbar_r,ch.ec_r,c.elev_r,ch.ksat_r,ch.wtenthbar_r,ch.wthirdbar_r,
         muagg.wtdepannmin
  FROM mapunit AS mu
  INNER JOIN component AS c ON mu.mukey = c.mukey
  INNER JOIN chorizon AS ch ON c.cokey = ch.cokey
  INNER JOIN muaggatt AS muagg ON mu.mukey = muagg.mukey
  WHERE mu.mukey IN (", mukeys, ")
")
    
    # Run the query
    soil_properties <- tryCatch(
      SDA_query(query),
      error = function(e) {
        error_message_reactive("Error querying soil properties. Check selected attributes.")
        return(NULL)
      }
    )
    
    if (is.null(soil_properties) || nrow(soil_properties) == 0) {
      error_message_reactive("No soil properties found for the selected attributes.")
      return()
    }
    
    # Ensure only selected properties and standard columns are included
    selected_properties <- c("mukey", "compname", "hzname", input$selected_props)
    soil_properties <- soil_properties[, selected_properties, drop = FALSE]
    
    # Rename columns to user-friendly names
    colnames(soil_properties) <- c("Mukey", "Component Name", "Horizon Name", 
                                   unname(sapply(input$selected_props, function(x) property_map[[x]])))
    
    # Store data in reactive variable
    soil_data_reactive(soil_properties)
    
    # Display soil properties as a table
    output$soil_table <- renderTable({
      soil_properties
    })
    
  })
  
  # Display error messages
  output$error_message <- renderText({
    error_message_reactive()
  })
  
  # Download handler for CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Soil_Properties_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      soil_data <- soil_data_reactive()
      if (!is.null(soil_data) && nrow(soil_data) > 0) {
        write.csv(soil_data, file, row.names = FALSE)
      } else {
        error_message_reactive("No data available to download.")
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
