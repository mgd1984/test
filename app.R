#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries 
source("load_libraries.R")

# Fetch data
source("get_data.R")

# Define Shiny App UI

header1 <- dashboardHeader(title = "Cell Tower Heatmap")

sidebar1 <-
    dashboardSidebar(sidebarMenu(
        menuItem(
            text = "Select Carrier(s):",
            icon = icon("th"),
            startExpanded = F,
            multiInput(
                "carrier",
                "Carrier:",
                choices = c(
                    "Bell Mobility Inc.",
                    "Rogers Communications Canada Inc.",
                    "TELUS Communications Inc."
                ),
                selected = "Rogers Communications Canada Inc.",
                #unique(data$LICENSEE),
            )
        ),
        # pickerInput(
        #     "carrier",
        #     "Carrier",
        #     choices = unique(data$LICENSEE),
        #     multiple = TRUE
        # ),
        menuItem(
            text = "Heatmap Options:",
            icon = icon("dashboard"),
            startExpanded = F,
            selectInput(
                "colour",
                "Heatmap Colour:",
                choices = c("viridis","plasma", "inferno","magma","cividis"),
                multiple = T
            ),
            numericRangeInput("frequency",
                              "Frequency:",
                              value = range(data$TRANSMIT_FREQ)
            ),
            sliderInput(
                "opacity",
                "Opacity:",
                min = 0,
                max = 1,
                value = 0.75,
                step = 0.05
            ),
            sliderInput(
                "radius",
                "Radius:",
                min = 0,
                max = 20,
                value = 2
            ),
            sliderInput(
                "blur",
                "Blur:",
                min = 0,
                max = 20,
                value = 2,
                step = 1
            ),
            sliderInput(
                "maxvalue",
                "MaxValue:",
                min = 0,
                max = 10,
                value = 1,
                step = 1
            ),
            sliderInput(
                "cellSize",
                "cellSize:",
                min = 1,
                max = 10,
                value = 1,
                step = 1
            )
        ) #menuInput
    )) #sidebarMenu) #dashboardSidebar

body1 <- dashboardBody(tabsetPanel(
    type = "tabs",
    tabPanel(
        title = "Map",
        width = "100%",
        solidHeader = TRUE,
        status = "primary",
        leafletOutput("baseMap"),
        tags$style(type = "text/css", "#baseMap {height: calc(100vh - 80px) !important;}"),
        tags$head(
            tags$script(src = "http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")
        ),
        tags$head(
            tags$style("#carrier {color: red; font-size: 20px;font-style: italic;}")
        ),
    )
))

ui <- dashboardPage(header1, sidebar1, body1)

# Define SERVER logic
server <- function(input, output, session) {
    filteredData <- reactive({
        data[data$LICENSEE %in% input$carrier[1],]
    })
    
    filteredData2 <- reactive({
        data[data$LICENSEE %in% input$carrier[2],]
    })
    
    filteredData3 <- reactive({
        data[data$LICENSEE %in% input$carrier[3],]
    })
    
    output$baseMap <- renderLeaflet({
        leaflet(data = data) %>%
            # fitBounds(
            #   ~ min(as.numeric(LONGITUDE)),
            #   ~ min(as.numeric(LATITUDE)),
            #   ~ max(as.numeric(LONGITUDE)),
            #   ~ max(as.numeric(LATITUDE))
            # ) %>%
            setView(lat = 53.5064502,lng = -89.4373341,4.25) %>% 
            enableTileCaching() %>% 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark",tileOptions(useCache=TRUE, crossOrigin=T)) %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite",tileOptions(useCache=TRUE, crossOrigin=T)) %>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB",tileOptions(useCache=TRUE, crossOrigin=T)) %>%
            addProviderTiles(providers$OpenStreetMap, group = "OSM",tileOptions(useCache=TRUE, crossOrigin=T)) %>%
            addFullscreenControl(pseudoFullscreen = F) %>%
            addLayersControl(baseGroups = c("Dark", "Satellite", "OSM", "CartoDB")) %>% 
            addResetMapButton() %>%
            leaflet.extras::addControlGPS(options = gpsOptions(setView = T,maxZoom = 15, autoCenter = T,position = "topright"))
        # leaflet.extras::gpsOptions(activate=TRUE,autoCenter = TRUE, setView = TRUE)
    })
    
    
    observe({
        # color_scheme <- viridis::cividis(n_distinct(data$LICENSEE %in% c("Bell Mobility Inc.", "Bell Canada")))
        # pal = colorFactor(color_scheme, data$LICENSEE)
        leafletProxy("baseMap", data = filteredData()) %>%
            clearHeatmap() %>%
            addHeatmap(
                lng = ~ LONGITUDE,
                lat = ~ LATITUDE,
                minOpacity = ~ input$opacity,
                blur = ~ input$blur,
                max = ~ input$maxvalue,
                radius = ~ sqrt(input$radius),
                gradient = input$colour[1],
                #input$colour, #viridis(20,1,0,1,1,option="B"),
                intensity = NULL,
                cellSize = ~ input$cellSize
            )
    })
    
    observe({
        # color_scheme <- viridis::cividis(n_distinct(data$LICENSEE %in% c("Bell Mobility Inc.", "Bell Canada")))
        # pal = colorFactor(color_scheme, data$LICENSEE)
        leafletProxy("baseMap", data = filteredData2()) %>%
            addHeatmap(
                lng = ~ LONGITUDE,
                lat = ~ LATITUDE,
                minOpacity = ~ input$opacity,
                blur = ~ input$blur,
                max = ~ input$maxvalue,
                radius = ~ sqrt(input$radius),
                gradient = input$colour[2],
                #input$colour, #viridis(20,1,0,1,1,option="B"),
                intensity = NULL,
                cellSize = ~ input$cellSize
            )
    })
    
    observe({
        # color_scheme <- viridis::cividis(n_distinct(data$LICENSEE %in% c("Bell Mobility Inc.", "Bell Canada")))
        # pal = colorFactor(color_scheme, data$LICENSEE)
        leafletProxy("baseMap", data = filteredData3()) %>%
            addHeatmap(
                lng = ~ LONGITUDE,
                lat = ~ LATITUDE,
                minOpacity = ~ input$opacity,
                blur = ~ input$blur,
                max = ~ input$maxvalue,
                radius = ~ sqrt(input$radius),
                gradient = input$colour[3],
                #input$colour, #viridis(20,1,0,1,1,option="B"),
                intensity = NULL,
                cellSize = ~ input$cellSize
            )
    })
    
} #server

# Run app
shinyApp(ui, server)




