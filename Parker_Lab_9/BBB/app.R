#Logan Parker

library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel( img(src="BBBLogo.png", height = "25%",
                    width = "25%")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("rb1","Choose a Central BBQ Location",c("Oxford"="oxford",
                                                                 "Inlet Beach"="inlet_beach",
                                                                 "Charleston"="charleston",
                                                                 "Nashville"="nashville",
                                                                 "Birmingham"="birmingham",
                                                                 "Homewood"="homewood",
                                                                 "Florence"="florence"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("BBBMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    cityData <- reactiveValues()
    output$BBBMap <- renderLeaflet({
        leaflet(cityData,options = leafletOptions(zoomSnap=0.1)) %>%
            addTiles()
    })
    
    observe({
        cityData$name <- c("Oxford","Inlet Beach","Charleston","Nashville","Birmingham","Homewood","Florence")
        cityData$address <- c("719 N Lamar","10711 E Hwy 30A","456 Meeting St","5304 Charlotte Ave","5361 US-280","1926 29th Avenue S","315 N Court St")
        cityData$state <- c("MS","Fl","SC","TN","AL","AL","AL")
        cityData$zip <- c("38655","32461","29403","37209","35242","35209","35630")
        cityData$phone <- c("662.236.2666 ", "850.532.6952","843.459.1800","615.610.3403","205.490.7568","205.666.7099","256.415.8545")
        cityData$long <- c("-89.516107","-86.010989","-79.939229","-86.851889","-86.674059","-86.788199","-87.677750")
        cityData$lat <- c("34.375572","30.2802131","32.795024","36.152353","33.420744","33.479773","34.802386")
        cityLabel <- sprintf("<b>%s Location</b>%s<br />%s, %s %s<br />%s",
                                cityData$name,cityData$address,cityData$name,cityData$state,cityData$zip,cityData$phone)%>%
            lapply(htmltools::HTML)
        if(input$rb1 == "oxford"){
            cityData$viewLong <- "-89.516107"
            cityData$viewLat <- "34.375572"
        }else if(input$rb1 == "inlet_beach"){
            cityData$viewLong <- "-86.010989"
            cityData$viewLat <- "30.2802131"
        }else if(input$rb1 == "charleston"){
            cityData$viewLong <- "-79.939229"
            cityData$viewLat <- "32.795024"
        }else if(input$rb1 == "nashville"){
            cityData$viewLong <- "-86.851889"
            cityData$viewLat <- "36.152353"
        }else if(input$rb1 == "birmingham"){
            cityData$viewLong <- "-86.674059"
            cityData$viewLat <- "33.420744"
        }else if(input$rb1 == "homewood"){
            cityData$viewLong <- "-86.788199"
            cityData$viewLat <- "33.479773"
        }else if(input$rb1 == "florence"){
            cityData$viewLong <- "-87.677750"
            cityData$viewLat <- "34.802386"
        }
        proxy <- leafletProxy("BBBMap") %>% clearMarkers()
        
        proxy %>% setView(lng=as.numeric(cityData$viewLong), lat=as.numeric(cityData$viewLat), zoom = 15) %>%
            addMarkers(lng = as.numeric(cityData$long), lat=as.numeric(cityData$lat), popup = cityLabel, label = cityLabel)
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)