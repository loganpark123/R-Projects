library(shiny)
library(leaflet)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Central BBQ"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("rb1","Choose a Central BBQ Location",c("Downtown"="dt","Poplar"="pp"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("CentralMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    CentralData <- reactiveValues()
    output$CentralMap <- renderLeaflet({
        leaflet(CentralData,options = leafletOptions(zoomSnap=0.1)) %>%
            addTiles()
            #addMarkers(~Long, ~Lat, popup = centralLabel, label = centralLabel)
    })
    
    observe({
        CentralData$name <- c("Downtown Location","Poplar Location")
        CentralData$address <- c("147 E Butler Ave 38102","6201 Poplar Ave 38119")
        CentralData$phone <- c("901.672.7760", "901.417.7962")
        CentralData$long <- c("-90.057134","-89.856992")
        CentralData$lat <- c("35.134117","35.101341")
        centralLabel <- sprintf("<b>%s</b><br />%s<br />%s",
                                CentralData$name,CentralData$address,CentralData$phone)%>%
            lapply(htmltools::HTML)
        if(input$rb1 == "dt"){
            CentralData$viewLong <- "-90.057134"
            CentralData$viewLat <- "35.134117"
        }else{
            CentralData$viewLong <- "-89.856992"
            CentralData$viewLat <- "35.101341"
        }  
        proxy <- leafletProxy("CentralMap") %>% clearMarkers()
        
        proxy %>% setView(lng=as.numeric(CentralData$viewLong), lat=as.numeric(CentralData$viewLat), zoom = 15) %>%
            addMarkers(lng = as.numeric(CentralData$long), lat=as.numeric(CentralData$lat), popup = centralLabel, label = centralLabel)
        
        
    })
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
