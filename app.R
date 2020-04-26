library(flexdashboard)
library(tidyverse)
library(plotly)
library(rAmCharts)
library(shiny)
library(leaflet)
library(tmap)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
library(shinydashboard)

ncov <- read.csv(url("https://www.researchgate.net/profile/Alex_Mirugwe/publication/340105307_COVID-19_Cases/data/5e790e98a6fdcceef97303f3/COVID-19-Cases.csv"))
# ui code begins here

ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  dashboardPage(skin = "green",
                
                dashboardHeader(title = "Corona-Virus monitoring System", titleWidth = 400),
                
                dashboardSidebar(width = 100,
                                 sidebarMenu(
                                   menuItem("Analysis", tabName = "Analysis", icon = icon("dashboard")),
                                   menuItem("Map", tabName = "Map", icon = icon("dashboard"))
                                 )),
                dashboardBody(
                  ## reference the style CSS sheet
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom3.css")
                  ),
                  # create popover using shinyBS package - on mouse hover question icon
                  bsPopover(id="q1", title = "Confirmed", #currentConfirmedCount
                            content = "Total number of current confirmed people", 
                            trigger = "hover", 
                            placement = "top",
                            options = list(container = "body")),         
                  
                  
                  # create popover using shinyBS package - on hover the infobox
                  bsPopover(id="info2", title = "Recovered", 
                            content = "Total number of cured peoples", 
                            trigger = "hover", 
                            placement = "top",
                            options = list(container="body")),
                  
                  bsPopover(id="info3", title = "Deaths", 
                            content = "Total number of dead peoples", 
                            trigger = "hover", 
                            placement = "left",
                            options = list(container="body")),
                  
                  bsPopover(id="info4", title = "Active", 
                            content = "Active Patients", 
                            trigger = "hover", 
                            placement = "left",
                            options = list(container="body")),
                  
                  tabItems(
                    # First tab content
                    tabItem(tabName = "Analysis",
                            fluidRow(
                              infoBoxOutput("info1",width = 3), # first infoBox
                              infoBoxOutput("info2",width = 3), # second infoBox
                              infoBoxOutput("info3",width = 2), # third infoBox
                              infoBoxOutput("info4",width = 3)
                              
                              
                            ),
                            column(4,
                                   
                                   selectInput("country",
                                               "Country:",
                                               c("All",
                                                 unique(as.character(ncov$Country_Region))))),
                            column(4,
                                   
                                   selectInput("Case_Type",
                                               "Case_Type:",
                                               c("All",
                                                 unique(as.character(ncov$Case_Type))))),
                            
                            mainPanel(
                              tabsetPanel(
                                id = 'dataset',
                                tabPanel("COVID-19", DT::dataTableOutput("mytable1")),
                                DT::dataTableOutput("table"),
                                plotOutput("plot2")
                              )))))))              

# server code begins here
server <- function(input, output, session) {
  ncov2 <- ncov %>%
    dplyr::select(ï..Date,Country_Region,Case_Type) 
  ncov2 <- as_tibble(ncov2)
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(ncov2, options = list(orderClasses = TRUE))
    # Filter data based on selections
    data_filter <- ncov2
    if (input$country != "All") {
      data_filter <- data_filter[data_filter$Country_Region == input$country,]
    }
    #DT::renderDataTable(ncov2, selection = 'none', editable = TRUE)
    
    else (input$Case_Type != "All") {
      data_filter <- data_filter[data_filter$Case_Type == input$Case_Type,]
    }
    data_filter
    #DT::renderDataTable(ncov2, selection = 'none', editable = TRUE)
  })
  
  output$plot2<-renderPlot({
    ncov5 <- ncov %>%
      group_by(Country.Region,Long,Lat) %>%
      summarise(Confirmed = sum(Confirmed))
    p <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey",  color = "white") +
      geom_point( data=ncov5, aes(x=Long, y=Lat ,  color=Country.Region)) +
      coord_fixed(1.3) +
      theme_void() +
      theme_void()  +
      guides(fill=FALSE) + theme(legend.position = "none")
    p
  })
  # infoBox for mean value of diamonds price
  output$info1 <- renderInfoBox({
    Confirmed <-  ncov2 %>%
      select(Case_Type) %>%
      filter(Case_Type == "Confirmed") 
    
    infoBox("Confirmed", count(Confirmed), 
            
            # used subtitle argument to create icon to be used to trigger the popover
            subtitle = tags$a(icon("question-circle"), id="q1"))
  })
  
  
  # infoBox for median value of diamonds price
  output$info3 <- renderInfoBox({
    Death <-  ncov2 %>%
      select(Case_Type) %>%
      filter(Case_Type == "Deaths") 
    
    infoBox("Deaths", count(Death))
    
  })
  output$info2 <- renderInfoBox({
    Recovered <-  ncov2 %>%
      select(Case_Type) %>%
      filter(Case_Type == "Recovered") 
    
    infoBox("Recovered", count(Recovered))
  })
  output$info4 <- renderInfoBox({
    Active <-  ncov2 %>%
      select(Case_Type) %>%
      filter(Case_Type == "Active") 
    
    infoBox("Active", count(Active))
  })
  
  
}

shinyApp(ui, server)