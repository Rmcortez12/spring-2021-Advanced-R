library(plotly)
library(ggplot2)
#library(tidygeocoder)

library(leaflet)
library(leaflet.extras)
library(magrittr)
library(mapview)


#leaflet heatmap
#https://bhaskarvk.github.io/leaflet.extras/reference/heatmap.html

#https://www.realtor.com/research/data/
#Create and save data sets
#core.county.history <- read.csv("Core_Metrics_County_History.csv")
#core.state.history <- read.csv("Core_Metrics_State_History.csv")
#hot.county.history <- read.csv("Hotness_Metrics_County_History.csv")
#hot.metro.history <- read.csv("Hotness_Metrics_Metro_History.csv")
#save(core.county.history,core.state.history,hot.county.history,hot.metro.history, file = "RealEstate.Rdata")


#core.state.history$date <- as.Date(paste0(as.character(core.state.history$month_date_yyyymm),'01'),format = '%Y%m%d')
#core.county.history$date <- as.Date(paste0(as.character(core.county.history$month_date_yyyymm),'01'),format = '%Y%m%d')

#get.state.f <- function(x){
#  a <- strsplit(as.character(x),',')
#  b <- stringr::str_trim(a[[1]][2])
#  county <- a[[1]][1]
#  state <- b
#  return(c(county,state))
#  }

#core.county.history$state <- unlist(lapply(toupper(core.county.history$county_name), function(x) get.state.f(x)[2]))
#core.county.history$county <- unlist(lapply(toupper(core.county.history$county_name), function(x) get.state.f(x)[1]))


#lat.long <- geo(county = x,state = y,country = country,method='osm')
#need to save this so we don't have to run again.
#save(lat.long, file = "latlong.Rdata")
#test <- tidyr::drop_na(lat.long)
#load("latlong.Rdata")
#lat.long$price <- core.county.history$median_listing_price/1000
#lat.long$date <- core.county.history$date
#lat.long$year <- core.county.history$year
#lat.long <- tidyr::drop_na(lat.long)
#save(lat.long, file = "latlong.Rdata")

#lat.long2021 <- lat.long[lat.long$year>2020,]
# lat_center <- c(lat.long2021$lat) %>% as.numeric() %>% mean
# long_center <- c(lat.long2021$long) %>% as.numeric() %>% mean
# viz_map <- lat.long2021 %>%
#   leaflet() %>% 
#   addTiles() %>% 
#   addProviderTiles(providers$OpenStreetMap.DE) %>% 
#   setView(long_center,lat_center,6) %>%
#   addHeatmap(lng=~long,lat=~lat,intensity=~price,max=max(lat.long2021$price),radius=10,blur=5)
# 
# viz_map

#this map is taking all the years, we need to add years. 


#core.state.history$year <- format(core.state.history$date,"%Y")
#core.county.history$year <- format(core.county.history$date,"%Y")




#load(url("https://github.com/Rmcortez12/spring-2021-Advanced-R/blob/master/Project2/RealEstate.Rdata?raw=true"))
#load(url("https://github.com/Rmcortez12/spring-2021-Advanced-R/blob/master/Project2/latlong.Rdata?raw=true"))
load("RealEstate.Rdata")
load("latlong.Rdata")

Years <- unique(core.state.history$year)

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(

  #theme
  theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel("Median Household Prices"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

      hr(), #Horizontal Line for visual separation
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max of Cohort Values
      selectInput(inputId = "min", 
                  label = "Choose Starting Year (Min):", 
                  choices = Years,
                  selected= 2016),
      uiOutput("year_sel"),

     
    selectInput(inputId = "state",
                label = "State:",
                choices = sort(toupper(unique(core.county.history$state))),
                selected="TX"),
    hr(),
    uiOutput("county_sel")
    
  ),
  
  
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Median Home Price NationWide",plotlyOutput(outputId = "nation"),leafletOutput(outputId = "map")),
        tabPanel("Median Home Price By State",plotlyOutput(outputId = "sp1"), plotlyOutput(outputId = "statebox"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  get.label <- function(x){
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    
    return(list(
      title = x,
      titlefont = f
    ))
    
  }
  
  
  
  output$nation <- renderPlotly({
    a <- c(core.state.history$year >= input$min & core.state.history$year <= input$max)
    df <- core.state.history[a,]
    x <- df$date
    y <- df$median_listing_price
    fig <- plot_ly(x = ~x, y = ~y,name = df$state, type = "scatter", mode = "lines")
    fig <- fig %>% layout(xaxis = get.label("Date"), yaxis = get.label("Median Listing Price"))
    fig
    
  })
  
  output$sp1 <- renderPlotly({
    #filter by year
    a <- c(core.county.history$year >= input$min & core.county.history$year <= input$max)
    df <- core.county.history[a,]
    #filter by state
    a <- c(df$state==input$state)
    df <- df[a,]
    
    if(!("All" %in% input$county)){
      df <- df[df$county_name %in% input$county,]
    }
    fig <- plot_ly(x = ~df$date, y = ~df$median_listing_price,name = df$county_name, type = "scatter", mode = "lines")
    fig <- fig %>% layout(xaxis = get.label("Date"), yaxis = get.label("Median Listing Price"))
    fig
    
  })
  
  output$statebox <- renderPlotly({
    a <- c(core.state.history$year >= input$min & core.state.history$year <= input$max)
    df <- core.state.history[a,]
    a <- c(toupper(df$state_id)==input$state)
    x <- df$year[a]
    y <- df$median_listing_price[a]
    n <- df$state[a]
    fig <- plot_ly(x= ~x ,y = ~y,name = n, type = "box")
    fig <- fig %>% layout(xaxis = get.label("Year"), yaxis = get.label("Median Listing Price"))
    
    fig
  })
  
  output$year_sel <- renderUI({
    selectInput(inputId = "max", 
                label = "Choose End Year (Max):", 
                choices = Years[Years>=input$min],
                selected= 2021)
  })
  
  output$county_sel <- renderUI({
    selectizeInput(inputId = "county", 
                label = "Choose County:", 
                choices = c("All",sort(as.character(core.county.history$county_name[core.county.history$state == input$state]))),
                selected= "All",
                multiple = T)
  })

  output$map <- renderLeaflet({
    boola <- c(lat.long$year >= input$min & lat.long$year <= input$max)
    try(df <- lat.long[boola,])
    lat_center <- c(df$lat) %>% as.numeric() %>% mean
    long_center <- c(df$long) %>% as.numeric() %>% mean
    viz_map <- df %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>%
      setView(long_center,lat_center,6) %>%
      addHeatmap(lng=~long,lat=~lat,intensity=~price,max=max(df$price),radius=10,blur=5)
    viz_map


  })
}

# Run the application 
shinyApp(ui = ui, server = server)
