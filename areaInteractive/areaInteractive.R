library(shiny)
library(dplyr)
library(ggplot2)
library(pluralize)
library(ggthemes)

artworks <- read.csv("../artwork_data.csv") %>% 
  filter(units == "mm") %>% 
  mutate(width.size = as.numeric(as.character(width)),
         height.size = as.numeric(as.character(height)),
         year.year = as.numeric(as.character(year)),
         acquisitionYear.year = as.numeric(as.character(acquisitionYear)),
         area = height.size * width.size
        # ratio = height.size / width.size,
        # portrait = ratio > 1
  )

areaOverTime <- artworks %>% filter(!is.na(acquisitionYear.year)) %>% 
  filter(!is.na(area)) %>% 
  arrange(acquisitionYear.year) %>% 
  group_by(acquisitionYear.year) %>% 
  dplyr::summarise(n=n(), sumArea=sum(area) ) %>% 
  mutate(sumN = cumsum(n), cumsumArea =cumsum(sumArea))

areaChoices = c(
  "Meters Square"=1000000,
  "Football Pitch"=7140000000,
  "Turbine Hall"=12460000000
  )

choices <- names(areaChoices)
ui <- fluidPage(theme="bootstrap.css",
  headerPanel("How much space?!"),
  mainPanel(width=12,
  fluidRow( p(paste0("Over the years the Tate has acquired more than ", nrow(artworks), " artworks. If they were all being displayed, how much space would you need? \n How does that compare to the year you were born?"))),
  fluidRow( p("#MakeOverMonday 2017/06/12 ", a(href="https://github.com/tategallery/collection", "https://github.com/tategallery/collection"))),
  fluidRow(
    
    inputPanel(
      sliderInput("year", "Acquisition Year:", 
                  min=min(areaOverTime$acquisitionYear.year), max=max(areaOverTime$acquisitionYear.year), value=2017, sep=""),
    
      selectInput("area", label = "Area:",
                  choices = choices, selected = "Football Pitch")
    ),
    p("Why not try the year you were born?")
  ),
  fluidRow(
    verbatimTextOutput("desc")
  ),
  fluidRow(
           plotOutput("plot1", height = 300 )
  )
  )
)
  
  server <- function(input, output) {
    output$desc <- renderText({
      yearArea <- areaOverTime %>% filter(acquisitionYear.year == input$year) %>% select(cumsumArea)
      numNeeded <- ceiling(yearArea/areaChoices[input$area])
      paste0(ceiling(yearArea/1000000), " Meters squared would be needed to display the Tate Artworks in ", input$year, ".\nThat is equivalent to ", numNeeded, " ", pluralize(input$area, numNeeded))
    })
    
    output$plot1 <- renderPlot({
      #input <- NULL
      #input$year <- 1901
      yearArea <- areaOverTime %>% filter(acquisitionYear.year == input$year) %>% select(cumsumArea)
      areaChoice <- areaChoices[input$area]
      numNeeded <- ceiling(yearArea/areaChoice)
      
      title <- if_else(areaChoice == 1, "Area needed to display the Tate Artworks", paste0("How many ", pluralize(input$area, numNeeded), " are needed to display the Tate Artworks"))
      ggplot(areaOverTime, aes(x=acquisitionYear.year, y=cumsumArea/areaChoice)) + 
        geom_line() + ggtitle(title) + 
        geom_point(aes(x=input$year, y= numNeeded), color="orange", size=5) + 
        xlab("Acquisition Year") + ylab("Number") + theme_solarized()
      
    })
  }
  
  shinyApp(ui, server)
