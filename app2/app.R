library(tidyverse)
library(plotly)
library(ggExtra)
library(dendextend)
library(randomForest)
library(shiny)

theme_set(theme_classic())

trackman <- read_csv("20190924-ChaseField-1.csv")

source("code2.r")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Clusters"),
   
   # Sidebar 
     column(3, wellPanel(style='overflow: hidden',
                         h3("Select Pitcher Data"),
                         
                         uiOutput("PitcherControl"),
                         
                         uiOutput("PitchesControl"), 
                         
                         uiOutput("ParameterControl"),
                         
                         uiOutput("yearControl"),
                         
                         HTML('<hr >'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1")
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Pitchers <-unique(trackman$pitcher)
  #Pitches <-unique(trackman$pitch_type_auto)
  
  ##############Make UI controls###############################
  
  ####### Pitcher control
  output$PitcherControl<-renderUI({
    selectizeInput(inputId="PitcherID",label="Pitcher:", choices=Pitchers, 
                   options = list(placeholder='Choose a Pitcher', onInitialize = I('function() { this.setValue(""); }')) #makes it blank at startup
    )
  })
  ####### Pitches control
  
  output$PitchesControl<-renderUI({
    validate(
      need(input$PitcherID != "", message=FALSE) #if a pitcher isn't selected, this won't show
    )
    selectizeInput(inputId = "Pitch", label="Pitch:", choices=c(trackman %>% filter(pitcher==input$PitcherID))$pitch_type_auto,
                   options = list(placeholder='Choose a Pitch' , onInitialize = I('function() { this.setValue(""); }'))
    )
  })
  
#####Data Housekeeping
  
  trackman.subset <- data.frame(
    ##this is potentially the porblem here
    trackman %>% 
      group_by(pitcher,pitch_type_auto) %>%
      summarize(spin_rate = mean(spin_rate),
                horz_break = mean(horz_break),
                vert_break = mean(vert_break),
                tilt = mean(tilt),
                extension = mean(extension),
                rel_speed = mean(rel_speed),
                spin_axis = mean(spin_axis))
    )
  
    trackman.scaled <- reactive({ data.frame(
      scale(trackman.subset %>%
                               filter(pitch_type_auto == input$Pitch) %>%
                               ungroup() %>%
                               select(spin_rate,
                                      horz_break,
                                      vert_break,
                                      extension,
                                      rel_speed,
                                      spin_axis))
    )})
    #Error in .getReactiveEnvironment()$currentContext() : 
    #Operation not allowed without an active reactive context
    #solution: make this reactive
    distance.matrix <- reactive({
      dist(trackman.scaled())
    })

    hc1 <- reactive({
      hclust(distance.matrix())
    })
    
   output$plot1 <- renderPlot({
     validate(
       need(input$PitcherID !="", message="Choose a Pitcher"),
       need(input$Pitch !="", message="Choose a Pitch")
     
     )
     
     #Warning: Error in as.double: cannot coerce type 'closure' to vector of type 'double'
     #solution: put hc1 as reactive so hc1()
     #now error: object pitch type auto not found
     plot(hc1())
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

