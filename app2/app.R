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
                         
                         uiOutput("ClusterControl"),
                         
                         uiOutput("yearControl"),
                         
                         HTML('<hr >'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),
         textOutput("text1")
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
  
  ####### Cluster control
  
  output$ClusterControl<-renderUI({
    validate(
      need(input$Pitch != "", message=FALSE) #if a pitch isn't selected, this won't show
    )
    sliderInput(inputId = "Cluster", label="Number of Clusters:", min = 1, max = 10, value = 3)

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
    
    cluster <- reactive({ cutree(hc1(),
                                 k = input$Cluster)
    })
    
   output$plot1 <- renderPlot({
     validate(
       need(input$PitcherID !="", message="Choose a Pitcher"),
       need(input$Pitch !="", message="Choose a Pitch")
     
     )
     
     #Warning: Error in as.double: cannot coerce type 'closure' to vector of type 'double'
     #solution: put hc1 as reactive so hc1()
     #now error: object pitch type auto not found. \
     #solution: idn't need to have trackman.subset reactive, so I was trying to groupby the inputs instead of just pitcher name and pitch type
     plot(hc1())
     rect.hclust(hc1(),
                 k = input$Cluster,
                 border = 1:3)
      
   })
   
   pitchfiltered <- reactive({data.frame(
     trackman.subset %>%
       filter(pitch_type_auto == input$Pitch) %>%
       ungroup() %>%
       mutate(cluster = cluster()) 
   )
  })
   #might need text output here
   # similarpitchers <- reactive({
   #   similarpitchersfunction(pitchfiltered(),input$PitcherID)
   #   })
   
   output$text1 <- renderText({
     validate(
       need(input$PitcherID !="", message=FALSE),
       need(input$Pitch !="", message=FALSE)
       
     )
     
     
     similarpitchersfunction(pitchfiltered(),input$PitcherID)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

