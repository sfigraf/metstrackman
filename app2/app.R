library(tidyverse)
library(plotly)
library(ggExtra)
library(dendextend)
library(randomForest)
library(shiny)

theme_set(theme_classic())

trackman <- read_csv("20190924-ChaseField-1.csv")

source("code2.r")

##TO DO: 
#new tab explaining what's going on, how the metrics are used to group a pitcher together
#labeling the dnedrogram

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Clusters"),
   
   # Sidebar 
   
     column(6, wellPanel(style='overflow: hidden',
                         h3("Select Pitcher Data"),
                         
                         uiOutput("PitcherControl"),
                         
                         uiOutput("PitchesControl"), 
                         
                         uiOutput("ClusterControl"),
                         
                         uiOutput("yearControl"),
                         
                         HTML('<hr >'))
      ),
   column(width = 6, 
          wellPanel(style='overflow: hidden',
                      h3("Select Metric Data"),
                      
                      uiOutput("xaxisControl"),
                      
                      uiOutput("yaxisControl"),
                      
                      HTML('<hr >'))
      ),
      
      # Show a plot of the generated distribution
   mainPanel(
     tabsetPanel(
       tabPanel(h4("HClusters"),plotOutput("plot1"),textOutput("text1")),
       tabPanel(h4("Metrics"),plotOutput("plot2"))
     )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Pitchers <-unique(trackman$pitcher)
  metrics <- colnames(trackman.subset)
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
  
  ####### X axis Metric control
  output$xaxisControl<-renderUI({
    selectizeInput(inputId="xaxismetric",label="X Axis:", choices=metrics, 
                   
                   options = list(placeholder='Choose a Metric', onInitialize = I('function() { this.setValue(""); }')) #makes it blank at startup
    )
  })
  
  ####### Y Axis Metric control
  
  output$yaxisControl<-renderUI({
    selectizeInput(inputId="yaxismetric",label="Y Axis:", choices=metrics, 
                   
                   options = list(placeholder='Choose a Metric', onInitialize = I('function() { this.setValue(""); }')) #makes it blank at startup
    )
  })
  
#####Data Housekeeping

  
  #####Cluster Data
    trackman.scaled <- reactive({ data.frame(
      scale(trackman.subset %>%
                               filter(pitch_type_auto == input$Pitch) %>%
                               ungroup() %>%
                               select(Spin.Rate,
                                      Horz.Break,
                                      Vert.Break,
                                      Extension,
                                      Spin.Axis,
                                      Velocity))
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
  ###pitch filtered data for similar pitchers function and text output 
    pitchfiltered <- reactive({data.frame(
      
      trackman.subset <- trackman.subset %>%
        filter(pitch_type_auto == input$Pitch) %>%
        ungroup() %>%
        mutate(cluster = as.factor(cluster())) #so that there's not a graident for a color pallete in plot2
    )
    })
  
    ###Metrics graph data; user can choose which metrics to display
    trackman.metrics <- reactive({data.frame(
      pitchfiltered()[,c(input$xaxismetric, input$yaxismetric,"pitcher","pitch_type_auto","cluster")]
      
    )})

########PLOTS
    
###plot 1    
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
   
   
   
   
   output$text1 <- renderText({
     validate(
       need(input$PitcherID !="", message=FALSE),
       need(input$Pitch !="", message=FALSE)
     )
     
     #paste("Pitchers with a profile similar to", as.character(input$PitcherID)) #, "based off", as.character(colnames(trackman.scaled)))
     similarpitchersfunction(pitchfiltered(),input$PitcherID)
     
     
     })
   
   output$plot2 <- renderPlot({
     validate(
       need(input$xaxismetric !="", message="Choose a X Axis Metric"),
       
       need(input$yaxismetric !="", message="Choose a Y Axis Metric"),
       need(input$Pitch !="", message="Choose a Pitch")
       
     )
     
     #Warning: Error in : `data` must be a data frame, or other object coercible by `fortify()`, not an S3 object with class uneval
     #Did you accidentally pass `aes()` to the `data` argument?
     #fixed by putting trackman.metrics() in front
     trackman.metrics() %>%
       ggplot(aes(x = trackman.metrics()[[1]],
                  y = trackman.metrics()[[2]])) +
       geom_jitter(aes(color = cluster, size = 3)) + #use geom_jitter when you don't want overlap between the points
       geom_text(aes(label = pitcher)) +
       labs(title=paste0(input$Pitch, " Metrics"),
            x =paste0(input$xaxismetric), y = paste0(input$yaxismetric))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

