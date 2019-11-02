library(tidyverse)
library(plotly)
library(ggExtra)
library(shinydashboard)
library(shiny)

theme_set(theme_classic())

trackman <- read_csv("20190924-ChaseField-1.csv")
trackman$spin_rate <- as.numeric(trackman$spin_rate) #change column to numeric
#ranks$pitcher <- factor(ranks$pitcher, levels = ranks$pitcher)
##comparisons data
ranks <- trackman %>%
  group_by(pitcher,pitch_type_auto) %>%
  summarize(avg_vertbreak = mean(vert_break),
            avg_horbreak = mean(horz_break),
            avg_velo = mean(zone_speed),
            avg_spin = mean(spin_rate))



#defining the kzone for later graphic

topKzone <- 3.5  #average value for top of the zone
botKzone <- 1.6  #average value for bottom of zone
inKzone <- -.95  
outKzone <- 0.95 
kZone <- data.frame(x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
                    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)) 

ui <- dashboardPage(
   
   # Application title
  dashboardHeader(title = "Trackman Data",
                  titleWidth = 300),
   
   # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    sidebarMenu(
    menuItem("Charts", tabName = "charts", icon = icon("dashboard"),
             menuSubItem("Strike Zone", tabName = "subitem1"),
             menuSubItem("Pitch Velocities", tabName = "subitem2")
             ),
    menuItem("Comparisons", tabName = "rankings", icon = icon("chart"),
             menuSubItem("Velocities", tabName = "subitem3"),
             menuSubItem("Spin Rate", tabName = "subitem4")
    ))),
  dashboardBody(
    selectInput("select1",
                              label = tags$h4(tags$b("Select a Pitcher")),
                              choices = sort(unique(trackman$pitcher))),
    selectInput("select2",
                label = tags$h4(tags$b("Select a Pitch")),
                choices = sort(unique(ranks$pitch_type_auto))),
    tabItems(
    tabItem("subitem1",
            box(
              plotlyOutput("plot1"),
              width = 12
            )
    ),
    tabItem("subitem2",
            box(
              plotlyOutput("plot2"),
              width = 12)),
    tabItem("subitem3",
            box(
              plotlyOutput("plot3"),
              width = 12)),
    tabItem("subitem4",
            box(
              plotlyOutput("plot4"),
              width = 12))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  pitches <- reactive({
    trackman %>%
      filter(pitcher == input$select1) 
      # mutate(exitvelobins = seq(min(exit_speed), max(exit_speed), length.out = 4)
             
    
    }) 
  
   output$plot1 <- renderPlotly({
     pitches() %>%
       ggplot(aes(x = plate_loc_x, y = plate_loc_z)) +
       geom_path(aes(x, y), data = kZone, lwd = 2, col = "black", alpha = .4) +
       geom_point(aes(color = pitch_type_auto, shape = pitch_call, size = 2)) +
       facet_wrap(~ batside) + 
       coord_equal() +
       # theme(panel.background = element_blank()) +
       ggtitle(paste(as.character(input$select1)," Pitches"))
   })
   
   output$plot2 <- renderPlotly({
     pitches() %>%
       ggplot(aes(x = pitchid, y = zone_speed, label = date)) +
       facet_wrap(~pitch_type_auto) +
       geom_point(aes(shape = pitch_call, size = 2)) +
       geom_smooth(col = "black", alpha = .5) +
       ggtitle(paste(as.character(input$select1),"Pitch Speeds")) 
       # theme(plot.subtitle = paste0("Game Date: ",as.character(date)))
   })
   
   
   
   ##by input pitch

   velo1 <- reactive({
     ranks <- ranks %>%
     filter(pitch_type_auto == input$select2)
     #velo
     ranks$avg_velo_z <- round((ranks$avg_velo - mean(ranks$avg_velo))/sd(ranks$avg_velo), 2)  # compute normalized avg_velo
     ranks$avg_velo_type <- ifelse(ranks$avg_velo_z < 0, "below", "above")  # above / below avg flag
     ranks <- ranks[order(ranks$avg_velo_z), ] #must have order, otherwise you get a "data must be dataframe or other object oercible by fortify
     
     #spin
     ranks$avg_spin_z <- round((ranks$avg_spin - mean(ranks$avg_spin))/sd(ranks$avg_spin), 2)  # compute normalized avg_spin
     ranks$avg_spin_type <- ifelse(ranks$avg_spin_z < 0, "below", "above")
     ranks <- ranks[order(ranks$avg_spin_z), ] #must have order, otherwise you get a "data must be dataframe or other object oercible by fortify
     
     #ranks$pitcher <- factor(ranks$pitcher, levels = ranks$pitcher)
     
     })
   
   
   #velo1$pitcher <- factor(velo1$pitcher, levels = velo1$pitcher)
   
      # sort
   
    
   ##Velo comparison plot
   output$plot3 <- renderPlotly({
     velo1() %>%
       #arrange(avg_velo_z) %>%
       ggplot(aes(x=pitcher, y=avg_velo_z, label=avg_velo)) + 
       geom_point(stat='identity', aes(col=avg_velo_type),size=4) +
       scale_color_manual(name="Velo",
                          labels = c("Above Average", "Below Average"),
                          values = c("above"="#00ba38", "below"="#f8766d")) +
       geom_text(color="black", size=1.5)  +
       labs(title=paste("Normalized Pitcher", as.character(input$select2), "Velos"),
            subtitle= paste("Normalized", as.character(input$select2), "Velos from trackman data")) +
       ylim(-2.5, 2.5) +
       coord_flip() +
       theme_classic()
   })
   
   #spin plot
   # spin1 <- reactive({
   #   ranks <- ranks %>%
   #     filter(pitch_type_auto == input$select2)
   #   ranks$avg_spin_z <- round((ranks$avg_spin - mean(ranks$avg_spin))/sd(ranks$avg_spin), 2)  # compute normalized avg_spin
   #   ranks$avg_spin_type <- ifelse(ranks$avg_spin_z < 0, "below", "above")  # above / below avg flag
   #   ranks <- ranks[order(ranks$avg_spin_z), ]
   #   #ranks$pitcher <- factor(ranks$pitcher, levels = ranks$pitcher)
   #   
   # })
   
   ## spin
   output$plot4 <- renderPlotly({
     velo1() %>%
       ggplot(aes(x=pitcher, y=avg_spin_z, label=avg_spin)) +
       geom_point(stat='identity', aes(col=avg_spin_type),size=4) +
       scale_color_manual(name="spin",
                          labels = c("Above Average", "Below Average"),
                          values = c("above"="#00ba38", "below"="#f8766d")) +
       geom_text(color="black", size=1.5)  +
       labs(title=paste("Normalized Pitcher", as.character(input$select2), "spin rate"),
            subtitle= paste("Normalized", as.character(input$select2), "spin rate from trackman data")) +
       ylim(-2.5, 2.5) +
       coord_flip()
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

