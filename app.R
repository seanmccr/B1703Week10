# ----- B1703 Week 10 | Creating Shiny Dashboards | 20.03.2024 -----
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
# ------ 2. Adding in Title and Two Filters -----
source('global.R', local = T)

# Define UI for application

ui<- dashboardPage(
  
  # HEADER
  dashboardHeader(title="World Cup 2018"), #here we create our title
  
  # Sidebar
  dashboardSidebar(
    width = 300, 
    selectInput(inputId= "select_stage", # We will tell it which variable to read in 
                label = "Select stage", # We will tell R how to name our filter 
                choices= Match_list$Stage, 
                selected = "Final", 
                multiple= FALSE ), 
    selectInput( inputId= "select_match", 
                 label = "Select Match", 
                 choices= Match_list$Match,
                 selected = "Final", multiple= FALSE )),
  
  #BODY
  dashboardBody( #Boxes need to be put in a row or column, we will add all our graphs and tables here 
    fluidRow( h1(textOutput('title'), align="center"), 
              h2(textOutput('Result'), align="center")),
    fluidRow(column(width=12, # I assign column width here to make sure this row and the next are aligned) 
                    valueBoxOutput('ShotsH', width=2), 
                    valueBoxOutput('ShotsTargetH', width=2), 
                    valueBoxOutput('ShotsAccuracyH', width=2), 
                    valueBoxOutput('ShotsA', width=2), 
                    valueBoxOutput('ShotsTargetA', width=2), 
                    valueBoxOutput('ShotsAccuracyA', width=2))),
    fluidRow(column(width = 12, 
                    valueBoxOutput('PassesH', width=2), 
                    valueBoxOutput('KPassesH', width=2), 
                    valueBoxOutput('RegainsH', width=2), 
                    valueBoxOutput('PassesA', width=2), 
                    valueBoxOutput('KPassesA', width=2), 
                    valueBoxOutput('RegainsA', width=2))),
    fluidRow(box(title= "Starting 11", 
                 width= 4, 
                 dataTableOutput("Lineup_home")), 
             box(title= "Shot time line", 
                 width = 4, 
                 plotlyOutput("TimeLinePlot"), 
                 align="center"), 
             box(title= "Starting 11", 
                 width=4, 
                 dataTableOutput("Lineup_away"))),
    fluidRow(tabBox(width=12, 
                    tabPanel("Shots",
                             plotlyOutput("ShotPlot")), 
                    tabPanel("Passes",
                             plotlyOutput("PassesPlot")), 
                    tabPanel("Key Passes",
                             plotlyOutput("KeyPassesPlot")), 
                    tabPanel("Regains",
                             plotlyOutput("RegainsPlot")), 
                    tabPanel("Touches",
                             plotlyOutput("TouchesPlot")), 
                    tabPanel("Turnovers",
                             plotlyOutput("TurnoversPlot")))) )) 


# ----- 8. Updated Server Code -----

server <- function(input, output, session){ # via the observe function below we can make sure that the second filter is based on the stage selection
  
  observe({
    new_choices <- Match_list %>%
      filter(Stage == input$select_stage) %>%
      pull(Match)
    
    new_choices <- c(new_choices)
    
    updateSelectInput(session, inputId = "select_match", #here R will update the select_match input with the newly filtered choices
                      choices = new_choices)
    
  })
  
  
  # below we run each function we have just written. Note how I added the dataframe we need to use (remember this was one of the inputs for all our functions) and I use input$select_stage and input$select_match as inputs taken from thh filters placed on the UI. We use reactive to ensure the visuals react based on user selection.
  
  
  Lineup_home <- reactive({
    lineup_home(TidyData, input$select_stage, input$select_match)
  })
  
  timeline_plot <- reactive({
    plot_timeline(ShotData, input$select_stage, input$select_match)
  })
  
  Lineup_away <- reactive({
    lineup_away(TidyData, input$select_stage, input$select_match)
  })
  
  shot_plot <- reactive({
    plot_shots(TidyData, input$select_stage, input$select_match)
  })
  
  passes_plot <- reactive({
    plot_passes(TidyData, input$select_stage, input$select_match)
  })
  
  keypasses_plot <- reactive({
    plot_keypasses(TidyData, input$select_stage, input$select_match)
  })
  
  regains_plot <- reactive({
    plot_regains(TidyData, input$select_stage, input$select_match)
  })
  
  touches_plot <- reactive({
    plot_touches(TidyData, input$select_stage, input$select_match)
  })
  
  turnovers_plot <- reactive({
    plot_turnovers(TidyData, input$select_stage, input$select_match)
  })
  
  results_text <- reactive({
    text_results(TidyData, input$select_stage, input$select_match)
  })
  
  stats_boxH<-reactive({
    stats_home(TidyData, input$select_stage, input$select_match)})
  
  stats_boxA<-reactive({
    stats_away(TidyData, input$select_stage, input$select_match)})
  
  
  # Next we create our outputs. Note the naming of the outputs is in line with how I named the outputs in the UI (e.g. dataTableOutput("Lineup_home) refers to output\$Lineup_home).
  
  
  output$title <- renderText(input$select_match) # this creates a dashboard title which shows which match is analysed
  
  output$Result <- renderText(results_text()) #this renders a text output in this case the match results (as per our results_text function)
  
  output$Lineup_home <- renderDataTable(Lineup_home()) # this renders a table output, our starting 11
  
  output$TimeLinePlot <- renderPlotly(timeline_plot()) # this renders a plotly plot (in this case our shot timeline)
  
  output$Lineup_away <- renderDataTable(Lineup_away()) # this renders a table output, our starting 11
  
  output$ShotPlot <- renderPlotly(shot_plot()) # the next few lines of code render the top 5 players for each outcome
  
  output$PassesPlot <- renderPlotly(passes_plot())
  
  output$KeyPassesPlot <- renderPlotly(keypasses_plot())
  
  output$RegainsPlot <- renderPlotly(regains_plot())
  
  output$TouchesPlot <- renderPlotly(touches_plot())
  
  output$TurnoversPlot <- renderPlotly(turnovers_plot())
  
  output$ShotsH <- renderValueBox({shots <- stats_boxH()$Shots
  valueBox(
    value = shots,
    subtitle = "Shots",
    color = "orange"
  )
  })
  
  output$ShotsTargetH <- renderValueBox({shots <- stats_boxH()$ShotsOnTarget
  valueBox(
    value = shots,
    subtitle = "Shots on Target",
    color = "orange"
  )
  })
  
  output$ShotsAccuracyH <- renderValueBox({shots <- paste(stats_boxH()$ShotAccuracy,"%")
  valueBox(
    value = shots,
    subtitle = "Shot accuracy",
    color = "orange"
  )
  })
  
  output$ShotsA <- renderValueBox({shots <- stats_boxA()$Shots
  valueBox(
    value = shots,
    subtitle = "Shots",
    color = "aqua"
  )
  })
  
  output$ShotsTargetA <- renderValueBox({shots <- stats_boxA()$ShotsOnTarget
  valueBox(
    value = shots,
    subtitle = "Shots on Target",
    color = "aqua"
  )
  })
  
  output$ShotsAccuracyA <- renderValueBox({shots <- paste(stats_boxA()$ShotAccuracy,"%")
  valueBox(
    value = shots,
    subtitle = "Shot accuracy",
    color = "aqua"
  )
  })
  
  output$PassesH <- renderValueBox({shots <- stats_boxH()$Passes
  valueBox(
    value = shots,
    subtitle = "Passes",
    color = "orange"
  )
  })
  
  output$KPassesH <- renderValueBox({shots <- stats_boxH()$KeyPasses
  valueBox(
    value = shots,
    subtitle = "Key Passes",
    color = "orange"
  )
  })
  
  output$RegainsH <- renderValueBox({shots <- stats_boxH()$Regains
  valueBox(
    value = shots,
    subtitle = "Regains",
    color = "orange"
  )
  })
  
  output$PassesA <- renderValueBox({shots <- stats_boxA()$Passes
  valueBox(
    value = shots,
    subtitle = "Passes",
    color = "aqua"
  )
  })
  
  output$KPassesA <- renderValueBox({shots <- stats_boxA()$KeyPasses
  valueBox(
    value = shots,
    subtitle = "Key passes",
    color = "aqua"
  )
  })
  
  output$RegainsA <- renderValueBox({shots <- stats_boxA()$Regains
  valueBox(
    value = shots,
    subtitle = "Regains",
    color = "aqua"
  )
  })
  
}


# ----- 9. Run Code -----

shinyApp(ui, server)


