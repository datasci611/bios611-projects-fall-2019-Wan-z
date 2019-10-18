library(shiny)
library(shinydashboard)
source('helper_functions.R')

ui <- fluidPage(
  
  titlePanel('Analysis on UMD Data'),
  
  navlistPanel(
    "Discontinued or Small Scale Services",
    tabPanel("Trends",
             h4('How Trends of Discontinued or Small Scale Services Look Like'),
             sidebarLayout(
               sidebarPanel(
                 selectInput("trend", 
                   h5("Trend of Which Variable"), 
                   choices = list('Bus Tickets' = 1,
                                  'Diapers' = 2,
                                  'School Kits' = 3,
                                  'Hgyiene Kits' = 4,
                                  'Financial Support' = 5
                           )
                 )
               ),
               mainPanel(
                 plotOutput(outputId = "popPlot")
                       ) 
               )
             ),
    tabPanel("Distributions",
             h4('How Distributions of Discontinued or Small Scale Services Look Like'),
             sidebarLayout(
               sidebarPanel(
                 selectInput("distri", 
                             h5("Distribution of Which Variable"), 
                             choices = list('Bus Tickets' = 1,
                                            'Diapers' = 2,
                                            'School Kits' = 3,
                                            'Hgyiene Kits' = 4,
                                            'Financial Support' = 5
                             )
                 )
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Histogram", plotOutput("hist")),
                             tabPanel("Smooth", plotOutput("smooth"), textOutput('text'))
               ) 
             )
           )
          ),
    "Food and Clothing",
    tabPanel("Food", 
             navbarPage("How Food Support Changes over Time",
               tabPanel("Plot",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("select_food", 
                                        h5("Variables for x-axis"), 
                                        choices = list('Total Pounds of Food' = 1,
                                                       'Number of People' = 2,
                                                       'Pounds of Food Per Person' = 3
                                        )
                            )
                          ),
                          mainPanel(
                            plotOutput("plotFood")
                          )
                        )
               ),
               tabPanel("Correlation",
                        plotOutput("correlation")
               )
             )
             ),
    tabPanel("Clothing",
             h4('Trends and Distributions of Clothing Items'),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("plotType", 
                              h5("Plots Type"),
                              choices = list("Histogram" = 1, "Smooth" = 2)
                              )
               ),
               mainPanel(
                 plotOutput(outputId = "cloth")
               )
             )
             ),
    "Analysis on Clients",
    tabPanel("Prediction of 2019",
             navbarPage("Prediction of 2019",
                        tabPanel("Cases",
                                 fluidRow(
                                   column(width = 6,
                                          plotOutput("plot1", height = 350,
                                                     click = "plot1_click",
                                                     brush = brushOpts(
                                                       id = "plot1_brush"
                                                     )
                                          ),
                                          actionButton("exclude_toggle", "Toggle points"),
                                          actionButton("exclude_reset", "Reset"),
                                          textOutput('text1')
                                   )
                                 )
                                 ),
                        tabPanel("Clients",
                                 fluidRow(
                                   column(width = 6,
                                          plotOutput("plot2", height = 350,
                                                     click = "plot2_click",
                                                     brush = brushOpts(
                                                       id = "plot2_brush"
                                                     )
                                          ),
                                          actionButton("exclude_toggle2", "Toggle points"),
                                          actionButton("exclude_reset2", "Reset"),
                                          textOutput('text2')
                                   )
                                 )
                        )
                        
             )
  )
  )
)




server <- function(input, output) {
  output$popPlot <- renderPlot({
    f(as.integer(input$trend))
  })
  output$hist <- renderPlot({
    f_distri(as.integer(input$distri))
  })
  output$smooth <- renderPlot({
    f_smooth(as.integer(input$distri))
  })
  output$text <- renderText({
    tx(as.integer(input$distri))
  })
  output$cloth <- renderPlot({
    f_cloth(as.integer(input$plotType))
  })
  output$plotFood <- renderPlot({
    f_food(as.integer(input$select_food))
  })
  output$correlation <- renderPlot({
    pairs(df_food)
  })
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(df_client))
  )
  
  output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- df_client[ vals$keeprows, , drop = FALSE]
    exclude <- df_client[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(year, cases)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25)
      
  })
  
  output$text1 <- renderText({
    keep <- df_client[ vals$keeprows, , drop = FALSE]
    lmod <- lm(cases ~ year, data = keep) 
    predict_cases2019 <- lmod$coefficients %*% c(1,2019)
    paste('The amount of services in 2019 is predict to be', predict_cases2019)
  })
  
  output$plot2 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- df_client[ vals$keeprows, , drop = FALSE]
    exclude <- df_client[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(year, clients)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25)
    
  })
  
  output$text2 <- renderText({
    keep <- df_client[ vals$keeprows, , drop = FALSE]
    lmod <- lm(clients ~ year, data = keep) 
    predict_cases2019 <- lmod$coefficients %*% c(1,2019)
    paste('The number of clients in 2019 is predict to be', predict_cases2019)
  })
  
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(df_client, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot2_click, {
    res <- nearPoints(df_client, input$plot2_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(df_client, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$exclude_toggle2, {
    res <- brushedPoints(df_client, input$plot2_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(df_client))
  })
  
  observeEvent(input$exclude_reset2, {
    vals$keeprows <- rep(TRUE, nrow(df_client))
  })
  
}

shinyApp(ui, server)