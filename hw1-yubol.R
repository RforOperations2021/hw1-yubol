#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(dplyr)

data <- read.csv(file='guns.csv', header=TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(
  setBackgroundImage(
    src = "https://www.blackknightinc.com/wp-content/uploads/2019/10/data-transfer.jpg"
  ),
    # Application title
    titlePanel("Gun Deaths in the US: 2012-2014"),
    downloadLink("downloadData", "Download Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

          # Select which types of movies to plot ------------------------
          checkboxGroupInput(inputId = "selected_place",
                             label = "Select Course Difficulty:",
                             choices = c("Home", "Street", "Other specified"),
                             selected = "Home"),
          # Select variable for year ----------------------------------
          selectInput(inputId = "selected_year", 
                      label = "Year:",
                      choices = c("2012" = "2012", 
                                  "2013" = "2013",
                                  "2014" = "2014"), 
                      selected = "2012"),
          # Show data table ---------------------------------------------
          checkboxInput(inputId = "show_data",
                        label = "Show data table",
                        value = TRUE),
          # Select sample size ----------------------------------------------------
          numericInput(inputId = "n_samp", 
                       label = "Sample size:", 
                       min = 1, max = nrow(data), 
                       value = 50)

        ),
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Monthly Comparision", plotOutput("plot1")), 
            tabPanel("Comparison by Gender", plotOutput("plot2")), 
            tabPanel("Comparison by Gender", plotOutput("plot3"))
          ),
          
          # Show data table ---------------------------------------------
          DT::dataTableOutput(outputId = "datatable")
        )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # Create a subset of data filtering for selected title types ------
  data_subset <- reactive({
    req(input$selected_place) # ensure availablity of value before proceeding
    data%>%
      filter(year %in% input$selected_year, place %in% input$selected_place)

  })
  
  
  # Update the maximum allowed n_samp for selected type courses ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(data_subset())),
                       max = nrow(data_subset())
    )
  })
  
  # Create new df that is n_samp obs from selected type courses ------
  data_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(data_subset(), input$n_samp)
  })

  
  
  # Print data table if checked -------------------------------------
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = data_sample()[, 2:10], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Create plot1 object the plotOutput function is expecting --
  output$plot1 <- renderPlot({

    ggplot(data=data_subset(),aes(x=month,fill = as.factor(month)))+geom_bar()+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
      labs(title = "(Not Influenced by Sample Size)")
    
  })
  
  # Create plot2 object the plotOutput function is expecting --
  output$plot2 <- renderPlot({
    ggplot(data=data_subset(),aes(x=intent,fill = as.factor(intent)))+geom_bar()+
      scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
      labs(title = "(Not Influenced by Sample Size)")
    
    
  })
  
  # Create plot3 object the plotOutput function is expecting --
  output$plot3 <- renderPlot({
    
    ggplot(data = data_sample(), aes(x = sex,y=age)) +
      geom_boxplot()+
      labs(title = "(Influenced by Sample Size)")
  })
  

  
  # Create download handler --
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
