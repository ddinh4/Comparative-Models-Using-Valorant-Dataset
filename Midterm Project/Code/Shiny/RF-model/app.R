#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),

    # Application title
    titlePanel("Random Forest Prediction for Valorant Matches"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("map",
                      "Choose Map:",
                      choices = levels(train$Map)),  
          selectInput("agent",
                        "Choose Agent:",
                        choices = levels(train$Agent))
        ),

        mainPanel(
           h3("The predicted result of the match is:"),
           br(),
           textOutput("pred")
        )
    )
)

server <- function(input, output) {

    model <- readRDS("rf_model.RDS")
    input_df <- reactive({
      data.frame(map = as.double(input$map),
                 agent = as.double(input$agent))
    })
    pred <- reactive({
      predict(model, input_df(), type="response")
    })
    output$pred <- renderText({pred()})
}

# Run the application 
shinyApp(ui = ui, server = server)
