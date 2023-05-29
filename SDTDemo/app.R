#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(psycho)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualizing Signal Detection Parameters"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("BR",
                        "Base Rate:",
                        min = .0001,
                        max = .9999,
                        value = .1),
            sliderInput("Ntrials",
                        "Number of Trials:",
                        min = 10,
                        max = 10000,
                        value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
