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
                        value = .1,
                        step = .01,
                        animate = animationOptions(interval = 300, loop=T)),
            sliderInput("Ntrials",
                        "Number of Trials:",
                        min = 10,
                        max = 10000,
                        value = 100,
                        step = 100,
                        animate = animationOptions(interval = 300, loop=T)),
            sliderInput("HR",
                        "Hit Rate:",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .01,
                        animate = animationOptions(interval = 300, loop=T)),
            sliderInput("FA",
                        "False Alarm Rate:",
                        min = 0,
                        max = 1,
                        value = .5,
                        step = .01,
                        animate = animationOptions(interval = 300, loop=T))
            ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("tab1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    SDTvals <- reactive({
      Ntrials <- input$Ntrials
      tot <- Ntrials
      tp <- input$HR*input$BR*input$Ntrials # true positive or hit
      fp <- input$FA*input$BR*input$Ntrials # false positive or false alarm
      fn <- (1-input$HR)*(1-input$BR)*input$Ntrials # false negative or miss
      tn <- (1-input$FA)*(1-input$BR)*input$Ntrials # true negative or correct rejection
      sens <- tp/(tp+fn)
      spec <- tn/(tn+fp)
      pvp <- tp/(fp+tp)
      pvn <- tn/(tn+fn)
      eff <- (tp+tn)/Ntrials
      oddsratio <- (tp/fp)/(fn/tn)
      relrisk <- (tp/(tp+fp))/(fn/(fn + tn))
      relriskSE <- sqrt((fp/(tp*(tp+fp))) + (tn/(fn*(tn+fn))))
      relriskCI <- c(relrisk*exp(-1.96*relriskSE),relrisk*exp(1.96*relriskSE))
      kappa <- ((tp/tot + tn/tot) - ((tp/tot + fp/tot)*(tp/tot + fn/tot) + (fn/tot + tn/tot)*(tn/tot + fp/tot)))/(1 - ((tp/tot + fp/tot)*(tp/tot + fn/tot) + (fn/tot + tn/tot)*(tn/tot + fp/tot)))
      oafraccor <- (tp + tn)/tot
      missrate <- 1 - oafraccor
      NNT <- 1 / ((tp/(tp+fp)) - (fn/(fn + tn)))
      ARR <- (fn/(fn + tn)) - (tp/(tp+fp))
      RRR <- ARR/(fn/(fn + tn))
      PosLR <- sens/(1 - spec)
      NegLR <- (1 - sens) / spec
      DiagOR <- (sens/(1 - sens))/((1 - spec)/spec)
      YoudenJ <- sens + spec - 1
      NND <- 1/YoudenJ
      YulesQ <- (oddsratio - 1)/(oddsratio + 1)
      McNemars <- ((fn - fp)^2)/(fn + fp)
      McNemars.p <- round(pchisq(McNemars,df=1,lower.tail=F),digits=2)
      dp <- unlist(psycho::dprime(tp,fp,fn,tn))
      out <- t(cbind("Number of Trials"=Ntrials,
                   "True Positives (Hits)" = tp,
                   "False Positives" = fp,
                   "False Negatives (Misses)" = fn,
                   "True Negatives" = tn,
                   "Sensitivity" = sens,
                   "Specificity" = spec,
                   "Positive Predictive Value" = pvp,
                   "Negative Predictive Value" = pvn,
                   "Efficiency" = eff,
                   t(dp)))
      #out <- cbind(out,dp)
    })
    output$tab1 <- renderTable({
      SDTvals()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
