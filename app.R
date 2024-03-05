library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)

## UI ----
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  titlePanel("Stats 101"),
  
  sidebarLayout(
    # Sidebar content
    sidebarPanel(
      
      # selector for A
      numericInput(
        inputId = "a_votes",
        label = "Votes for Celeb A",
        min = 0,
        max = 10,
        value = 0
      ),
      numericInput(
        inputId = "a_n",
        label = "Num Observations for Celeb A",
        min = 0,
        max = 10,
        value = 1
      ),
      # selector for B
      numericInput(
        inputId = "b_votes",
        label = "Votes for Celeb B",
        min = 0,
        max = 10,
        value = 0
      ),
      numericInput(
        inputId = "b_n",
        label = "Num Observations for Celeb B",
        min = 0,
        max = 10,
        value = 1
      )
    ), 
    
    # Main panel
    mainPanel(
      width = 8,
      tags$h3("Who's gayer?"),
      br(),
      textOutput("results"),
      plotOutput("plot")
    )
  )
)

####----  
## Server ----
server <- function(input, output, session) {
  
  # Get input
  a_yes <- reactive({input$a_votes})
  a_n <- reactive({input$a_n})
  b_yes <- reactive({input$b_votes})
  b_n <- reactive({input$b_n})
  
  # stat test
  output$results <- renderText({
    prop_test_results <- prop.test(x = c(a_yes(), b_yes()),
                                   n = c(a_n(), b_n()))
    p_value <- prop_test_results$p.value
    
    if (!is.na(p_value)) {
      if (p_value < 0.05) {
        if (a_yes() > b_yes()) {
          message <- "Celeb A!"
        } else {
          message <- "Celeb B!"
        }
      } else {
        message <- "Inconclusive!"
      }
    } else {
      message <- "Waiting for data to be entered..."
    }
    
    message
  })

  # plot
  output$plot <- renderPlot({
    # Parameters for the first normal distribution
    mu1 <- 100*a_yes()/a_n()     # Mean
    if (mu1 %in% c(0,100)) {
      sigma1 <- 100*sqrt(.00001*(1-(.00001))/a_n())
    } else {
      sigma1 <- 100*sqrt((a_yes() / a_n()) * (1 - (a_yes() / a_n()))/a_n())  # Standard deviation
    }
    
    # Parameters for the second normal distribution
    mu2 <- 100*b_yes()/b_n()     # Mean
    if (mu2 %in% c(0,100)) {
      sigma2 <- 100*sqrt(.00001*(1-(.00001))/b_n()) # adjust to ~ 0
    } else {
      sigma2 <- 100*sqrt((b_yes()/b_n())*(1-(b_yes()/b_n()))/b_n())  # Standard deviation
    }
    

    # Generate x values
    x <- seq(-50, 150, length.out = 1000)

    # Calculate PDF for both distributions
    pdf1 <- dnorm(x, mean = mu1, sd = sigma1)
    pdf2 <- dnorm(x, mean = mu2, sd = sigma2)

    # Plot the first normal distribution
    plot(x, pdf1, type = "l", ylim = c(0, max(pdf1, pdf2)),
         main = "Results",
         xlab = "Number of People Voting Yes", ylab = "Don't Worry About It",
         col = "blue", lwd = 2)

    # Add the second normal distribution to the plot
    lines(x, pdf2, col = "red", lwd = 2)

    # Add a legend
    legend("topright", legend = c("Celeb A", "Celeb B"),
           col = c("blue", "red"), lwd = 2, cex = 0.8)
  })
  
}

## Run ----
shinyApp(ui, server)