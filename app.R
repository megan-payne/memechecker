library(shiny)
library(ggplot2)
source("fin_functions.R")
# Define UI for application that plots the loan data
ui <- fluidPage(theme = "bootstrap.css",
   
   # Application title
   titlePanel("Meme Checker"),
   
   # Sidebar with a slider input for interest and loan lengths 
   sidebarLayout(
     sidebarPanel(
       helpText("Check out the meme using various APRs and loan lengths."),
             
       sliderInput("loanlength", 
                   label = "Amortization length (years)",
                   min = 23, max = 30, value=23),
       
       sliderInput("interest", 
                   label = "Interest rate (APR):",
                   min = 1, max = 12, value = 6, step=0.1),
       helpText("Beginning value of loan: $24,600"),
       helpText("Montly payment made: $118")

   ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot", click = "plot_click"),
        verbatimTextOutput("info")
      )
   )
)

# Define server logic required to draw plot
server <- function(input, output) {

   output$distPlot <- renderPlot({
      initLoan <- 26400
      minPayment <- 118

#      valPayment <- minPayCalc(input$loanlength, input$interest, initLoan)
      y1 <- currentLoanValue(initLoan, input$interest, input$loanlength, minPayment)
      x1 <- 1:(input$loanlength*12)
      y1 <- round(y1, 2)
      df1 <- data.frame(x=23*12, y=y1[[23*12]])
      plot(x1, y1, type="l", xlab="Time in months from beginning of repayment", 
           ylab="Principal amount in USD") 
      points(df1$x, df1$y, col="red", pch=19)

   })
   output$info <- renderText({
     paste0("Click on graph to get specific points.\n", "x (in years)=", 
            round(input$plot_click$x/12, 2), "\ny= (in dollars)", 
            input$plot_click$y)
   })
}


shinyApp(ui = ui, server = server)


