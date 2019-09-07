
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("How much will you save?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("loan_amount", "Loan Amount:", 150000),
            numericInput("annual_rate", "Annual Rate:", 0.04),
            numericInput("term", "Term (months):", 360),
            numericInput("xtr1", "Extra monthly payment:", 0, step = 50),
            actionButton("calculate", "Calculate"),
            downloadButton("report", "Download Report")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("savings"),
            tableOutput("schedule")
        )
    )
))
