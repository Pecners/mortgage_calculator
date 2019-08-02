#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("How much will you save?"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("loan_amount", "Loan Amount:", 100000),
            numericInput("annual_rate", "Annual Rate:", .04),
            numericInput("term", "Term (in months):", 360)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("savings"),
            tableOutput("schedule")
        )
    )
))
