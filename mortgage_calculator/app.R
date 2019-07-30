#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How much will you save?"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("loan_tot", "Loan Total:", 0),
            numericInput("rate", "Annual Rate:", 0)
        )
    ),
    
        # Show a plot of the generated distribution
        mainPanel(
           textOutput("total_loan")
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$total_loan <- renderText({
        
        paste("With a loan of",
              paste("$", format(input$loan_tot, digits = 2, big.mark = ","), sep =""))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

