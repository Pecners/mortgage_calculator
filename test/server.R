#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    
    output$schedule <- renderDataTable({
        monthly_rate <- input$annual_rate/12
        
        total_PI <- input$loan_amount * (input$monthly_rate * (1 + input$monthly_rate) ^ input$term)/(((1 + input$monthly_rate) ^ input$term) - 1)
        
        interest <- principal <- balance <- vector("numeric", input$term)
        
        for (i in 1:input$term) {
            intr <- input$loan_amount * monthly_rate
            prnp <- total_PI - intr
            loan_amount <- input$loan_amount - prnp
            
            interest[i] <- intr
            principal[i] <- prnp
            balance[i] <- loan_amount
        }
        
        std_sched <- tibble(month = 1:input$term, 
                            interest = round(interest, 2), 
                            principal = round(principal, 2), 
                            balance = round(balance, 2))
        std_sched
        })

})
