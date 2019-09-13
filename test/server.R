
library(shiny)
library(tidyverse)
library(DT)
library(rmarkdown)
library(tinytex)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
   std_sched <- eventReactive(input$calculate, {
      
      # Calculate standard amortization schedule
      term <- input$term
      loan_amount <- input$loan_amount
      annual_rate <- input$annual_rate
      
      monthly_rate <- annual_rate/12
      total_PI <- loan_amount * 
         (monthly_rate * (1 + monthly_rate) ^ term)/
         (((1 + monthly_rate) ^ term) - 1)
      
      interest <- principal <- balance <- vector("numeric", term)
      
      for (i in 1:term) {
         intr <- loan_amount * monthly_rate
         prnp <- total_PI - intr
         loan_amount <- loan_amount - prnp
         
         interest[i] <- intr
         principal[i] <- prnp
         balance[i] <- loan_amount
      }
      
      std_sched <- tibble(Payment = 1:term, 
                          Interest = round(interest, 2), 
                          Principal = round(principal, 2), 
                          Balance = paste("$", format(round(balance, 2), big.mark = ",")))
      std_sched
   })
   
   # Calculate updated schedule
   updated_sched <- eventReactive(input$calculate, {
      
      term <- input$term
      loan_amount1 <- input$loan_amount
      annual_rate <- input$annual_rate
      
      monthly_rate <- annual_rate/12
      total_PI <- loan_amount1 * 
         (monthly_rate * (1 + monthly_rate) ^ term)/
         (((1 + monthly_rate) ^ term) - 1)
      
      interest1 <- principal1 <- extra <- balance1 <- bonus <- equity <- vector("numeric", term)
      
      for (i in 1:term) {
         intr1 <- loan_amount1 * monthly_rate
         prnp1 <- total_PI - intr1
         xtr1 <- input$xtr1
         loan_amount1 <- loan_amount1 - prnp1 - xtr1 - bonus[i]

         extra[i] <- xtr1
         interest1[i] <- intr1
         principal1[i] <- prnp1
         balance1[i] <- loan_amount1
      }
      
      updated_sched <- tibble(Payment = 1:term, 
                              Interest = round(interest1, 2), 
                              Principal = paste("$", format(round(principal1, 2), big.mark = ",")), 
                              Extra = paste("$", format(round(extra, 2), big.mark = ",")), 
                              Bonus = paste("$", format(round(bonus, 2), big.mark = ",")),
                              Balance = paste("$", format(round(balance1, 2), big.mark = ","))) %>%
         filter(!grepl("-", Interest))
      
      updated_sched
   })
   
   savings <- eventReactive(input$calculate, { 
      savings <- sum(std_sched()$Interest) - sum(updated_sched()$Interest)
      savings
   })
   
   cut <- eventReactive(input$calculate, {
      cut <- input$term -nrow(updated_sched())
      cut
   })
   
   text <- eventReactive(input$calculate, {
      
      if(input$xtr1 == 0) {
         paste("Add extra payments to see how much you'd save over the lifetime of your loan!")  
      }
      else{
      term <- input$term
      loan_amount <- input$loan_amount
      annual_rate <- input$annual_rate
      
      monthly_rate <- annual_rate/12
      total_PI <- loan_amount * 
         (monthly_rate * (1 + monthly_rate) ^ term)/
         (((1 + monthly_rate) ^ term) - 1)
      
      paste("By paying an extra", 
            paste("$", format(input$xtr1, digits = 2, big.mark = ","), sep =""), 
            "dollars a month, you will save",
            paste("$", format(savings(), nsmall = 2, big.mark = ","), sep =""),
            "in interest, and you will cut", 
            floor(cut()/12), "years and", cut()%%12, 
            "months off your loan. Your increased cashflow over that time from rent will be", 
            paste("$", format(cut() * (total_PI), nsmall = 2, big.mark = ","),
                  ",", sep =""),
            "which, added to the interest savings amounts to",
            paste("$", format((savings() + cut() * (total_PI)), 
                              nsmall = 2, big.mark = ","), 
                  ".", sep = ""), sep = " ")
      }
   })
   
   output$report <- downloadHandler(
      filename = "mortgage_report.pdf",
      content = function(file) {
         output <- rmarkdown::render("mortgage_report.Rmd", 
                                     params = list(loan_amount = input$loan_amount,
                                                   annual_rate = input$annual_rate,
                                                   term = input$term,
                                                   xtr1 = input$xtr1)
         )
         file.copy(output, file)
      })
   
   output$savings <- renderText({
         text()
   }) 
   
   output$schedule <- renderTable({
      updated_sched()
   })
   
})
