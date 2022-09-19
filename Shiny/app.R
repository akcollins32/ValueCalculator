#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

costdata <- data.frame(list(Orders = c("Pap Smear", "CXR", "MRI Brain w Contrast","Breast Biopsy", "Coronary Artery Bypass Graft, 1-3", "Coronary Artery Bypass Graft, 4+",
                                       "Total Hip Arthroplasty", "Appendectomy"),
               Price = c(34.59, 80.57, 169.34, 327.65, 2689.45, 3468.97, 10649.51, 2413.89)))

library(tidyverse)
library(shiny)


  ui <- pageWithSidebar(headerPanel("Value Calculator V0.1"),
                     sidebarPanel(selectizeInput("items",
                                                 "Orders",
                                                 costdata$Orders,
                                                 multiple = T),
                                  #textInput("text2", "Price"),
                                  actionButton("update", "Update Table"),
                                  #actionButton("reset", "Clear Table"),
                                  actionButton("resetInput", "Reset Inputs and Table")),
                     mainPanel(tableOutput("table1")))
  
  server=function(input, output,session) {
    values <- reactiveValues()
    
    ##Just used to see how many inputs are selected, used later on for for loop
    # observeEvent(input$items, {
    #   cat(length(input$items), input$items)
    # })
    
    ##initializes running list df
    values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
    
    
    # ##resets the table
    # resetTable <- observeEvent(input$reset,{
    #   values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
    #   
    #     output$table1 <- renderTable({values$df})
    # })
    # 
    
    ##resets the user inputs and table
    resetSelections <- observeEvent(input$resetInput,{
      updateSelectizeInput(session, "items", selected = "")
      
      values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
      
      output$table1 <- renderTable({values$df})
    })
    
    
    # observeEvent(input$items, {
    #              tablecosts <- costdata%>%
    #                filter(costdata$Orders %in% input$items)
    #              
    #       tablecostsf <- tablecosts[match(input$items, tablecosts$Orders),] 
    #              
    #       print(tablecosts)
    #       
    #       print(tablecostsf)
    #       
    # })
    
    
    ##Adds rows to running list df
    newEntry <- observeEvent(input$update,{ 
      values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
      
      #relevant costs
      tablecosts <- costdata%>%
        filter(costdata$Orders %in% input$items)
      
      #changes order such that it matches input order (needed for multiselection)     
      tablecostsf <- tablecosts[match(input$items, tablecosts$Orders),] 
      
        if(length(input$items) == 1){
        newLine <- isolate(c(input$items, tablecostsf$Price))
        isolate(values$df[nrow(values$df) + 1,] <- c(input$items, tablecostsf$Price))
        isolate(values$df[nrow(values$df) + 1,] <- c("Total", sum(as.numeric(values$df[[2]]))))
        } else if (length(input$items) > 1){
          for(i in 1:length(input$items)){
            newLine <- isolate(c(input$items[[i]], tablecostsf$Price[[i]]))
            isolate(values$df[nrow(values$df) + 1,] <- c(input$items[[i]], tablecostsf$Price[[i]]))
            if(i == length(input$items)){
              newLine <- isolate(c(input$items[[i]], tablecostsf$Price[[i]]))
              isolate(values$df[nrow(values$df) + 1,] <- c("Total", sum(as.numeric(values$df[[2]]))))}
          
        }
      }
    })
    output$table1 <- isolate(renderTable({values$df}))
    
  }

  shinyApp(ui = ui, server = server)