#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# costdata <- data.frame(list(Orders = c("Pap Smear", "CXR", "MRI Brain w Contrast","Breast Biopsy", "Coronary Artery Bypass Graft, 1-3", "Coronary Artery Bypass Graft, 4+",
#                                        "Total Hip Arthroplasty", "Appendectomy"),
#                Price = c(34.59, 80.57, 169.34, 327.65, 2689.45, 3468.97, 10649.51, 2413.89)))

# 
# costdata <- fread("Data/Lab Costs_082622.csv")
# 
# costdata <- costdata[,c(8,6)]
# 
# colnames(costdata) <- c("Orders", "Price")
# 
# costdata <- costdata%>%
#   arrange(Orders)
# 
# write.csv(costdata, "LabCostsClean.csv", row.names = F)
#setwd("C:/Users/austi/Desktop/playground")




library(data.table)
library(tidyverse)
library(shiny)
costdata <- fread("Data/LabCostsClean.csv")
physfee <- read_rds("Data/Final Physician Fee Schedule (App).rds")

Descs <- unique(physfee$Description)
allorders <- append(Descs, costdata$Orders)


  ui <- 
    
    headerPanel(navbarPage(title = "Value Calculator V1.0",
                           tabPanel("About", htmlOutput("about")),
                           tabPanel("Explore",
                                    
                                    fluidPage(
                                      fluidRow(
                                        column(2,
                                               selectizeInput("location", "Location", unique(physfee$NAME)),
                                               selectizeInput("items",
                                                              "Orders",
                                                              choices = NULL,
                                                              multiple = T),
                                               actionButton("update", "Update Table"),
                                               actionButton("resetInput", "Reset Inputs and Table")),
                                        column(10,
                                               tableOutput("table1"))
                                        )
                                      )
                                    ),
                                    
                           navbarMenu("Cases", 
                                    tabPanel("Neuro",
                                             fluidPage(
                                                #fluidRow(
                                                  #column(12, htmlOutput("neurocase"),
                                                   fluidRow(
                                                     column(4,htmlOutput("neurocase")),
                                                     column(2, 
                                                            selectizeInput("caselocation", "Location", unique(physfee$NAME)),
                                                            selectizeInput("caseitems",
                                                                           "Orders",
                                                                           choices = NULL,
                                                                           multiple = T),
                                                            actionButton("update1", "Update Table"),
                                                            actionButton("resetInput1", "Reset Inputs and Table")),
                                                     column(6, 
                                                            tableOutput("table2"))
                                               )
                                             )
                                    #)
                                    
                                    ),
                                    tabPanel("Cardiology")
                    
                           )
                           
  )
  )
  
  
  
                
              
                     
                     
  
  server=function(input, output,session) {
  
### Calculator ------  
    
      values <- reactiveValues()
    
    ##Just used to see how many inputs are selected, used later on for for loop
    # observeEvent(input$items, {
    #   cat(length(input$items), input$items)
    # })
    
    ##initializes running list df
    values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
    
    #server side selectize all orders for improved performance
    updateSelectizeInput(session, 'items', choices = allorders, server = TRUE)
    
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
      updateSelectizeInput(session, "location", selected = input$location)
      
      values$df <- data.frame(Orders = numeric(0), Price = numeric(0))
      
      output$table1 <- renderTable({values$df})
    })
    

    # observeEvent(input$items, {
    #              tablecosts <- costdata%>%
    #                filter(costdata$Orders %in% input$items)
    # 
    #              fees <- physfee%>%
    #                filter(physfee$NAME %in% input$location)%>%
    #                select(c(9,7))
    #           
    #              
    #              fees <- fees%>%
    #                filter(fees$Description %in% input$items)
    #              
    #              
    #              colnames(fees) <- c("Orders", "Price")
    #              fees$Price <- as.numeric(fees$Price)
    #              
    #              tablecosts <- bind_rows(fees,tablecosts)
    #             
    #       tablecostsf <- tablecosts[match(input$items, tablecosts$Orders),]
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
      
      fees <- physfee%>%
        filter(physfee$NAME %in% input$location)%>%
        select(c(9,7))
      
     
      
      fees <- fees%>%
        filter(fees$Description %in% input$items)
      
      colnames(fees) <- c("Orders", "Price")
      fees$Price <- as.numeric(fees$Price)
      
      tablecosts <- bind_rows(fees,tablecosts)
      
      
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
    
#Cases ------
    
    output$neurocase <- renderUI({HTML("<p>Geckos are a group of usually small, usually nocturnal lizards. They are found on every continent except Australia.</p>")})
    output$hfcase <- renderUI({HTML("<p>Test multiple cases<p>")})
    
    
    #Case Calculator
    values1 <- reactiveValues()

    
    ##initializes running list df
    values1$df <- data.frame(Orders = numeric(0), Price = numeric(0))
    
    #server side selectize all orders for improved performance
    updateSelectizeInput(session, 'caseitems', choices = allorders, server = TRUE)
    
     
    
    ##resets the user inputs and table
    resetSelections <- observeEvent(input$resetInput1,{
      updateSelectizeInput(session, "caseitems", selected = "")
      updateSelectizeInput(session, "caselocation", selected = input$caselocation)
      
      values1$df <- data.frame(Orders = numeric(0), Price = numeric(0))
      
      output$table2 <- renderTable({values1$df})
    })
    
    
    
    ##Adds rows to running list df
    newEntry <- observeEvent(input$update1,{ 
      values1$df <- data.frame(Orders = numeric(0), Price = numeric(0))
      
      #relevant costs
      tablecosts <- costdata%>%
        filter(costdata$Orders %in% input$caseitems)
      
      fees <- physfee%>%
        filter(physfee$NAME %in% input$caselocation)%>%
        select(c(9,7))
      
      
      
      fees <- fees%>%
        filter(fees$Description %in% input$caseitems)
      
      colnames(fees) <- c("Orders", "Price")
      fees$Price <- as.numeric(fees$Price)
      
      tablecosts <- bind_rows(fees,tablecosts)
      
      
      #changes order such that it matches input order (needed for multiselection)     
      tablecostsf <- tablecosts[match(input$caseitems, tablecosts$Orders),] 
      
      if(length(input$caseitems) == 1){
        newLine <- isolate(c(input$caseitems, tablecostsf$Price))
        isolate(values1$df[nrow(values1$df) + 1,] <- c(input$caseitems, tablecostsf$Price))
        isolate(values1$df[nrow(values1$df) + 1,] <- c("Total", sum(as.numeric(values1$df[[2]]))))
      } else if (length(input$caseitems) > 1){
        for(i in 1:length(input$caseitems)){
          newLine <- isolate(c(input$caseitems[[i]], tablecostsf$Price[[i]]))
          isolate(values1$df[nrow(values1$df) + 1,] <- c(input$caseitems[[i]], tablecostsf$Price[[i]]))
          if(i == length(input$caseitems)){
            newLine <- isolate(c(input$caseitems[[i]], tablecostsf$Price[[i]]))
            isolate(values1$df[nrow(values1$df) + 1,] <- c("Total", sum(as.numeric(values1$df[[2]]))))}
          
        }
      }
    })
    
    
    
    output$table2 <- isolate(renderTable({values1$df}))
    
    
#About ------
    output$about <- renderUI({HTML("<p>The Value Calculator: What, How and Why?
                                  <p>This app is best viewed in fullscreen!<p>")})
  }
  

  shinyApp(ui = ui, server = server)