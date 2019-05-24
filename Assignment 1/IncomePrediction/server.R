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
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(rpart)
library(e1071)
library(C50)

shinyServer(function(input, output) {
  c5model <- readRDS("c5model.rds")
   
  output$incomeOutput <- renderText({
    userInput <- data.frame(age=c(as.numeric(input$Age)),
                       workclass=c(input$workclass),
                       education=c(input$education),
                       'education-num'=c(10),
                       marStat=c(input$marStat),
                       occupation=(input$occupation),
                       relationship=(input$relationship),
                       race=c(input$Race),
                       sex=c(input$Sex),
                       'hrs-per-week'=c(as.numeric(input$`hrs-per-week`)))
    incomepredict <- predict(c5model,userInput)
    
    if(incomepredict == 1){
      
      "You are valued at more than 50,000$"
    }
    else{
      "You are valued at less than 50,000$"
    }
    
    })
  
})
