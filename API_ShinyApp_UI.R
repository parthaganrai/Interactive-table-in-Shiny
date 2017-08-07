###############################################################################################################
# This is a file used to define the the dashboard layout for the App
###############################################################################################################
#Required Packages
package<-c("iris","shiny","DT","shinydashboard","sqldf","rhandsontable","readr")

sapply(package,install.packages)

library(shiny)                  #Package to create shiny UI and server
library(DT)                     #Package to enhance functionalties using renderDataTable
library(shinydashboard)         #Pacakge to create appealing dashboard
library(sqldf)                  #Pacakge to incorporate SQL queries in the R programming
library(rhandsontable)          #Pacakge for iteractive data table
library(readr)                  #Pacakge to read data from local folder
library(dplyr)

#Loading File containing the API Details

DF1 <- read_csv("C:/Partha/Sykes/Project 1/All APIs Data Science Production May 2017.csv")

#Assigning File to DF
DF<-DF1
View(DF)
head(DF)

#Shiny UI-Defines the placement of various controls and input/output features on the dashboard

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = " Dash to view API"),
  #Dashboard sidebar  
    dashboardSidebar(
      checkboxInput(inputId = "readonly",label = "Read Only",value = TRUE),             
      h1(strong('Filter Data'),align="center",style="color:White"),
      fluidRow(
        selectizeInput('fil1', 'Account', choices = unique(DF$Account), multiple = TRUE),
        selectizeInput('fil2', 'API', choices = unique(DF$API), multiple = TRUE),
        selectizeInput('fil3', 'Keys / Endpoints', choices = unique(DF$X6), multiple = TRUE),
        selectizeInput('fil4', 'Production', choices = unique(DF$Production), multiple = TRUE),
        selectizeInput('fil5', 'Type', choices = unique(DF$type), multiple = TRUE)
        ),
       hr(),
        fluidRow(column(actionButton(inputId = "filter",label = "Filter"),width = 4),
                 column(actionButton(inputId = "clear",label = "Clear"),width = 4)
        )
     ),
  
    
    dashboardBody(
      fluidRow(),
      rHandsontableOutput(outputId = 'table',height = 500),                           #Rhandsontable provides interactivity in the output table
      fluidRow(column(10,allign="center",actionButton(inputId = "save",label = "   Save   ")),
               #column(,actionButton(inputId = "cancel",label = "Cancel")),
               column(1, align="right",
                      checkboxInput(inputId = "filtered",label = "Download_All",value = TRUE)),
               column(1, align="right",
                      actionButton("download",label = "Download"))
               ),
      br(),
      h1(strong('Add New Records'),align="center",style="color:black"),
       
      fluidRow(
        column(selectizeInput('add1', 'Account', choices = unique(DF$Account), multiple = FALSE),width = 3),
        column(selectizeInput('add2', 'Production', choices = unique(DF$Production), multiple = FALSE),width =3),
        column(textInput('add3', 'API',value = ''),width = 3),
        column(textInput('add4', 'Description', value = ''),width = 3)),
      fluidRow(
        column(textInput('add5', 'Azure ML Experiment/API Facing Name',value=''),width = 3),
        column(selectizeInput('add6', 'X6', choices = unique(DF$X6), multiple = FALSE),width = 3),
        column(textInput('add7', 'Keys / Endpoints',value=''),width = 3),
        column(selectizeInput('add8', 'Type', choices = unique(DF$type), multiple = FALSE),width = 3)),
      fluidRow(
        column(selectizeInput('add9', 'AzureML DB table input / output', choices = unique(DF$`AzureML DB table input / output`), multiple = FALSE),width = 3),
        column(selectizeInput('add10', 'Other SQL DB input dependency', choices = unique(DF$`Other SQL DB input dependency`), multiple = FALSE),width = 3),
        column(selectizeInput('add11', 'Data Factory Jobs', choices = unique(DF$`Data Factory Jobs`), multiple = FALSE),width = 3)
      ),
      fluidRow(column(actionButton('Add', '       Add        '),width=12,allign="center"))
  )
)
)

