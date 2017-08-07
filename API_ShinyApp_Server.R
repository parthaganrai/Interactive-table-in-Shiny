###############################################################################################################
#Code to define the functionalties 
###############################################################################################################

server<-shinyServer (function(input, output,session){

values<-reactiveValues()                   #Creating a reactive variable        
values[["TempB"]]<-NULL    
# Defining the action of filters 

observeEvent(input$filter,{
  
   values[["Tempa"]]<-rbind(isolate(values[["DF"]]),isolate(values[["TempB"]]))  # Creating a temporary variable and initailizing with values of entire data
   values[["TempB"]]<-NULL                                                       # Creating a temp empty variable
   
  # Filtering data based on Filter1    
   if(!is.null(isolate(input$fil1)))
     {
      values[["Temp"]]<-isolate(values[["Tempa"]])  
      values[["Tempa"]]<-(isolate(values[["Temp"]]) %>% filter(values[["Temp"]]$Account %in% isolate(input$fil1)))     #Storing filtered data into tempa
      values[["Tempb"]]<-(isolate(values[["Temp"]]) %>% filter(!(values[["Temp"]]$Account %in% isolate(input$fil1))))  #Storing remaining data into tempb
      values[["TempB"]]<-rbind(isolate(values[["TempB"]]),isolate(values[["Tempb"]]))
      }
    
   #Filtering data based on Filter2
   if(!is.null(isolate(input$fil2)))
    {
      values[["Temp"]]<-isolate(values[["Tempa"]]) 
      values[["Tempa"]]<-(isolate(values[["Temp"]]) %>% filter(values[["Temp"]]$API %in% isolate(input$fil2)))
      values[["Tempb"]]<-(isolate(values[["Temp"]]) %>% filter(!(values[["Temp"]]$API %in% isolate(input$fil2))))
      values[["TempB"]]<-rbind(isolate(values[["TempB"]]),isolate(values[["Tempb"]]))
    }
    
    values[["TempA"]]<-isolate(values[["Tempa"]])              #Assigning Tempa to TempA
    values[["DF"]]<-isolate(values[["TempA"]])                 #Assigning TempA to DF. DF is displayed in table output
  })


# Making filtered options reactive

observe(
  {
    updateSelectizeInput(session,'fil2',choices = unique(values[["DF"]]$API))
    updateSelectizeInput(session,'fil1',choices = unique(values[["DF"]]$Account))
    updateSelectizeInput(session,'fil3',choices = unique(values[["DF"]]$X6))
    updateSelectizeInput(session,'fil4',choices = unique(values[["DF"]]$Production))
    updateSelectizeInput(session,'fil5',choices = unique(values[["DF"]]$type))
  } )


  
# Action to clear the filters applied on the data table  
 observeEvent(input$clear,{
    values[["DF"]]<-rbind(isolate(values[["DF"]]),isolate(values[["TempB"]]))  # Merging the filtered and unfiltered data
    values[["TempA"]]<-NULL                                                    # Making Temp files Null
    values[["TempB"]]<-NULL                                                    # Making Temp files Null  
    DF <<- isolate(values[["DF"]])                                             # Writing the edited file in the server to the R server  
  })
  
# Defining operations from the actions buttons in the dashboard body
 
 ##Making Data table reactive
  observe({
    if (!is.null(input$table)) 
    {
      values[["previous"]] <- isolate(values[["DF"]])
      DF <- hot_to_r(input$table)
    } 
    else 
    {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  
  
## Outputing data to the console
  output$table <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = TRUE,readOnly=input$readonly) #Making output table non-editable using readonly input
  })
  
  
## Save Data to orginal data frame
  observeEvent(input$save, {
    if(isolate(!input$readonly)){
      values[["DF"]]<-rbind(isolate(values[["DF"]]),isolate(values[["TempB"]]))
      values[["TempB"]]<-NULL
      DF <<- isolate(values$DF)
    }
  })
  
## Cancel last action    
  #observeEvent(input$cancel, {
    
   # if(!is.null(isolate(values[["previous"]]))) values[["DF"]] <- rbind(isolate(values[["previous"]]),isolate(values[["TempB"]]))
    
    
   #DF <<- isolate(values$DF)
    
#  })
  
  
## Add new rows to the data table
  observeEvent(input$Add, {
    if(isolate(!input$readonly)) { 
      DF <- isolate(values[["DF"]])
#      values[["previous"]] <- DF
      newrow <- isolate(c(input$add1,input$add2,input$add3,input$add4,input$add5,input$add6,
                          input$add7,input$add8,input$add9,input$add10,input$add11))       # Creating a vector for new record
     
       isolate(values[["DF"]] <- rbind(isolate(values[["DF"]]),newrow))                    # Binding New record to the original data table 
      values[["DF"]]<-rbind(isolate(values[["DF"]]),isolate(values[["TempB"]]))
      values[["TempB"]]<-NULL
      DF <<- values$DF
    }
    
  })
  
# Download the Data table to local folder  
observeEvent(input$download, {
    
    if(isolate(!input$filtered))
       {
     isolate(write.csv(values[["DF"]],file = "API.csv",row.names = F))
     }   
    else {
      if(is.null(isolate(values[["TempB"]])))
      isolate(write.csv(values[["DF"]],file = "API_All.csv",row.names = F))
      else
        isolate(write.csv(rbind(isolate(values[["DF"]]),isolate(values[["TempB"]])),file = "API_All.csv",row.names = F))
      
      }
    
    })
  
})



nrow(DF)

shinyApp(ui=ui,server=server)

DF<-DF1
 