#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


#Load data
shoppersIntDS<- read.csv("data/online_shoppers_intention.csv")

shinyServer(function(input, output) {

    #Table of selected dataset
    output$shopperIntDT<- renderDataTable({
        datasetInput()
    })

    datasetInput <- reactive({
        #dtFull<- shoppersIntDS
        if(length(input$cols) != 0) {
            return(datatable(shoppersIntDS %>% dplyr::select(!!!input$cols), rownames = FALSE, filter = 'top'))
        } 
        return(datatable(shoppersIntDS, rownames = FALSE, filter = 'top'))
    })
    
    # Downloadable csv of selected dataset 
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("OnlineShopping", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame( downloadDS() ), file, row.names = FALSE)
        }
    )

    downloadDS <- reactive({
        if(length(input$cols) != 0) {
            return(shoppersIntDS %>% dplyr::select(!!!input$cols))
        } 
        return(shoppersIntDS)
    })

})
