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
library(dplyr)
#library("tidyverse")
library(ggplot2)
library(plotly)



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
    
    #Exploratory Analysis
    #summary 
    output$summ<- renderDataTable({
        shopData <- shoppersIntDS
        if(input$varVisitorType != "All") {
            shopData <- shoppersIntDS %>% filter(VisitorType == input$varVisitorType)  %>%  select(BounceRates, ExitRates, PageValues)
        } else {
            shopData <- shoppersIntDS %>% select(BounceRates, ExitRates, PageValues)
        }
        datatable(round(apply(shopData, 2, summary), 2), 
                  caption = paste("Summary statistics for some of the attributes"),
                  options = list(pageLength = 6, dom = 'tip', paging=FALSE, info=FALSE), 
                  )
    })
    
    output$visitorTypeByRev<- renderTable(
        as.data.frame.matrix(table(shoppersIntDS$Revenue, shoppersIntDS$VisitorType)), 
        striped=TRUE, rownames = T,
        caption = paste("VisitorType vs Revenue Information"),
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    #Box Plots
    output$administrativeDurationByRev <- renderPlotly({
        #ProductRelated
        shopData <- shoppersIntDS
        if(input$varVisitorType != "All") {
            shopData <- shoppersIntDS %>% filter(VisitorType == input$varVisitorType)
        } 
        ggplotly(ggplot(shopData, aes(x = Revenue, y = Administrative_Duration)) + geom_boxplot() + 
            geom_jitter(aes(color = Revenue)) + ggtitle("BoxPlot for Product Related Page Browing Duration"))
    })
    output$informationalDurationByRev <- renderPlot(
        #ProductRelated
        ggplot(shoppersIntDS, aes(x = Revenue, y = Informational_Duration)) + geom_boxplot() + 
            geom_jitter(aes(color = Revenue)) + ggtitle("BoxPlot for Product Related Page Browing Duration")
    )
    output$productDurationByRev <- renderPlot(
        #ProductRelated
        ggplot(shoppersIntDS, aes(x = Revenue, y = ProductRelated_Duration)) + geom_boxplot() + 
            geom_jitter(aes(color = Revenue)) + ggtitle("BoxPlot for Product Related Page Browing Duration")
    )
    
    #Download box plots
    output$downAdministrativeDurationByRev <- downloadHandler(
        filename =  function() {
            paste("AdministrativeDurationByRev.png")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
            shopData <- shoppersIntDS
            png(file) # open the png device
            print(ggplot(shopData, aes(x = Revenue, y = Administrative_Duration)) + geom_boxplot() + 
                      geom_jitter(aes(color = Revenue)) + ggtitle("BoxPlot for Product Related Page Browing Duration")) # for GGPLOT
            dev.off()  # turn the device off
            
        } 
    )
    
    #Histograms
    output$administrativeHist <- renderPlot(
        ggplot(shoppersIntDS, aes(x = Administrative, ..density..)) + 
            geom_histogram(bins = 15) + 
            ggtitle("Histogram for Administrative") + 
            ylab("Density") + geom_density(col = "red", lwd = 1)
        )
    
    output$informationalHist <- renderPlot(
        ggplot(shoppersIntDS, aes(x = Informational, ..density..)) + 
            geom_histogram(bins = 20) + 
            ggtitle("Histogram for Informational") + 
            ylab("Density") + geom_density(col = "red", lwd = 1, adjust = 0.8)
    )
    
    output$productRelatedHist <- renderPlot(
        ggplot(shoppersIntDS, aes(x = ProductRelated, ..density..)) + 
            geom_histogram(bins = 20) + 
            ggtitle("Histogram for ProductRelated") + 
            ylab("Density") + geom_density(col = "red", lwd = 1, adjust = 0.8)
    )
    
})
