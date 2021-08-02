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
library(caret)


#Load data
shoppersIntDS <- read.csv("data/online_shoppers_intention.csv")
shopDS <- shoppersIntDS
shopDS$Revenue <- as.factor(shopDS$Revenue)
levels(shopDS$Revenue) <- c("No Revenue", "Revenue Generated")

shinyServer(function(input, output) {
    #Table of selected dataset
    output$shopperIntDT <- renderDataTable({
        datasetInput()
    })
    
    datasetInput <- reactive({
        #dtFull<- shoppersIntDS
        if (length(input$cols) != 0) {
            return(
                datatable(
                    shoppersIntDS %>% dplyr::select(!!!input$cols),
                    rownames = FALSE,
                    filter = 'top'
                )
            )
        }
        return(datatable(
            shoppersIntDS,
            rownames = FALSE,
            filter = 'top'
        ))
    })
    
    # Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("OnlineShopping", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(as.data.frame(downloadDS()), file, row.names = FALSE)
        }
    )
    
    downloadDS <- reactive({
        if (length(input$cols) != 0) {
            return(shoppersIntDS %>% dplyr::select(!!!input$cols))
        }
        return(shoppersIntDS)
    })
    
    #Exploratory Analysis
    #summary
    output$summ <- renderDataTable({
        shopData <- shoppersIntDS
        if (input$varVisitorType != "All") {
            shopData <-
                shoppersIntDS %>% filter(VisitorType == input$varVisitorType)  %>%  select(BounceRates, ExitRates, PageValues)
        } else {
            shopData <-
                shoppersIntDS %>% select(
                    BounceRates,
                    ExitRates,
                    PageValues,
                    Administrative_Duration,
                    Informational_Duration,
                    ProductRelated_Duration
                )
        }
        datatable(
            round(apply(shopData, 2, summary), 2),
            caption = paste("Summary statistics for some of the attributes"),
            options = list(
                pageLength = 6,
                dom = 'tip',
                paging = FALSE,
                info = FALSE
            ),
        )
    })
    
    output$visitorTypeByRev <- renderTable(
        as.data.frame.matrix(table(shopDS$Revenue, shopDS$VisitorType)),
        striped = TRUE,
        rownames = T,
        caption = paste("Revenue vs VisitorType"),
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    
    output$visitorTypeByWeekend <- renderTable(
        as.data.frame.matrix(table(shopDS$Weekend, shopDS$VisitorType)),
        striped = TRUE,
        rownames = T,
        caption = paste("Weekend vs VisitorType"),
        caption.placement = getOption("xtable.caption.placement", "top")
    )
    
    output$monthRevBox <- renderPlotly({
        shopData <- shoppersIntDS
        
        #convert to month abb.
        shopData[shopData$Month == "June", ]$Month = "Jun"
        shopData$Month <- factor(shopData$Month, levels = month.abb)
        #levels(shopData$Month) <- month.abb
        if (input$varVisitorType != "All") {
            shopData <-
                shoppersIntDS %>% filter(VisitorType == input$varVisitorType)
        }
        
        ggplotly(ggplot(shopData, aes(factor(Month), BounceRates)) +
                     geom_boxplot(aes(color = Revenue)) +
                     ggtitle("Month vs BounceRates "))
    })
    
    #Helper function to create Box Plots
    getggPlot <- function(y_var) {
        shopData <- shoppersIntDS
        if (input$varVisitorType != "All") {
            shopData <-
                shoppersIntDS %>% filter(VisitorType == input$varVisitorType)
        }
        ggplot(shopData, aes_string(x = "Revenue", y = y_var)) + geom_boxplot() +
            geom_jitter(aes(color = Revenue)) + ggtitle(paste0("BoxPlot for ", y_var))
    }
    
    #Box Plots
    output$administrativeDurationByRev <- renderPlotly({
        ggplotly(getggPlot("Administrative_Duration"))
    })
    
    output$informationalDurationByRev <- renderPlot(getggPlot("Informational_Duration"))
    output$productDurationByRev <- renderPlot(getggPlot("ProductRelated_Duration"))
    
    #Download box plots
    output$downAdministrativeDurationByRev <- downloadHandler(
        filename =  function() {
            "AdministrativeDurationByRev.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getggPlot("Administrative_Duration"))
            dev.off()  # turn the device off
            
        }
    )
    output$downInformationalDurationByRev <- downloadHandler(
        filename =  function() {
            "InformationalDurationByRev.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getggPlot("Informational_Duration"))
            dev.off()  # turn the device off
        }
    )
    output$downProductDurationByRev <- downloadHandler(
        filename =  function() {
            "ProductDurationByRev.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getggPlot("ProductRelated_Duration"))
            dev.off()  # turn the device off
        }
    )
    #Helper function to create Histograms.
    getHistPlot <- function(y_var) {
        shopData <- shoppersIntDS
        if (input$varVisitorType != "All") {
            shopData <-
                shoppersIntDS %>% filter(VisitorType == input$varVisitorType)
        }
        ggplot(shopData, aes_string(x = y_var, "..density..")) +
            geom_histogram(bins = 15) +
            ggtitle(paste0(y_var, " Histogram")) +
            ylab("Density") + geom_density(col = "red", lwd = 1)
    }
    
    #Histograms
    output$administrativeHist <- renderPlot(getHistPlot("Administrative"))
    
    output$informationalHist <- renderPlot(getHistPlot("Informational"))
    
    output$productRelatedHist <- renderPlot(getHistPlot("ProductRelated"))
    
    output$downAdministrativeHist <- downloadHandler(
        filename =  function() {
            "AdministrativeHist.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getHistPlot("Administrative"))
            dev.off()  # turn the device off
        }
    )
    
    output$downInformationalHist <- downloadHandler(
        filename =  function() {
            "InformationalHist.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getHistPlot("Informational"))
            dev.off()  # turn the device off
        }
    )
    
    output$downProductRelatedHist <- downloadHandler(
        filename =  function() {
            "ProductRelatedHist.png"
        },
        content = function(file) {
            png(file) # open the png device
            print(getHistPlot("ProductRelated"))
            dev.off()  # turn the device off
        }
    )
    
    #Fit logistic Regression
    output$logisticFitSummary<- renderPrint({

        #logisticModelFit <- glm(Revenue ~ ., data=shoppersIntDS, family="binomial")
        #summary(logisticModelFit)
        
        #Convert resonse "Revenue" as factor
        shoppersIntDS$Revenue <- factor(shoppersIntDS$Revenue)
        
        #Create train and test datasets
        train <- sample(1:nrow(shoppersIntDS), nrow(shoppersIntDS)*(input$sliderSubset/100))
        shoppersIntDS_train <- shoppersIntDS[train,]
        shoppersIntDS_test <- shoppersIntDS[-train,]
        
        #Fit logistic regression
        fit <- train(Revenue ~ ., data = shoppersIntDS_train, 
                     method = "glm", 
                     family = "binomial")
        fit$results$Accuracy
        summary(fit)
        
        
    })
    
    #Fit Classification Tree
    output$ClassificationTreeSummary<- renderPrint({
        set.seed(50)
        #logisticModelFit <- glm(Revenue ~ ., data=shoppersIntDS, family="binomial")
        #summary(logisticModelFit)
        
        #Convert resonse "Revenue" as factor
        shoppersIntDS$Revenue <- factor(shoppersIntDS$Revenue)
        
        #Create train and test datasets
        train <- sample(1:nrow(shoppersIntDS), nrow(shoppersIntDS)*(input$sliderSubset/100))
        shoppersIntDS_train <- shoppersIntDS[train,]
        shoppersIntDS_test <- shoppersIntDS[-train,]
        
        #Fit Classification Tree
        cTFit <- train(Revenue ~ ., data = shoppersIntDS_train,
                      method = "rpart",
                      trControl = trainControl(method = "repeatedcv",
                                               number = 10),
                      preProcess = c("center", "scale"))
                     # tuneGrid = data.frame(cp = seq(0.01, 0.3, by=0.01)))
        
        #fit$results$Accuracy
        cTFit
    
    })
    
    #Fit Classification Tree
    output$RFTreeSummary<- renderPrint({
        set.seed(50)
        #logisticModelFit <- glm(Revenue ~ ., data=shoppersIntDS, family="binomial")
        #summary(logisticModelFit)
        
        #Convert resonse "Revenue" as factor
        shoppersIntDS$Revenue <- factor(shoppersIntDS$Revenue)
        
        #Create train and test datasets
        train <- sample(1:nrow(shoppersIntDS), nrow(shoppersIntDS)*(input$sliderSubset/100))
        shoppersIntDS_train <- shoppersIntDS[train,]
        shoppersIntDS_test <- shoppersIntDS[-train,]
        
        #Fit logistic regression
        rfFit <- train( Revenue ~., data= head(shoppersIntDS,200),
                        method="rf",
                        trControl = trainControl(method="repeatedcv", number=2),
                        preProcess= c("center","scale"),
                        tuneGrid = data.frame(mtry = 2))
        rfFit
    })
    

    
})
