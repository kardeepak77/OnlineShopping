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
    output$administrativeDurationByRev <- renderPlot({
        getggPlot("Administrative_Duration")
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
    
    userInput <- reactive({
        # Assign values to corresponding variables
        i <- 0
        tmp_values <- c()
        for (x in input$colsForModel) {
            i <- i+1
            tmp_values[i] <- eval(parse(text=paste0("input$select",x)))
        }
        tmp_values
    })
    

    output$predictionOutput <- renderPrint({
        df <- as.data.frame(t(userInput()))
        colnames(df) <- input$colsForModel
       
     
        if(input$varVisitorType == "Logistic regression") {
            finalPred <- predict((logisticFitSumm()["CTReeModel"])$CTReeModel, df)
            finalPred
        } else if(input$varVisitorType == "Logistic regression") {
            finalPred <- predict((logisticFitSumm()["RFModel"])$RFModel, df)
            finalPred
        } else {
            finalPred <- predict((logisticFitSumm()["LRModel"])$LRModel, df)
            finalPred
            }
        
        

    })
    
    logisticFitSumm <- eventReactive(input$runModelsButton, {
        
        set.seed(50)
        #Convert resonse "Revenue" as factor
        shoppersIntDS$Revenue <- factor(shoppersIntDS$Revenue)
        
        #Create train and test datasets
        train <- sample(1:nrow(shoppersIntDS), nrow(shoppersIntDS)*(input$sliderSubset/100))
        shoppersIntDS_train <- shoppersIntDS[train,]
        shoppersIntDS_test <- shoppersIntDS[-train,]
        #Revenue ~ !!!input$colsForModel , data = shoppersIntDS_train,
        
        #Fit logistic regression 
        trainModelLog <- train(shoppersIntDS_train %>% dplyr::select(!!!input$colsForModel),
              shoppersIntDS_train[,"Revenue"],
              method = "glm", 
              family = "binomial")
        
    
        
        #Fit Classification Tree
        trainModelCTree <- train(shoppersIntDS_train %>% dplyr::select(!!!input$colsForModel),
              shoppersIntDS_train[,"Revenue"],
                       method = "rpart",
                       trControl = trainControl(method = "repeatedcv",
                                                number = 10),
                       preProcess = c("center", "scale"))
        
        
        #Fit logistic regression

        trainModelRF <- train( shoppersIntDS_train %>% dplyr::select(!!!input$colsForModel),
               shoppersIntDS_train[,"Revenue"],
                        method="rf",
                        trControl = trainControl(method="repeatedcv", number=2),
                        preProcess= c("center","scale"),
                        tuneGrid = data.frame(mtry = input$varmtry))
        
        #Prediction on Test set
        prediction <- predict(trainModelLog, newdata = shoppersIntDS_test)
        logPredOutPut <- postResample(pred = prediction, obs = shoppersIntDS_test$Revenue)
        
        prediction <- predict(trainModelCTree, newdata = shoppersIntDS_test)
        CTreePredOutPut <- postResample(pred = prediction, obs = shoppersIntDS_test$Revenue)
        
        prediction <- predict(trainModelRF, newdata = shoppersIntDS_test)
        RFPredOutPut <- postResample(pred = prediction, obs = shoppersIntDS_test$Revenue)
        
        bestPredModel <- which.max(c(as.numeric(logPredOutPut[1]), as.numeric(CTreePredOutPut[1]), as.numeric(RFPredOutPut[1])))
        
        print(bestPredModel)
        list(LRModel = trainModelLog, CTReeModel = trainModelCTree, RFModel = trainModelRF, 
             logPred = logPredOutPut, CTReePred = CTreePredOutPut, RFPred = RFPredOutPut, bestModel = bestPredModel)
        
    })
    
    output$logisticFitSummary<- renderPrint({
        logisticFit <- logisticFitSumm()["LRModel"]
        list("Fit Statistics: Accuracy" = logisticFit$LRModel$results$Accuracy, "Model Summary"= logisticFit$LRModel , "Prediction Results" = logisticFitSumm()["logPred"])
    })
    
    output$ClassificationTreeSummary<- renderPrint({
        cTFit <- logisticFitSumm()["CTReeModel"]
        list("Fit Statistics: Accuracy" = cTFit$CTReeModel$results$Accuracy[1], "Model Summary"= cTFit$CTReeModel, "Prediction Results" = logisticFitSumm()["CTReePred"])
    })
    
    output$RFTreeSummary<- renderPrint({
        rfFit <- logisticFitSumm()["RFModel"]
        list("Fit Statistics: Accuracy" = rfFit$RFModel$results$Accuracy, "Model Summary"= rfFit$RFModel, "Prediction Results" = logisticFitSumm()["RFPred"])
    })
    
    output$TestSummary<- renderPrint({
        bModel <- logisticFitSumm()
        bModelText <- switch(bModel$bestModel,"Logistic regression","Classification Tree","Random Forest")
        paste0("Best Performing Model is : ", bModelText)
    })
    
    output$mtryInput <- renderUI({
        if(as.numeric(length(input$colsForModel)) > 0)
            numericInput("varmtry", "Radom Forest Tunning Parameter mtry:", 1, min = 1, max = as.numeric(length(input$colsForModel)))
    })
    
})
