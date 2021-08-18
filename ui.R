#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(plotly)
#library(shinycssloaders)

#Load data
shoppersIntDS<- read.csv("data/online_shoppers_intention.csv")
#Month data has June has incorrect abbrevation. Fix that and create factor for month.
shoppersIntDS[shoppersIntDS$Month == "June", ]$Month = "Jun"
shoppersIntDS$Month <- factor(shoppersIntDS$Month, levels = c("Feb", "Mar","May","Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec") )

shoppersIntDS$VisitorType <- factor(shoppersIntDS$VisitorType)
shoppersIntDS$Weekend <- factor(shoppersIntDS$Weekend)

#month.abb
#predvar<-levels (as.factor( mbl_pitching %>% filter(yearID==2015) %>% select(teamID)))


shinyUI(
    navbarPage(
        strong("Shopping Intention"),
        theme = shinytheme("united"),
        
        tabPanel("Data Exploration",
                 fluidPage(
                     tags$div(
                         h2("Exploratory analysis"),
                         tags$body("This page allow the user to browse throughthe whole
                                    dataset. Select columns, filter rows using column values as needed"),
                     ),
                     fluidRow(
                         column(6, withSpinner(tableOutput("visitorTypeByRev"))),
                         column(6, withSpinner(tableOutput("visitorTypeByWeekend")))
                         
                     ),
                     tags$div(selectInput("varVisitorType", "Select the Visitor Type", 
                                          selected = 1,
                                          choices = c("All",levels(as.factor(shoppersIntDS$VisitorType))))),
                     fluidRow(
                         #column(4, tags$div(dataTableOutput("summ"))),
                         column(12, tags$div(withSpinner(plotlyOutput("monthRevBox")))),
                         #column(3, tags$div(dataTableOutput("summ")))
                     ),
                     br(),
                     fluidRow(
                                column(4, align="center", tags$div(withSpinner(plotOutput("administrativeDurationByRev")), downloadButton(outputId = "downAdministrativeDurationByRev", label = "Download the plot"))),
                                column(4, align="center", tags$div(withSpinner(plotOutput("informationalDurationByRev")), downloadButton(outputId = "downInformationalDurationByRev", label = "Download the plot"))),
                                column(4, align="center", tags$div(withSpinner(plotOutput("productDurationByRev")), downloadButton(outputId = "downProductDurationByRev", label = "Download the plot")))
                         ),
                     br(),
                         fluidRow(
                             column(4, align="center", tags$div(withSpinner(plotOutput("administrativeHist")),downloadButton(outputId = "downAdministrativeHist", label = "Download the plot"))),
                             column(4, align="center", tags$div(withSpinner(plotOutput("informationalHist")),downloadButton(outputId = "downInformationalHist", label = "Download the plot"))),
                             column(4, align="center", tags$div(withSpinner(plotOutput("productRelatedHist")),downloadButton(outputId = "downProductRelatedHist", label = "Download the plot")))
                         ),
                     fluidRow(
                         tags$div(dataTableOutput("summ")),
                         #column(12, tags$div(plotlyOutput("monthRevBox"))),
                         #column(3, tags$div(dataTableOutput("summ")))
                     ),
                     
                     
                 )
        ),
        tabPanel("About",
                 fluidPage(
                     fluidRow(column(
                         8,
                         tags$div(
                             h2("Online Shoppers Purchasing Intention"),
                             tags$body(
                                 "E-commerce has substantial role in today’s economy. Businesses in this sector spend large amount in marketing to attract visitors to their shopping sites. Marking in such businesses has primary need to understand what factors influence a visitor to transform into a purchaser. In this study, my aim is to build a machine learning models to predict prospective customer among visitors to an online shopping portal."
                             )
                         ),
                         tags$div(h3("Source"),
                                  fluidRow(
                                      column(
                                          width = 6,
                                          strong("C. Okan Sakar"),
                                          br(),
                                          tags$body("Department of Computer Engineering, Faculty of"),
                                          br(),
                                          tags$body("Engineering and Natural Sciences, Bahcesehir University,"),
                                          br(),
                                          tags$body("34349 Besiktas, Istanbul, Turkey"),
                                      ),
                                      column(
                                          width = 6,
                                          strong("Yomi Kastro"),
                                          br(),
                                          tags$body("Inveon Information Technologies Consultancy and Trade,"),
                                          br(),
                                          tags$body("34335 Istanbul, Turkey"),
                                      )
                                  )),
                     ),
                     column(
                         4,
                         tags$img(src = "https://www.feedyes.com/wp-content/uploads/2019/02/word-image-1.png", width =
                                      "80%"),
                     ), ),
                     
                     tags$div(
                         h3("Attribute Information"),
                         tags$body(
                             "The dataset consists of 10 numerical and 8 categorical attributes."
                         ),
                         br(),
                         tags$body("The 'Revenue' attribute can be used as the class label."),
                         br(),
                         br(),
                         tags$body(
                             strong("Administrative"),
                             " , ",
                             strong("Administrative Duration"),
                             " , ",
                             strong("Informational"),
                             " , ",
                             strong("Informational Duration"),
                             " , ",
                             strong("Product Related"),
                             "and",
                             strong("Product Related Duration"),
                             "represent the number of different types of pages visited by the visitor in that session and total time spent in each of these page categories."
                         ),
                         tags$ul(
                             tags$li(
                                 "The values of these features are derived from the URL information of the pages visited by the user and updated in real time when a user takes an action, e.g. moving from one page to another. "
                             ),
                         ),
                         
                         tags$body(
                             "The ",
                             strong("Bounce Rate"),
                             ", ",
                             strong("Exit Rate"),
                             " and ",
                             strong("Page Value"),
                             " features represent the metrics measured by ",
                             strong("Google Analytics"),
                             " for each page in the e-commerce site."
                         ),
                         tags$ul(
                             tags$li(
                                 "The value of ",
                                 strong("Bounce Rate"),
                                 " feature for a web page refers to the percentage of visitors who enter the site ",
                                 "from that page and then leave (",
                                 strong("bounce"),
                                 ") without triggering any other requests to the analytics server during that session."
                             ),
                             tags$li(
                                 "The value of ",
                                 strong("Exit Rate"),
                                 " feature for a specific web page is calculated as for all pageviews to the page,",
                                 "the percentage that were the last in the session."
                             ),
                             tags$li(
                                 "The ",
                                 strong("Page Value"),
                                 " feature represents the average value for ",
                                 "a web page that a user visited before completing an e-commerce transaction."
                             ),
                         ),
                         
                         tags$p(
                             "The ",
                             strong("Special Day"),
                             " feature indicates the closeness of the site visiting time to a specific special day",
                             " (e.g. Mother’s Day, Valentine's Day) in which the sessions are more likely to be finalized with transaction. ",
                             "The value of this attribute is determined by considering the dynamics of e-commerce such as the duration between ",
                             "the order date and delivery date. For example, for Valentina’s day, this value takes a nonzero value between ",
                             "February 2 and February 12, zero before and after this date unless it is close to another special day, and its ",
                             "maximum value of 1 on February 8."
                         ),
                         tags$body(
                             "The dataset also includes operating system, browser, region, traffic type, visitor type as returning or new visitor, a Boolean value indicating whether the date of the visit is weekend, and month of the year."
                         ),
                         
                     ),
                     a(href = "https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset#",
                       "More Information"),
                     
                     tags$div(
                         h3("App's Menu Usage"),
                         strong("1. About"),
                         tags$ul(
                             tags$li("Describes the purpose of the app"),
                             tags$li("Briefly discuss the data and its source"),
                             tags$li("Describes the purpose of each menu of the app")
                         ),
                         strong("2. Data"),
                         tags$ul(
                             tags$li("Browse dataset"),
                             tags$li("Subset data"),
                             tags$li("Export data")
                         ),
                         strong("3. Data Exploration"),
                         tags$ul(
                             tags$li("Create interactive charts and download as required."),
                             tags$li("Filter Chart data")
                         ),
                         strong("4. Modeling"),
                         tags$ul(
                             tags$li("Modeling Info Tab: Descibes Modeling approaches."),
                             tags$li("Model Fitting: Analyize model fits and best model selection")
                         ),
                         strong("5. Prediction"),
                         tags$ul(tags$li(
                             "Perform prediction using model of your choice"
                         ))
                         
                     ),
                 ), ),
        
        tabPanel("Data",
                 fluidPage(
                     tags$div(
                         h2("Dataset"),
                         tags$body("This page allow the user to browse throughthe whole
                                    dataset. Select columns, filter rows using column values as needed")
                     ),
                     
                     tags$div( 
                         br(),
                         fluidRow(column(6, varSelectInput("cols", "Select Columns", shoppersIntDS, multiple = T)),
                                  column(6, downloadButton("downloadData", "Download", class = "btn-primary" )))
                         
                     ),
                     fluidRow(
                         column(12, dataTableOutput("shopperIntDT"))
                     )
                     # tags$div(
                     #     
                     #     dataTableOutput("shopperIntDT")
                     # )
                     
                 )),
        
        #tabPanel("Modeling", tabName = "modelTab"),
        tabPanel("Modeling", 
                 fluidPage(
                     fluidPage(
                         tabsetPanel(
                             tabPanel(
                                 "Modeling Info",
                                 br(),
                                 tags$div("We will analyze 3 modeling approaches to predict with given online session will generate revenue or not."),
                                 br(),
                                 tags$div(
                                     strong("1. Logistic Regression"),
                                     tags$div("Logistic regression is type of Generalized Linear Regression Model and is type of supervised learning method. Generalized Linear Models allows response from non-normal distributions and support both categorical and continuous predictors. Logistic regression is used when response is binary, such as success / failure. Logistic Regression models success probability using the logistic function"),
                                     tags$div("This function never goes below zero and above 1."),
                                     
                                     tags$div(
                                     withMathJax(),
                                     h3("Formula"),
                                     
                                     p("\\(\\log(\\frac{P}{1-P})=\\beta_0+\\beta_1*x_1+...+\\beta_p*x_p\\)"),
                                     "where, P is sucess probability, ", 
                                     span("\\(x_i\\)")," predictors,",
                                     span("\\(\\beta_0\\)")," the intercept,",
                                     span("\\(\\beta_i\\)"),"slops"),
                                     
                                     tags$div("This shows logit (log-odds) of success is linear in the parameters. Such logit function is also called a link function."),
                                     strong("Advantages:"),
                                     tags$ul(
                                         tags$li("Allows modeling binary response which is non-normal distribution."),
                                         tags$li("Model is simple to understand and is interpretable."),
                                         tags$li("Coefficients/weights of model have meaning and helps understand how prediction is made.")
                                     ),
                                     strong("Disadvantages:"),
                                     tags$ul(
                                         tags$li("Logistic regression assumes that independent variables are linearly related with link function (log odds) of dependent variable."),
                                         tags$li("Logistic regression requires no or minimal collinearity between independent variables."),
                                         tags$li("Compared to Classification Tree/ Random forest techniques, logistic regression requires manual careful selection of independent variables.")
                                     ),
                                     strong("2. Classification Tree"),
                                     tags$div("Classification Tree is a Tree based supervised learning method, in which predictor space is split into regions, and different predictions are made for each region. Predictions are made by identifying most prevalent class for a given region. For optimizing regions, Gini index or deviance is used.  Tree fitting is done by minimizing these majors."),
                                     strong("Advantages:"),
                                     tags$ul(
                                         tags$li("Simple to understand and easy to interpret output"),
                                         tags$li("Predictors doesn’t require scaling"),
                                         tags$li("No statistical assumptions are necessary"),
                                         tags$li("Has built in variable selection")
                                     ),
                                     strong("Disadvantages:"),
                                     tags$ul(
                                         tags$li("Small change in data results in vastly different fitted trees"),
                                         tags$li("Trees usually requires pruning"),
                                         tags$li("Require Greedy-algorithm")
                                     ),
                                     strong("3.	Random Forest"),
                                     tags$div("Random Forest is tree based supervised learning method and uses Ensemble Learning technique. It uses same idea as bagging, where multiple trees are used to predict response and results are averaged. However, in Random Forest randomly selected subset of predictors are used for created multiple trees. Such random selection or predictors avoids strong predictors overpowering prediction. It also make bagged tree predictors more correlated."),
                                     strong("Advantages:"),
                                     tags$ul(
                                         tags$li("Feature scaling is not required in case for Random forest."),
                                         tags$li("Random forest can handle linear as well as non-linear parameters."),
                                         tags$li("Random forest automatically takes care of outliers."),
                                         tags$li("Compared to classification tree, Random Forest Is less impacted even if new data points are introduced in dataset.")
                                     ),
                                     strong("Disadvantages:"),
                                     tags$ul(
                                         tags$li("As Random Forest creates lot of trees, it’s computation resource heavy and takes longer to train compared to classification tree."),
                                         tags$li("As Random forest uses many trees for prediction, prediction interpretability is lost."),
                                     ),
                                 ),
                                 ),
                             tabPanel("Model Fitting",
                                      fluidPage(
                                          fluidRow(
                                          sliderInput("sliderSubset", "% Of Dataset to use for Training:", 50, 90, 70, step=5),
                                          tags$body("Restricting use to use more than 50% but less than 90% of data as Training, to ensure we have enough data for training & prediction."),
                                          varSelectInput("colsForModel", "Select Columns", shoppersIntDS %>% select(-Revenue), multiple = T),
                                          #textInput("txt", "Text input:", "text here"),
                                          uiOutput("mtryInput"),
                                          #numericInput("varmtry", "mtry:", 1, min = 1, max = "length(input.colsForModel)"),
                                          #actionButton("action", "Button"),
                                          actionButton("runModelsButton", "Run Models", class = "btn-primary"), br(),
                                          #verbatimTextOutput ("logisticFitSummary"),
                                          #verbatimTextOutput ("ClassificationTreeSummary")
                                          br(),
                                          verbatimTextOutput ("TestSummary")
                                          ),
                                          fluidRow(
                                              column(4,
                                                     h3("Logistic Regression"),
                                                     withSpinner(verbatimTextOutput ("logisticFitSummary"))),
                                              column(4,
                                                     h3("Classification Tree"),
                                                     withSpinner(verbatimTextOutput ("ClassificationTreeSummary"))),
                                              column(4,
                                                     h3("Random Forest"),
                                                     withSpinner(verbatimTextOutput ("RFTreeSummary"))),
                                          )
                                      )
                                      ),
                             tabPanel("Prediction",
                                      sidebarLayout(
                                          sidebarPanel(
                                              #conditionalPanel(condition="input.action=='original dataset'", 
                                                               conditionalPanel(condition="input.colsForModel.includes('Administrative')",
                                                                                sliderInput("selectAdministrative", "Administrative:", 
                                                                                            min = 0, max = 27, 
                                                                                            value = 4, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Administrative_Duration')",
                                                                                sliderInput("selectAdministrative_Duration", "Administrative_Duration:", 
                                                                                            min = 0., max = 3400, 
                                                                                            value = 10, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Informational')",
                                                                                sliderInput("selectInformational", "Informational:", 
                                                                                            min = 0., max = 24, 
                                                                                            value = 11, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Informational_Duration')",
                                                                                sliderInput("selectInformational_Duration", "Informational_Duration:", 
                                                                                            min = 0., max = 2550, 
                                                                                            value = 0, step = 1)             
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('ProductRelated')",
                                                                                sliderInput("selectProductRelated", "ProductRelated:", 
                                                                                            min = 0., max = 705, 
                                                                                            value = 1, step = 0.1)                 
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('ProductRelated_Duration')",
                                                                                sliderInput("selectProductRelated_Duration", "ProductRelated_Duration:", 
                                                                                            min = 2, max = 63974, 
                                                                                            value = 6, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('BounceRates')",
                                                                                sliderInput("selectBounceRates", "BounceRates:", 
                                                                                            min = 0, max = 0.2, 
                                                                                            value = 0.1, step = 0.01)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('ExitRates')",
                                                                                sliderInput("selectExitRates", "ExitRates:", 
                                                                                            min = 0, max = 0.2, 
                                                                                            value = 0.1, step = 0.01)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('PageValues')",
                                                                                sliderInput("selectPageValues", "PageValues:", 
                                                                                            min = 1, max = 362, 
                                                                                            value = 200, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('SpecialDay')",
                                                                                sliderInput("selectSpecialDay", "SpecialDay:", 
                                                                                            min = 0, max = 1, 
                                                                                            value = 1, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Month')",
                                                                                selectInput("selectMonth", "Month:", 
                                                                                            selected = 1,
                                                                                            choices = levels(shoppersIntDS$Month))
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('OperatingSystems')",
                                                                                sliderInput("selectOperatingSystems", "OperatingSystems:", 
                                                                                            min =1, max = 8, 
                                                                                            value = 1, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Browser')",
                                                                                sliderInput("selectBrowser", "Browser:", 
                                                                                            min = 1, max = 13, 
                                                                                            value = 1, step = 50)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Region')",
                                                                                sliderInput("selectRegion", "Region:", 
                                                                                            min = 1., max = 9, 
                                                                                            value = 2, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('TrafficType')",
                                                                                sliderInput("selectTrafficType", "TrafficType:", 
                                                                                            min = 1, max = 20, 
                                                                                            value = 1, step = 1)
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('VisitorType')",
                                                                                #sliderInput("selectVisitorType", "VisitorType:", 
                                                                                #            min = 0., max = 400, 
                                                                                #            value = 350, step = 50)
                                                                                selectInput("selectVisitorType", "VisitorType", 
                                                                                            selected = 1,
                                                                                            choices = levels(shoppersIntDS$VisitorType))
                                                               ),
                                                               conditionalPanel(condition="input.colsForModel.includes('Weekend')",
                                                                                #sliderInput("selectWeekend", "Weekend:", 
                                                                                #            min = 0., max = 400, 
                                                                                #            value = 350, step = 50)
                                                                                selectInput("selectWeekend", "Weekend:", 
                                                                                            selected = 1,
                                                                                            choices = levels(shoppersIntDS$Weekend))
                                                               ),
                            
                                              #)
                                          ),
                                          mainPanel(
                                              tags$div("Please Note: Model Fitting is pre-requist for performing Predictions."),
                                              br(),
                                              tags$div(selectInput("varModelSelector", "Select the Model For Prediction", 
                                                                   selected = 1,
                                                                   choices = c("Logistic regression","Classification Tree","Random Forest"))),
                                              verbatimTextOutput("predictionOutput")
                                              )
                                      )
                                      
                                      
                                      
                                      
                                      
                                      )
                             )
                         )
                     )
                 )
        # tabPanel("Plot",
        #          fluidPage(
        #              sidebarPanel(
        #                  textInput("txt", "Text input:", "text here"),
        #                  sliderInput("slider", "Slider input:", 1, 100, 30),
        #                  actionButton("action", "Button"),
        #                  actionButton("action2", "Button2", class = "btn-primary")
        #              ),
        #              mainPanel(tabsetPanel(tabPanel("Tab 1"),
        #                                    tabPanel("Tab 2")))
        #          )
        #          )
        
    )
)