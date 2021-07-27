#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)
library(shinythemes)
#library(bs4Dash)

shinyUI(
    navbarPage(strong("Shopping Intention"), theme = shinytheme("united"),
               
               tabPanel("About", 
                        fluidPage(
                            fluidRow(
                    
                                column(8, 
                                       tags$div(
                                           h2("Online Shoppers Purchasing Intention"), 
                                           tags$body("E-commerce has substantial role in today’s economy. Businesses in this sector spend large amount in marketing to attract visitors to their shopping sites. Marking in such businesses has primary need to understand what factors influence a visitor to transform into a purchaser. In this study, my aim is to build a machine learning models to predict prospective customer among visitors to an online shopping portal.")
                                           ),
                                       tags$div(
                                           h3("Source"), 
                                           fluidRow(
                                               column( width=6,
                                                       strong("C. Okan Sakar"), br(),
                                                       tags$body("Department of Computer Engineering, Faculty of"), br(),
                                                       tags$body("Engineering and Natural Sciences, Bahcesehir University,"),br(),
                                                       tags$body("34349 Besiktas, Istanbul, Turkey"),
                                               ),
                                               column( width=6,
                                                       strong("Yomi Kastro"), br(),
                                                       tags$body("Inveon Information Technologies Consultancy and Trade,"), br(),
                                                       tags$body("34335 Istanbul, Turkey"),
                                               )
                                           )
                                       ),
                                       ),
                                column(4, tags$img(src="https://www.feedyes.com/wp-content/uploads/2019/02/word-image-1.png", width="80%"),
                                ),
                                ),
                            
                            
                            
                            
                            tags$div(
                                h3("Attribute Information"), 
                                tags$body("The dataset consists of 10 numerical and 8 categorical attributes."),
                                br(),
                                tags$body("The 'Revenue' attribute can be used as the class label."),
                                br(),br(),
                                tags$body(strong("Administrative"), " , ", strong("Administrative Duration"), " , ", strong( "Informational")," , ", strong("Informational Duration"), " , ", strong("Product Related"), "and", strong("Product Related Duration"), 
                                "represent the number of different types of pages visited by the visitor in that session and total time spent in each of these page categories."), 
                                tags$p("The values of these features are derived from the URL information of the pages visited by the user and updated in real time when a user takes an action, e.g. moving from one page to another. "),

                                tags$body("The ", strong("Bounce Rate"), ", ", strong("Exit Rate")," and ", strong("Page Value"),
                                          " features represent the metrics measured by ", strong("Google Analytics"), " for each page in the e-commerce site."), 
                                tags$p("The value of ", strong("Bounce Rate")," feature for a web page refers to the percentage of visitors who enter the site ",
                                "from that page and then leave (", strong("bounce"),") without triggering any other requests to the analytics server during that session.",
                                br(),"The value of ", strong("Exit Rate"), " feature for a specific web page is calculated as for all pageviews to the page,", 
                                       "the percentage that were the last in the session. The ", strong("Page Value"), " feature represents the average value for ",
                                       "a web page that a user visited before completing an e-commerce transaction."),
                                
                                tags$p("The ", strong("Special Day"), " feature indicates the closeness of the site visiting time to a specific special day",
                                          " (e.g. Mother’s Day, Valentine's Day) in which the sessions are more likely to be finalized with transaction. ",
                                          "The value of this attribute is determined by considering the dynamics of e-commerce such as the duration between ",
                                          "the order date and delivery date. For example, for Valentina’s day, this value takes a nonzero value between ", 
                                          "February 2 and February 12, zero before and after this date unless it is close to another special day, and its ",
                                          "maximum value of 1 on February 8."),
                                tags$body("The dataset also includes operating system, browser, region, traffic type, visitor type as returning or new visitor, a Boolean value indicating whether the date of the visit is weekend, and month of the year."),
                                
                            ),
                            a(href="https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset#",
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
                                tags$ul(
                                    tags$li("Perform prediction using model of your choice")
                                )
                            
                        ),
                    ),
                ),
             
               tabPanel("Data", tabName = "dataTab"),
               tabPanel("Data Exploration", tabName = "dataexpTab"),
               tabPanel("Modeling", tabName = "modelTab"),
               tabPanel("Prediction", tabName = "predTab"),
               tabPanel("Plot",
                        fluidPage(
                            #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
                            sidebarPanel(
                                textInput("txt", "Text input:", "text here"),
                                sliderInput("slider", "Slider input:", 1, 100, 30),
                                actionButton("action", "Button"),
                                actionButton("action2", "Button2", class = "btn-primary")
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Tab 1"),
                                    tabPanel("Tab 2")
                                )
                            )
                        )
               )
               
    )
)