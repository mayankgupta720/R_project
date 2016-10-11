
library(shiny)
library(ggplot2)
library(plyr)
#install.packages("caret")
library(caret)
library(plyr)
library(markdown)
# 
#reading file
deptdata1<- read.csv("https://raw.githubusercontent.com/mayankgupta720/R_project/master/Walmart.csv", header=TRUE, sep=",")
names(deptdata1)
#summarise by store on basis of year and week
storesummary<-aggregate(deptdata1$Weekly_Sales, by=list(deptdata1$Store,deptdata1$Year,deptdata1$Week, deptdata1$Fuel_Price, deptdata1$Temperature, deptdata1$Unemployment, deptdata1$CPI),
                        FUN=sum)
names(storesummary)
storelookup<- read.csv("https://raw.githubusercontent.com/mayankgupta720/R_project/master/stores.csv", header=TRUE, sep=",")
#merge(x=storesummary,y=storelookup,by.x=Group.1, y=Store)
names(storesummary)[names(storesummary)=="Group.1"] <- "Store"
names(storesummary)[names(storesummary)=="Group.2"] <- "Year"
names(storesummary)[names(storesummary)=="Group.3"] <- "Week"
names(storesummary)[names(storesummary)=="Group.4"] <- "Fuel_Price"
names(storesummary)[names(storesummary)=="Group.5"] <- "Temperature"
names(storesummary)[names(storesummary)=="Group.6"] <- "Unemployment"
names(storesummary)[names(storesummary)=="Group.7"] <- "CPI"
names(storesummary)[names(storesummary)=="x"] <- "Sales"
names(storesummary)
unique.stores <- as.list(unique(storesummary$Store))
unique.years<- as.list(unique(storesummary$Year))
unique.months <- as.list(unique(storesummary$Month))
combinedData <- join(storesummary, storelookup, by='Store', type='left', match='all')
names(combinedData)
nrow(combinedData)


# URL1 <- getURL("https://raw.githubusercontent.com/mayankgupta720/R_project/master/CombinedData.csv")
# combinedData <- read.csv(text = URL1)
# names(combinedData)

#create data set for 2010
library(shiny)

ui1 <-  shinyUI(pageWithSidebar(
  
  #  Demo title
  headerPanel("Multiple Linear Regression"),
  
  sidebarPanel(
    
    h3("Training Data"),
    sliderInput("rseed", "Random Seed:", 
                min=1, max=100, value=27),
    br(),
    sliderInput("train", "Proportion of the data for the training [ % ]:", 
                min=1, max=100, value=80),
    
    br(),
    h3("Regression"),
    
    selectInput("regression", "Select Regression Model:",
                list("Sales ~ Week", 
                     "Sales ~ Week + Size",
                     "Sales ~ Week + Size + Temperature"
                     #"Sales ~ Week + Size + Temperature + Fuel Price",
                     #"Sales ~ Week + Size + Temperature + Fuel Price + Unemployment",
                     #"Sales ~ Week + Size + Temperature + Fuel Price + Unemployment + CPI" 
                     ),
                selected = "Sales ~ Week"),
    checkboxInput("constant", "Include Intercept", TRUE),
    
    
    h3("Plot"),
    checkboxInput("showdata", "Show Test Data", TRUE),
    
    checkboxInput("predict", "Show Regression Curve", TRUE),
    checkboxInput("resid", "Show Residuals", TRUE)
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel('Regression Model',
               #plotOutput("scatter"),
               #br(),
               h4("Statistics of the trained model:", style = "color:blue"),
               tableOutput("lmStats"),
               h4("Coefficients of the trained model:", style = "color:blue"),
               tableOutput("lmResults")
      ),
      
      tabPanel('Prediction Quality',
               #h3("Plot of residual error as a function of prediction", style = "color:orange"),
               #plotOutput("residuals"),
               h4(textOutput("R_sq_test"), style = "color:orange"),
               h4(textOutput("RMSE_test"), style = "color:orange"),
               h4(textOutput("R_sq_train"), style = "color:blue"),
               h4(textOutput("RMSE_train"), style = "color:blue")
               
      ),
      
      tabPanel('Training Data',
               h5(textOutput("datacaption")),
               #uiOutput("datacaption"),
               dataTableOutput('train')
               
      ),
      
      tabPanel('Test Data',
               h5(textOutput("datacaptionTest")),
               #uiOutput("datacaption"),
               dataTableOutput('test')
               
      )
    )
  )
)
)


input <- list(rseed=1)
data.maker <- function(seed=1) {
  set.seed(seed)
} 

server1 <- shinyServer(function(input, output) {
  mydata <- reactive({
    combinedData
    data.maker(seed=input$rseed)
  })
  
  trainData <- reactive({
    
    set.seed(input$rseed) ### Dodal
    trainIndex <- createDataPartition(y=combinedData$Sales   , p=input$train/100   , list = FALSE)
    training_data <- combinedData[trainIndex,]
    
  })
  
  testData <- reactive({
    set.seed(input$rseed) ### Dodal
    trainIndex <- createDataPartition(y=combinedData$Sales   , p=input$train/100   , list = FALSE)      
    testing_data <- combinedData[-trainIndex,]
  })
  
  output$train <- renderDataTable(trainData(), options = list(iDisplayLength = 10)) 
  output$test <- renderDataTable(testData(), options = list(iDisplayLength = 10)) 
  
  lmResults <- reactive({
    regress.exp <- input$regression
    if (!input$constant) regress.exp <- paste(input$regression, "- 1")
    
    
    set.seed(input$rseed) 
    trainIndex <- createDataPartition(y=combinedData$Sales   , p=input$train/100   , list = FALSE)
    trainData <- combinedData[trainIndex,]
    testData <- combinedData[-trainIndex,]
    
    lm(regress.exp, data=trainData)  
  }) 
  
  output$lmStats <- renderTable({
    results <- summary(lmResults())
    data.frame(R2=results$r.squared,
               adj.R2=results$adj.r.squared,
               DOF.model=results$df[1],
               DOF.available=results$df[2],
               DOF.total=sum(results$df[1:2]),
               f.value=results$fstatistic[1],
               f.denom=results$fstatistic[2],
               f.numer=results$fstatistic[3],
               p=1-pf(results$fstatistic[1],
                      results$fstatistic[2],
                      results$fstatistic[3]))
  })
  
  # Show coefficients
  
  output$lmResults <- renderTable(summary(lmResults()))
  
  # Show plot of points, regression line, residuals
  output$scatter <- renderPlot({
    
    set.seed(input$seed)     
    trainIndex <- createDataPartition(y=combinedData$Sales   , p=input$train/100   , list = FALSE) 
    #str(trainIndex)
    data1 <- combinedData[-trainIndex,] 
    x <- data1$Week
    y <- data1$Sales
    xcon <- seq(min(combinedData$Week)-.1, max(combinedData$Week)+.1, .025)
    x2 <- xcon^2
    
    
    predictor <- data.frame(x=xcon,x2=x2)
    
    regression <- paste0("Regression Model: ", input$regression)
    
    if(input$regression == "Sales ~ Week") { 
      
      regression <- expression(paste("Regression Model: ", combinedData$Sales, " ~ ", combinedData$Week))
      
    }
    
    if(input$regression == "Sales~Week2") { 
      
      regression <- expression(paste("Regression Model: ", combinedData$Sales, " ~ ", combinedData$Week^{2}))
      
    }
    
    yline <- predict(lmResults(), predictor)
    
    plot(c(min(combinedData$Week),max(combinedData$Week)) 
         ,c(min(combinedData$Sales,yline),max(combinedData$Sales,yline)), 
         type="n",
         xlab="Weeks",
         ylab="Sales",
         main=regression)
    
    if (input$resid) for (j in 1:length(combinedData$Week)) {
      
      yhat <- predict(lmResults(), data1[j,]) #Dodal
      lines(rep(x[j],2), c(yhat,combinedData$Sales[j]), col="red") #Namesto yhat sem dal yline
      
    } 
    
    if (input$showdata) points(combinedData$Week,combinedData$Sales)
    
    if (input$predict) lines(xcon, yline, lwd=2, col="blue") #Namesto lines funkcije sem dal funcijo points
  })
  
  
  
  output$residuals <- renderPlot({
    
    dTest <- testData()
    dTest$y_pred <- predict(lmResults(), newdata=dTest)
    
    
    ggplot(data=dTest,aes(x=y_pred, y=y_pred-combinedData$Sales)) +
      geom_point(alpha=0.2,color="black") +
      geom_smooth(aes(x=y_pred,  y = y_pred - combinedData$Sales),  color="black")
    
    
  })
  
  
  rsq <- function(y,f) { 1 - sum((combinedData$Sales-f)^2)/sum((combinedData$Sales-mean(combinedData$Sales))^2) }
  rmse <- function(y, f) { sqrt(mean( (combinedData$Sales-f)^2 )) }
  
  
  
  output$R_sq_test <- renderText(paste0("R-squared on test data: ",
                                        round(rsq(testData()$Sales,predict(lmResults(),
                                                                           newdata=testData())),2)))
  
  
  output$RMSE_test <- renderText(paste0("RMSE on test data: ",
                                        round(rmse(testData()$Sales,predict(lmResults(),
                                                                            newdata=testData())),2)))
  
  
  output$R_sq_train <- renderText(paste0("R-squared on train data: ",
                                         round(rsq(trainData()$Sales,predict(lmResults(),
                                                                             newdata=trainData())),2)))
  
  
  output$RMSE_train <- renderText(paste0("RMSE on train data: ",
                                         round(rmse(trainData()$Sales,predict(lmResults(),
                                                                              newdata=trainData())),2)))   
  
}
)

shinyApp (server = server1, ui = ui1)

#names(data1)