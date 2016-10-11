# #reading file
# workingdata<- read.csv(file="C:\\Users\\gupta\\OneDrive\\MS BAIM\\Using R for Analytics\\Project\\workingdata.csv", header=TRUE, sep=",")
# tail(workingdata)
# names(workingdata)
# dim(workingdata)

# combinedData <- read.csv("C:\\Users\\gupta\\OneDrive\\MS BAIM\\Using R for Analytics\\Project\\combinedData.csv", header=TRUE, sep=",")
# names(combinedData)

#aggregate based on two dimensions
# 
# year_month_store_dept_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ year+month+Store+Dept
#                                      , data=workingdata, FUN=mean, row.names = FALSE)
# 
# year_month_store_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ year+month+Store
#                                      , data=workingdata, FUN=mean, row.names = FALSE)
# 
# month_store_dept_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ month+Store+Dept
#                                  , data=workingdata, FUN=mean, row.names = FALSE)


# month_store_dept_sales=month_store_dept_sales[order(month_store_dept_sales$Store,
#                                                           month_store_dept_sales$Dept,
#                                                           month_store_dept_sales$month),]
#


library(RCurl)
URL1 <- getURL("https://raw.githubusercontent.com/mayankgupta720/R_project/master/CombinedData.csv")
combinedData <- read.csv(text = URL1)
URL2 <- getURL("https://raw.githubusercontent.com/mayankgupta720/R_project/master/DataMarkdown.csv")
datamarkdown <- read.csv(text = URL2)
names(combinedData)
dim(datamarkdown)
URL3 <- getURL("https://raw.githubusercontent.com/mayankgupta720/R_project/master/workingdata.csv")
workingdata <- read.csv(text = URL3)
dim(workingdata)



dim(workingdata)
tail(month_store_dept_sales)
tail(year_month_store_sales)
tail(year_month_store_dept_sales)
#unique(year_month_store_sales$year)
#unique(year_month_store_sales$month)

library(shiny)
library(ggplot2)
#install.packages("DT")
library(DT)
ui = navbarPage("Walmart: Sales Analysis",  
                # Give the page a title
                tabPanel("Monthly Performance"
                         ,
                         # Generate a row with a sidebar
                         sidebarLayout(      
                           # Define the sidebar with one input
                           sidebarPanel(
                             selectInput(inputId = "store"
                                         , label = "Store:"
                                         , choices=unique(as.character(month_store_dept_sales$Store)))
                             ,
                             selectInput(inputId = "dept"
                                         ,label = "Department:"
                                         ,choices=unique(as.character(month_store_dept_sales$Dept))),
                             hr()
                             ,
                             helpText("Select Store and the Department for which you want to know the sales")
                           )
                           ,
                           
                           # Create a spot for the barplot
                           mainPanel(
                             plotOutput("MonthlySalesPlot"),plotOutput("MonthlyTempPlot"),
                             verbatimTextOutput("stats")
                           )
                         )
                )
                
                , tabPanel("Sales by month and year",
                           # Generate a row with a sidebar
                           sidebarLayout(      
                             # Define the sidebar with one input
                             sidebarPanel(
                               selectInput(inputId = "month"
                                           , label = "Month:"
                                           , choices=c("All",unique(year_month_store_sales$month)))
                               
                               ,selectInput(inputId = "year"
                                            ,label = "Year:"
                                            ,choices=c("All",unique(year_month_store_sales$year)))
                               ,hr()
                               ,helpText("Select the month and the year for which you want to know the sales")
                             )
                             # Create a spot for the barplot
                             ,mainPanel(
                               DT::dataTableOutput("Sales_table") 
                             )
                           )
                )
#deepti part
               ,
                tabPanel("Sales by Store-Type",

                         # Application title
                         titlePanel(title= h4("Weekly Walmart Sales Dashboard",align="center")),

                         # Sidebar with a slider input for number of bins
                         sidebarLayout(
                           sidebarPanel(
                             #Walmart Logo
                             #img(src="C:\\Users\\Deepti\\Documents\\R\\walmart.png", height = 100, width = 250),

                             # Copy the line below to make a select box
                             selectInput("type_deepti", label = h3("Select Store Type"),
                                         choices = c("A","B","C")),
                             selectInput("dept_deepti", label = h3("Select Department"),
                                         choices = unique(combinedData$Department)),
                             selectInput("year_deepti", label = h3("Select Year"),
                                         choices = unique(combinedData$Year))


                           ),
                           # Show a plot
                           mainPanel(

                             plotOutput("plot"),
                             h4("Summary"),
                             verbatimTextOutput("summary")

                           ))
                ) ,

                tabPanel("MarkDown Effect",

                         # Application title
                         titlePanel(title= h4("Sales variation with Markdown",align="center")),

                         # Sidebar with a slider input for number of bins
                         sidebarLayout(
                           sidebarPanel(
                             # Copy the line below to make a select box
                             selectInput("Type", label = h3("Select Store Type"),
                                         choices = c("A","B","C")),
                             selectInput("dept", label = h3("Select Department"),
                                         choices =unique(datamarkdown$Dept))


                           ),
                           # Show a plot
                           mainPanel(
                             h4("Sales variation with Markdown1"),
                             plotOutput("plot1"),
                             h4("Sales variation with Markdown2"),
                             plotOutput("plot2"),
                             h4("Sales variation with Markdown3"),
                             plotOutput("plot3"),
                             h4("Sales variation with Markdown4"),
                             plotOutput("plot4"),
                             h4("Sales variation with Markdown5"),
                             plotOutput("plot5")
                           ))
                ),
                tabPanel("Sales by Store Size",

                         # Show a plot
                         mainPanel(
                           selectInput("year1", label = h3("Select Year"),
                                       choices = unique(combinedData$Year)),
                           selectInput("dept1",label=h3("Select Department"), choices=unique(combinedData$Department)),
                           # h4("Variation of Sales by Store Size"),
                           plotOutput(outputId = "main_plot")

                         ))


                
                
                
                )



server = function(input, output) {
  
  # Fill in the spot we created for a plot
  output$MonthlySalesPlot <- renderPlot({
    # Render a barplot
    
    data1 = month_store_dept_sales[month_store_dept_sales$Store==input$store,]
    data=data1[data1$Dept==input$dept,]
    
    # if (input$store != "All") {
    #   data = month_store_dept_sales[month_store_dept_sales$Store==input$store,]
    # }
    # if (input$dept != "All") {
    #   data = month_store_dept_sales[month_store_dept_sales$Dept==input$dept,]
    # }
    barplot(data[,"Weekly_Sales"]
            ,main="Monthly Sales"
            ,ylab="Weekly_Sales"
            ,xlab="Month"
            ,names.arg=c("Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec")
    )
    
  })
  
  output$stats <- renderPrint({
    data=subset(month_store_dept_sales,month_store_dept_sales$Store==input$store & month_store_dept_sales$Dept==input$dept)
    summary(data[,"Weekly_Sales"])
  })
  
  output$MonthlyTempPlot <- renderPlot({
    data=subset(month_store_dept_sales,month_store_dept_sales$Store==input$store & month_store_dept_sales$Dept==input$dept)
    ggplot(data = data,
           aes(x=Temperature,y=Weekly_Sales)) + geom_line()
  })
  
  output$Sales_table <- DT::renderDataTable(DT::datatable({
    
    data <- year_month_store_sales  
    
    if((input$year != "All") & (input$month == "All")){
      data <- year_month_store_sales[year_month_store_sales$year == input$year,]
    }
    if((input$month != "All") & (input$year == "All")){
      data <- year_month_store_sales[year_month_store_sales$month == input$month,]
    }
    if((input$year != "All") & (input$month != "All")){
      data <- subset(year_month_store_sales,year_month_store_sales$year == input$year & year_month_store_sales$month == input$month)
      
    }
    
    data
  })) 

  
#deepti part
  
  output$plot=renderPlot({
    summarystore=subset(combinedData,combinedData$Type==input$type_deepti)
    summarydept=subset(summarystore, summarystore$Department==input$dept_deepti)
    dataplot=subset(summarydept,summarydept$Year==input$year_deepti)
    weeks=dataplot$Week
    Sales =dataplot$Sales
    #ggplot2(weeks,Sales,pch=20,type="line")
    ggplot(dataplot,aes(x=Week,y=Sales))+ geom_line(aes(color=factor(Store),group=Store))+scale_color_discrete(name="Store Number")+scale_y_continuous(labels=comma)+labs(x="Week",y="Sales in Dollars")
    #title(main="Variation of Sales with weeks")
    
  })
  
  output$summary <- renderPrint({
    summarystore=subset(combinedData,combinedData$Type==input$type_deepti)
    summarydept=subset(summarystore,summarystore$Department==input$dept_deepti)
    dataplot=subset(summarydept,summarydept$Year==input$year_deepti)
    summary(dataplot$Sales)
  })
  output$main_plot <- reactivePlot(function() {
    library(ggplot2)
    summaryyear=subset(combinedData,combinedData$Year==input$year1)
    summarydata=subset(summaryyear,summaryyear$Department==input$dept1)
    ggplot(summarydata,aes(Size,Sales))+geom_point(aes(colour=Type))+scale_color_discrete(name="Store Type")+scale_y_continuous(labels=comma)+labs(x="Store Size(Area)",y="Sales in Dollars")
  })
  
  output$plot1=reactivePlot(function(){
    
    summaryyear=subset(datamarkdown,combinedData$Type==input$Type)
    summarydata=subset(summaryyear,summaryyear$Dept==input$dept)
    ggplot(summarydata,aes(MarkDown1,Weekly_Sales))+geom_point(color="Red")
    
    
  })
  
  output$plot2=reactivePlot(function(){
    
    summaryyear=subset(datamarkdown,combinedData$Type==input$Type)
    summarydata=subset(summaryyear,summaryyear$Dept==input$dept)
    ggplot(summarydata,aes(MarkDown2,Weekly_Sales))+geom_point(color="Purple")
    
    
  })
  output$plot3=reactivePlot(function(){
    
    summaryyear=subset(datamarkdown,combinedData$Type==input$Type)
    summarydata=subset(summaryyear,summaryyear$Dept==input$dept)
    ggplot(summarydata,aes(MarkDown3,Weekly_Sales))+geom_point(color="Green")
    
    
  })
  output$plot4=reactivePlot(function(){
    
    summaryyear=subset(datamarkdown,combinedData$Type==input$Type)
    summarydata=subset(summaryyear,summaryyear$Dept==input$dept)
    ggplot(summarydata,aes(MarkDown4,Weekly_Sales))+geom_point(color="Blue")
    
    
  })
  
  output$plot5=reactivePlot(function(){
    
    summaryyear=subset(datamarkdown,combinedData$Type==input$Type)
    summarydata=subset(summaryyear,summaryyear$Dept==input$dept)
    ggplot(summarydata,aes(MarkDown5,Weekly_Sales))+geom_point(color="Yellow")
    
    
  })  
  
  
  }

shinyApp(ui=ui,server=server)