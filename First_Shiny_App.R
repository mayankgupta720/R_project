# #reading file
# workingdata<- read.csv(file="C:\\Users\\gupta\\OneDrive\\MS BAIM\\Using R for Analytics\\Project\\workingdata.csv", header=TRUE, sep=",")
# tail(workingdata)
# names(workingdata)
# dim(workingdata)
# 
# #aggregate based on two dimensions
# 
# year_month_store_dept_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ year+month+Store+Dept
#                                      , data=workingdata, FUN=mean)
# 
# year_month_store_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ year+month+Store
#                                      , data=workingdata, FUN=mean)
# 
# month_store_dept_sales=aggregate(cbind(Store_Size,Weekly_Sales,Temperature,Fuel_Price) ~ month+Store+Dept
#                                  , data=workingdata, FUN=mean)
# 
# 
# # month_store_dept_sales=month_store_dept_sales[order(month_store_dept_sales$Store,
# #                                                           month_store_dept_sales$Dept,
# #                                                           month_store_dept_sales$month),]

tail(month_store_dept_sales)
tail(year_month_store_sales)
tail(year_month_store_dept_sales)

library(shiny)
library(ggplot2)
#install.packages("DT")
library(DT)
ui = navbarPage("Menu",  
  # Give the page a title
  tabPanel("Sales by stores"
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
  
 , tabPanel("Sales by month and Year"
            ,
            # Generate a row with a sidebar
            sidebarLayout(      
              # Define the sidebar with one input
              sidebarPanel(
                selectInput(inputId = "month"
                            , label = "Month:"
                            , choices=c("All",unique(year_month_store_sales$month))
                ,
                selectInput(inputId = "year"
                            ,label = "Year:"
                            ,choices=c("All",unique(year_month_store_sales$year)))
              ,hr()
              ,helpText("Select the month and the year for which you want to know the sales")
              )
            ,# Create a spot for the barplot
              mainPanel(
              DT::dataTableOutput("Sales_table") 
                       )
    )
  )
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
  summary(data[,"Weekly_Sales"])
  })
  
  output$MonthlyTempPlot <- renderPlot({
    ggplot(data = data,
    aes(x=Temperature,y=Weekly_Sales)) + geom_point()
})
  
  output$Sales_table <- DT::renderDataTable(DT::datatable({
  data  <-   year_month_store_sales
    if ((input$year != "All") && (input$month == "All")) {
      data=year_month_store_sales[year_month_store_sales$year == input$year,]
    }
    if ((input$month != "All") && (input$year == "All")) {
      data=year_month_store_sales[year_month_store_sales$month == input$month,]
    } 
    if (input$year != "All" && input$month != "All") {
      data=year_month_store_sales[year_month_store_sales$year == input$year,]
      data=data[data$month == input$month,]
    }   
  data
  })) 
  }

shinyApp(ui=ui,server=server)