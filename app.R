library(shinydashboard)
library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(car)
library(varhandle)
ui <-   dashboardPage(
  
  # Application title
  dashboardHeader(title ="Dashboard about the number of newborns in the United States from 1968 to 2008"),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("date", label = h3("Date range")))),
  dashboardBody(
    
    ## Using box to display plots
    tabItem(tabName = "box",
            # First Row
            fluidRow((box(title = "Box with a plot", plotOutput("plot3", height = 250))),
                     box(title = "Box with a plot", plotOutput("plot4", height = 250)))),
    
    # Second Row
    fluidRow(box(title = "Box with scatterplot", plotOutput("plot5", height = 250), width = 8))
  ))
library(shiny)
library(shinydashboard)
server <-function(input, output){
  temp <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/6647392bbdeff491311560e00ae0cc81/raw/17cd0a885dafc0fd8354544e28ae9067d26f4721/Temp.csv")
  temp1 <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/b018f1606a2357d416edf6f91c5ae476/raw/1add0975b872e0c2650959f2dbae5e386a81c865/Temp1.csv")
  tabble3 <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/2633e6b8e5c99f3b39b753928b5f137f/raw/3845d1838e5058a1f5f3d15747bf28ab7818b72a/tabble3.csv")
  todo_copies <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/265a7b3d8f9c3293a3631b09e0506c91/raw/5167fe978c5adc2f99153fb96a787b7cd3b16985/Todo_copies.csv")
  todo_copies <- unfactor(todo_copies)
  output$plot1 <- renderPlot({
    
    plot(temp$date, length.out = input$date + 1, temp$Avg.baby3)
  })
  output$value <- renderPrint({input$plot1})
  output$plot2 <- renderPlot({
    plot(temp1$Avg.baby1, temp1$date)
  })
  output$plot3 <- renderPlot({
    ggplot(temp, aes(x=date, y=Avg.baby3)) + 
      geom_bar(stat="identity")
  })
  output$plot4 <- renderPlot({
    ggplot(temp1, aes(x=temp1$year, y=temp1$Avg.baby1)) + 
      geom_bar(stat="identity")})
  output$plot5 <- renderPlot({
    ggplot(todo_copies, aes(x=apgar_1min, y=mother_age)) +
      geom_point()
  })}
shinyApp(ui, server)