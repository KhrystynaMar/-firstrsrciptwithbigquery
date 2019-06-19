library(shinydashboard)
library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(car)
library(varhandle)
library(datasets)
temp <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/6647392bbdeff491311560e00ae0cc81/raw/17cd0a885dafc0fd8354544e28ae9067d26f4721/Temp.csv", stringsAsFactors = TRUE)
Avg.baby3 <- select(
  .data = temp,
  Avg.baby3)
date1 <- as.Date(temp$date)
temp5 <- data.frame(date1=date1, Avg.baby3=Avg.baby3)
temp1 <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/b018f1606a2357d416edf6f91c5ae476/raw/1add0975b872e0c2650959f2dbae5e386a81c865/Temp1.csv")
Avg.baby1 <- select(
  .data = temp1,
  Avg.baby1)
year <- as.Date(as.character(temp1$year), format = "%Y")
temp8 <- data.frame(x=c(year), y=c(Avg.baby1))
temp1$year <- as.numeric(temp1$year)
tabble3 <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/2633e6b8e5c99f3b39b753928b5f137f/raw/3845d1838e5058a1f5f3d15747bf28ab7818b72a/tabble3.csv")
todo_copies <- read.csv("https://gist.githubusercontent.com/KhrystynaMar/265a7b3d8f9c3293a3631b09e0506c91/raw/5167fe978c5adc2f99153fb96a787b7cd3b16985/Todo_copies.csv")
todo_copies <- unfactor(todo_copies)
ui <-   dashboardPage(
  
  # Application title
  dashboardHeader(title ="Dashboard about the number of newborns in the United States from 1968 to 2008"),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("date8", label = h3("Date range"), start  = min(temp5$date1),
                     end    = max(temp5$date1),
                     min    = min(temp5$date1),
                     max    = max(temp5$date1),
                     separator = " - ", format = "dd/mm/yy"),
      selectInput(inputId = 'Avg.baby3',
                  label='Avg.baby3',
                  choices='Avg.baby3',
                  selected='Avg.baby3'),
      sliderInput("year1", label = h3("Slider"), min = (1971), 
                  max = (2008), value = c(temp1$year))
    )),
  dashboardBody(
    
    ## Using box to display plots
    tabItem(tabName = "box",
            # First Row
            fluidRow((box(title = "Box with a plot", plotOutput("plot3", height = 250))),
                     box(title = "Box with a plot", plotOutput("plot6", height = 250)))),
    fluidRow((box(title = "Box with a plot", plotOutput("plot4", height = 250))),
              box(title = "Box with a plot", plotOutput("plot8", height = 250))),
    
    # Second Row
    fluidRow((box(title = "Box with scatterplot", plotOutput("plot5", height = 250)))
  ))
library(shiny)
library(shinydashboard)
server <-function(input, output){
  output$plot3 <- renderPlot({
    ggplot() +
      labs(x = "Date", y = "Baby")+
      geom_bar(data=filter(temp5, date1==input$date8),
               mapping=aes(x=date1, y=Avg.baby3, fill=Avg.baby3), stat="identity") +
      coord_flip()
  })
  output$plot6 <- renderPlot({
    ggplot(temp, aes(x=date, y=Avg.baby3)) + 
      geom_bar(stat="identity")
  })
  output$plot4 <- renderPlot({
    ggplot() +
      labs(x = "Year", y = "Baby") +
    geom_bar(data=filter(temp1, year==input$year1),
             mapping=aes(x=input$year1, y=Avg.baby1, fill=Avg.baby1), stat="identity") +
      coord_flip()
  })
  output$plot5 <- renderPlot({
    ggplot(todo_copies, aes(x=apgar_1min, y=mother_age)) +
      geom_point()
  })
  output$plot8 <- renderPlot({
    ggplot(temp1, aes(x=temp1$year, y=temp1$Avg.baby1)) + 
      geom_bar(stat="identity")
  })
  output$plot9 <- renderPlot({
    ggplot(todo_copies, aes(x=ever_born, y=cigarette_use)) + 
      geom_point()
  })
  }
shinyApp(ui, server)
