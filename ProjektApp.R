

library (dplyr)
library(shiny)
library(ggplot2)
library(plotly)

library (plyr)


student <-
  read.csv("D:/Michal/Semestr V/awd/Projekt/student-mat.csv")

zmienne <- colnames(student)


student$Dalc <- as.factor(student$Dalc)
student$Dalc <- mapvalues(
  student$Dalc,
  from = 1:5,
  to = c("Very Low", "Low", "Medium", "High", "Very High")
)

student$Walc <- as.factor(student$Walc)
student$Walc <- mapvalues(
  student$Walc,
  from = 1:5,
  to = c("Very Low", "Low", "Medium", "High", "Very High")
)
studentcopy <- student

ui <- fluidPage(
  navbarPage(
    "Analiza danych o spożyciu alkoholu przez uczniów szkół średnich w Porto",
    inverse = TRUE,
    tabPanel(
      "Wszystkie Pytania z Ankiety",
      sidebarLayout(
        titlePanel("Wszystkie pytania zadane ankietowanym"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "pytanie",
              "Wybierz pytanie zadane ankietowanym:",
              c(zmienne[3], zmienne[5], zmienne[7:15], zmienne[18:20], zmienne[23:29])
            ),
            br(),
            
            radioButtons(
              "radio2",
              h3("Która szkoła"),
              choices = list(
                "GP - Gabriel Pereira " = 1,
                "MS - Mousinho da Silveira" = 2,
                "obydwie" = 3
              ),
              selected = 3 ,
              inline = TRUE
            ),
            
            radioButtons(
              "radio3",
              h3("Płeć"),
              choices = list(
                "Męższczyzna" = 1,
                "Kobieta" = 2,
                "Oboje" = 3
              ),
              selected = 3 ,
              inline = TRUE
            ),
            br(),
            
            radioButtons(
              inputId = "radio",
              "Kategoria:",
              choices = c(zmienne[4] , zmienne[6] , zmienne[16], zmienne[17], zmienne[21], zmienne[22])
            )
          ),
          mainPanel(plotOutput("barchart"))
        )
      )
    ),
    tabPanel(
      "Nieobecnośći uczniów",
      sidebarLayout(
        titlePanel("Nieobecnośći uczniów lub wybór stopnia klasy"),
        sidebarLayout(
          sidebarPanel(
            selectInput("pytanie2", "Wybierz pytanie zadane ankietowanym:", c(zmienne[30:33])),
            
            sliderInput(
              inputId = "bins",
              label = "Liczba przedziałów:",
              min = 1,
              max = 75,
              value = 12
            ),
            
          ),
          
          
          mainPanel(plotOutput("barchart2"))
        )
      )
    ),
    tabPanel(
      "Spożycie alkoholu przez uczniów",
      sidebarLayout(
        titlePanel("Spożycie alkoholu przez uczniów"),
        sidebarLayout(sidebarPanel(
          selectInput("pytanie3", "Wybierz pytanie zadane ankietowanym:", c(zmienne[27:28])),
          
        ),
        
        
        mainPanel(plotOutput("barchart3")))
      )
    )
    
  )
)


server <- function(input, output, session) {
  output$barchart <- renderPlot({
    rad <- as.character(input$radio)
    colm <- as.character(input$pytanie)
    
    switch(input$radio2,
           "1" = {
             student <- filter(student, school == "GP")
           },
           "2" = {
             student <- filter(student, school == "MS")
           },
           "3" = {
             student <- studentcopy
           },
           stop("Enter something that switches me!"))
    switch(input$radio3,
           "1" = {
             student <- filter(student, sex == "M")
           },
           "2" = {
             student <- filter(student, sex == "F")
           },
           "3" = {
             student <- studentcopy
           },
           stop("Enter something that switches me!"))
    ggplot(student, aes_string(x =
                                 colm , col = rad , fill = rad)) + geom_bar(stat = "count",
                                                                            width = 0.7,) +
      ylab("Liczba osób")
    
    
  })
  output$barchart2 <- renderPlot({
    rad <- as.character(input$radio)
    colm <- as.character(input$pytanie2)
    
    bins <-
      seq(min(student[input$pytanie2]), max(student[input$pytanie2]), length.out = input$bins + 1)
    
    ggplot(student, aes_string(x = colm)) +
      geom_histogram(
        fill = "light blue",
        color = "black",
        position = "identity",
        breaks = bins
      ) +
      ylab("Liczba osób")
    
    
  })
  output$barchart3 <- renderPlot({
    col <- c("red", "green", "orange", "blue", "yellow")
    colm <- as.character(input$pytanie3)
    
    if (colm == "Dalc")
      
      ggplot(student, aes(x = age, fill = student$Dalc)) +
      geom_histogram(binwidth = 1, colour = "black") +
      facet_grid( ~ student$Dalc) +
      scale_fill_manual(values = col) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Spożycie alkoholu przez ucznióW w dni robocze") +
      xlab("Wiek uczniow")
    else
      
      
      ggplot(student, aes(x = age, fill = student$Walc)) +
      geom_histogram(binwidth = 1, colour = "black") +
      facet_grid( ~ student$Walc) +
      scale_fill_manual(values = col) +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle("Spożycie alkoholu przez uczniow w weekend") +
      xlab("Wiek uczniow")
    
  })
  
  
}

shinyApp(ui = ui, server = server)