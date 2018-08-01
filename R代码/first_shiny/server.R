library(shiny)

library(datasets)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


shinyServer(function(input,output){
  #把需要变动的放入reactive中
  formulaText <- reactive({
    paste("mpg ~",input$variable)
  })
  
  #设置标题
  output$caption <- renderText({
    formulaText()
  })
  
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers
            )
  })
  
})