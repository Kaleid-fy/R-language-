library(shiny)


shinyUI(pageWithSidebar(
  
  #应用标题
  headerPanel("Miles per Gallon"),
  
  sidebarPanel(
    selectInput("variable","Varible:",
                list("Cylinder"="cyl",
                     "Transmission"="am",
                     "Gear"="gear")),
    
    checkboxInput("outliers","Show Outliers",FALSE)
    ),
  
  mainPanel(
    h3(textOutput("caption")),
    
    plotOutput("mpgPlot")
  )
  
))