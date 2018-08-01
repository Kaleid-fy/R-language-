library(shiny)
library(shinydashboard)
library(DT)
library(plyr)
library(dygraphs)

server <- function(input, output) {
  retention <- reactive({
    read.csv(input$file1$datapath, header=T)
  })

  output$retention <- DT::renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    DT::datatable(retention(),rownames = F)
  })
  
  output$barplot1 <- renderPlot({
    if(is.null(input$file1))
      return(NULL) 
    barplot(retention()[1:7,2],axes = F,col = "lightblue",
            main="前七日留存率")
    axis(1,at=c(0.5,2,3,4.3,5.5,6.7,8),labels = retention()[1:7,1],
         font.axis=2,cex.axis=.8,col.axis="red",lty=5,col.ticks = "yellow4")
    axis(2,at=seq(0,1,by=0.1),font.axis=2,cex.axis=.8,col.axis="blue",lty=5,col.ticks = "yellow4" )
  })
  
  # 留存率预测
  #  系数估计
  fit <- reactive({
    if(is.null(input$file1))
      return(NULL) 
    x <- ifelse(nchar(retention()[,1])==5,substr(retention()[,1],1,1),substr(retention()[,1],1,2))
    x <- as.numeric(x)
    y <- retention()[,2]
    fit <- nls(y ~ a*x^b,start = list(a = 1,b = 1))
  })
 #  留存率预测
  predicted <- reactive({
    predited <- predict(fit(),data.frame(x=seq(1,365)))
  })
  # LT预测
  LT <- reactive({
    cumsum(predicted())+1
  })
  # 估计系数展示
  output$model <- renderPrint({
    print(fit())
  })
  # 留存率预测曲线
  output$line2 <- renderDygraph({
    predicted.ts <- ts(predicted())
    dygraph(predicted.ts,main="retention raw forecast") %>% 
      dySeries("V1",label="retention_raw",strokeWidth = 3) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>%
      dyAxis("x", drawGrid = FALSE) %>% 
      dyAxis("y", label = "Revention Raw") %>%
      dyOptions(stackedGraph = TRUE,
                includeZero = TRUE, 
                axisLineColor = "navy", 
                gridLineColor = "lightblue") %>%
      dyRangeSelector()
    
  })
  # LT预测曲线
  output$line3 <- renderDygraph({
    LT.ts <- ts(LT())
    dygraph(LT.ts,main="LT forecast") %>% 
      dySeries("V1",label="LT",strokeWidth = 3) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>%
      dyAxis("x", drawGrid = FALSE) %>% 
      dyAxis("y", label = "LV(Life Time)") %>%
      dyOptions(includeZero = TRUE,
                colors = "green",
                fillGraph = TRUE,
                fillAlpha = 0.4,
                axisLineColor = "navy", 
                gridLineColor = "lightblue") %>%
      dyRangeSelector()
  })
  # LTV预测曲线
  output$line4 <- renderDygraph({
    LTV.ts <- input$arpu*ts(LT())
    dygraph(LTV.ts,main="LTV forecast") %>% 
      dySeries("V1",label="LTV",strokeWidth = 3) %>%
      dyOptions(colors = "red",fillGraph = TRUE,fillAlpha = 0.4) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE) %>%
      dyAxis("x", drawGrid = FALSE) %>% 
      dyAxis("y", label = "LTV(Life Time Value)") %>%
      dyRangeSelector()
  })
  # 关键数据统计
  output$data1 <- DT::renderDataTable({
    data1 <- data.frame("day"=c(1:365),"Retention Raw"=paste0(round(ts(predicted())[1:365],4)*100,"%"),
                        "LT"=round(ts(LT())[1:365],3),"LTV"=round((input$arpu*ts(LT()))[1:365],3))
    DT::datatable(data1,options = list(pageLength = 4),rownames = F)
  })
}
