library(shiny)
library(shinydashboard)
library(DT)
library(plyr)
library(dygraphs)

dashboardPage(
  dashboardHeader(title = "数据预测模型"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("LTV预测",tabName = "LTV",icon = icon("bar-chart"),
               menuSubItem("导入7日留存率数据", tabName = "Retention-rate", icon = icon("table")),
               menuSubItem("预测模型",tabName = "forecast",icon = icon("line-chart")))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Retention-rate",
              fileInput('file1', 'Choose CSV File'),
              DT::dataTableOutput('retention'),
              plotOutput("barplot1")
      ),
      tabItem(tabName = "forecast",
              tabBox(width = 12,
                fluidRow(
                  box(
                    title = "系数估计", status = "primary", solidHeader = TRUE, width = 6, collapsible = T,
                    verbatimTextOutput("model")
                  )
                ),
                fluidRow(
                  box(
                    title = "数据统计", status = "primary", solidHeader = TRUE, width = 6, collapsible = T,
                    DT::dataTableOutput("data1")
                  )
                )
              ),
              numericInput("arpu","ARPU：",value = 0.83,step = 0.1),
                     fluidRow(
                       box(
                         title = "留存率预测曲线", status = "info", solidHeader = TRUE, width = 12, collapsible = T,
                         dygraphOutput("line2")
                       )
                     ),
              fluidRow(
                box(
                  title = "LT预测曲线", status = "info", solidHeader = TRUE, width = 12, collapsible = T,
                  dygraphOutput("line3")
                )
              ),
              fluidRow(
                box(
                  title = "LTV预测曲线", status = "info", solidHeader = TRUE, width = 12, collapsible = T,
                  dygraphOutput("line4")
                )
              ))
)))