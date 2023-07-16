# 加载必要的包
library(shiny)
library(readxl)
library(survival)
library(survminer)

# 定义用户界面
ui <- fluidPage(
  titlePanel("生存分析"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "选择Excel文件",
                accept = c(".xlsx")
      ),
      textInput("status", "输入结局变量名","status"),
      textInput("time", "输入结局变量名","time"),
      uiOutput("varSelect"),
      actionButton("go", "开始分析")
    ),
    mainPanel(
      tableOutput("table"),
      plotOutput("plot")
      #plotOutput("plot2")
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  output$varSelect <- renderUI({
    selectInput("variable", "选择临床变量", choices = setdiff(names(data()), input$outcome))
  })
  sur_fit <- reactive({
    fit <- survfit(as.formula(paste("Surv(time, status) ~", input$variable)), data = data())
  })

  observeEvent(input$go, {
    output$table <- renderTable({
      fit <- sur_fit()
      summary(fit)
    })
    
    output$plot <- renderPlot({
      fit <- sur_fit()
      ggsurvplot(fit, data = data(), risk.table = TRUE)
    })
  })
  #output$plot2 <- renderPlot({

  #})
}

# 运行应用
shinyApp(ui = ui, server = server)