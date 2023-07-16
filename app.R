# 加载必要的包
library(shiny)
library(readxl)
library(survival)
library(survminer)

# 定义需要用到的各种参数

palette_list <- c("grey","npg","aaas","lancet","jco", 
"ucscgb","uchicago","simpsons","rickandmorty","hue")

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
      selectInput("palettes","选择调色板",choices = palette_list,"hue" )
      actionButton("go", "开始分析")
    ),
    mainPanel(
      tableOutput("table"),
      plotOutput("plot"),
      plotOutput("plot2")
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
      ggsurvplot(sur_fit(), data = data(), 
                    risk.table = TRUE,
                    surv.median.line = "hv",# 增加中位生存时间
                    conf.int = TRUE ,# 增加置信区间
                    pval = TRUE, # 添加P值
                    add.all = TRUE , # 添加总患者生存曲线
                    palette = input$palettes,  # 自定义调色板
                    xlab = "Follow up time(d)", # 指定x轴标签
                    ylab = "Survival probability",# 指定x轴标签
                    legend = c(0.8,0.75), # 指定图例位置
                    legend.title = "", # 设置图例标题
                    legend.labs = factor(input$variable), # 指定图例分组标签
                    break.x.by = 100 # 设置x轴刻度间距
                    )
    })
    output$plot2 <- renderPlot({
    ggsurvplot(sur_fit(), data = data(), 
           conf.int = TRUE, # 增加置信区间
           fun = "cumhaz", # 绘制累计风险曲线
           cumevents = TRUE,
           surv.median.line = "hv",# 增加中位生存时间
           conf.int = TRUE ,# 增加置信区间
           pval = TRUE, # 添加P值
           add.all = TRUE , # 添加总患者生存曲线
           palette = input$palettes,  # 自定义调色板
           xlab = "Follow up time(d)", # 指定x轴标签
           ylab = "Survival probability",# 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "", # 设置图例标题
           legend.labs = factor(input$variable), # 指定图例分组标签
           break.x.by = 100# 设置x轴刻度间距
           )


  })
  })
  
}

# 运行应用
shinyApp(ui = ui, server = server)