# 加载必要的包
library(shiny)
library(readxl)
library(survival)
library(survminer)
library(stringr)
library(dplyr)
library(tidyr)
# 定义需要用到的各种参数
# 调色盘的选项预加载
palette_list <- c("grey","npg","aaas","lancet","jco", 
                  "ucscgb","uchicago","simpsons","rickandmorty","hue")
# 对magicformula的formula化生效
dfs_transform <- function(x){
  as.formula(x)
}
# 提取magicformula的最后一个变量进行处理
de_magic_power <- function(magicformula){
  str_split(magicformula,"~")[[1]][2] %>% str_trim()
}
# 对不存在于本魔法世界的magicword进行警告，简单说就是对不在于表格内的变量进行警告
real_world_warning <- function(magicformula,magicwords){
  if (de_magic_power(magicformula) %in% magicwords){
    real_world_warning <- "Your prompt words is almost perfect!"
  }else {
    real_world_warning <- "Your prompt words does not work!"
  }
  
}
# 对magicformla的最后一个变量进行识别是否是分类变量

# 计算levels先

levels_num <- function(magicwords,df){
  df %>% na.omit() %>% select(all_of(magicwords)) %>% unlist()  %>% factor() %>% levels() %>% length()
}

magic_word_classification <- function(magicnum){
  
  if (magicnum == 2){
    class_magic <- TRUE
  }else{
    class_magic <- FALSE
    
  }
}

# 如果不是分类变量，则需要新建一个组别使其可分类
magic_table_process <- function(df,magicnum,magicwords){
  if (magicnum == TRUE){
    df <- df
  } else {
    #tidyverse 实现
    # df <- df %>%
    #  mutate(group = ifelse(magicwords >= median(magicwords, na.rm = TRUE), 
    #                       "1", "2")) %>% 
    #mutate(group = as.numeric(group))
    # base-r 实现
    df <- within(df, {
      group <- ifelse(magicwords >= median(magicwords, na.rm = TRUE), 
                      "1", "2")
    })
  }
}
# 如果不是分类变量，则需要修改magicformula
maigcformula_assesment <- function(magicwords,magicformula){
  if (magicwords == TRUE){
    magicformula <- magicformula
  } else {
    magicformula <- "Surv(time, status) ~ group"
  }
}

# excel 文件读取，强制全部为numeric,并设置path为null时自动加载data文件夹下的lung.xlxs

force_numeric_excel <- function(path){
  if (is.null(path)){
    col_types <- rep("numeric", ncol(read_excel("data/lung.xlsx", n_max = 3)))
    initial_data <- read_excel("data/lung.xlsx",col_types = col_types) 
  } else {
    col_types <- rep("numeric", ncol(read_excel(path, n_max = 3)))
    initial_data <- read_excel(path,col_types = col_types) 
  }
}

# TO-DOS
# 解析字符后的变量类型，如果是分类变量直接运行，如果是连续型变量，按中位数进行分组为high-levle&low-level
# 解释路径，如果路径不存在，默认加载data文件下的lung.xlxs
# 定义用户界面
ui <- fluidPage(
  titlePanel("Survival Analysis Utils"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Your Excel files",
                accept = c(".xlsx")
      ),
      textInput("magicformula", "Input Your Magic Prompt","Surv(time, status) ~ sex"),
      selectInput("palettes","选择调色板",choices = palette_list,"hue" ),
      actionButton("go", "开始分析")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "生存曲线",
                 plotOutput("plot"),
                 textOutput("warn")
        ),
        tabPanel(title = "累积风险曲线",
                 plotOutput("plot2")
        ),
        tabPanel(title = "建模细节", 
                 verbatimTextOutput("table")
        ),
        tabPanel(title = "关于我", 
                 textOutput("about")
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  initial_data <- reactive({
    req(input$file)
    initial_data <- force_numeric_excel(input$file$path)
  })
  
  magic_classify <- reactive({
    initial_data() %>% na.omit() %>% select(de_magic_power(input$magicformula)) %>% 
      unlist()  %>% factor() %>% levels() %>% length() %>% 
      magic_word_classification()
    
  })
  
  data <- reactive({
    if (magic_classify() == TRUE){
      df <- initial_data()
    } else {
      #tidyverse 实现
      # df <- df %>%
      #  mutate(group = ifelse(magicwords >= median(magicwords, na.rm = TRUE), 
      #                       "1", "2")) %>% 
      #mutate(group = as.numeric(group))
      # base-r 实现
      df <- within(initial_data(), {
        group <- ifelse(de_magic_power(input$magicformula) >= median(de_magic_power(input$magicformula), na.rm = TRUE), 
                        "1", "2")
      })
    }
  })
  
  #因为目前算法上有bug,所以在连续变量无法转换成功时，使用总的生存图
  
  magic_assessment_again <- reactive({
    if (magic_classify() == TRUE){
      magic_assessment_again = TRUE
    } else if (
      data() %>% na.omit() %>% select(group) %>% 
      unlist()  %>% factor() %>% levels() %>% length() %>% 
      magic_word_classification() == TRUE){
      magic_assessment_again = TRUE
    } else {
      magic_assessment_again = FALSE
    }
    
  })
  dfs_transed <- reactive({
    if (magic_assessment_again() == TRUE){
      new_magicformula <- magic_classify() %>% 
        maigcformula_assesment(input$magicformula)
    }else {
      new_magicformula <- "Surv(time, status) ~ 1"
    }
    
  })
  sur_fit <- reactive({
    #dfs <- input$magicformula
    fit <- surv_fit(
      formula = dfs_transform(dfs_transed()), data = data())
  })
  output$warn <- renderText({
    paste(real_world_warning(input$magicformula,names(initial_data())),
          "your magic variable is",magic_assessment_again())
  })
  output$about <- renderText({
    print(paste("This application is under developed by Yanhua Zheng,Dr.Qinchuan Yu and Prof.Xiaoxue Wang from the department of hematology,CMU1H, and originally inspired by Dr.Zhenghua Liu. "))
  })
  
  observeEvent(input$go, {
    output$table <- renderPrint({
      fit <- sur_fit()
      fit
    })
    
    
    
    
    output$plot <- renderPlot({
      fit <- sur_fit()
      if (magic_assessment_again() == TRUE){
        ggsurvplot(fit, data = data(), 
                   risk.table = TRUE,
                   surv.median.line = "hv",# 增加中位生存时间
                   conf.int = TRUE ,# 增加置信区间
                   pval = TRUE, # 添加P值
                   add.all = TRUE , # 添加总患者生存曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability",# 指定x轴标签
                   break.x.by = 100 # 设置x轴刻度间距
        )
      }else {
        ggsurvplot(fit, 
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability",# 指定x轴标签
                   break.x.by = 100 # 设置x轴刻度间距
        )
      }
      
    })
    output$plot2 <- renderPlot({
      
      fit <- sur_fit()
      if (magic_assessment_again() == TRUE){
        ggsurvplot(fit, data = data(), 
                   conf.int = TRUE, # 增加置信区间
                   fun = "cumhaz", # 绘制累计风险曲线
                   cumevents = TRUE,
                   #surv.median.line = "hv",# 增加中位生存时间
                   pval = TRUE, # 添加P值
                   add.all = TRUE , # 添加总患者生存曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability",# 指定x轴标签
                   break.x.by = 100# 设置x轴刻度间距
        )
      }else {
        ggsurvplot(fit, 
                   fun = "cumhaz", # 绘制累计风险曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability",# 指定x轴标签
                   break.x.by = 100 # 设置x轴刻度间距
        )
      }
      
      
    })
  })
  
}

# 运行应用
shinyApp(ui = ui, server = server)