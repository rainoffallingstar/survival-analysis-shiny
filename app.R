# 加载必要的包
library(shiny)
library(readxl)
library(survival)
library(survminer)
library(stringr)
library(dplyr)
library(tidyr)
library(DT)
library(mellon)
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
  word <- stringr::str_split(magicformula,"~")[[1]][2] %>% 
    stringr::str_trim()
  return(word)
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
  df[[magicwords]] %>% 
    unlist()  %>% 
    factor() %>% levels() %>% length()
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
   df <- df %>%
     dplyr::filter(.[[magicwords]] != "NA") %>% 
     dplyr::filter(!is.na(.[[magicwords]])) %>% 
      dplyr::mutate(group = case_when(
        as.numeric(.[[magicwords]]) >= median(as.numeric(.[[magicwords]]), na.rm = TRUE) ~ "highlevel", 
        as.numeric(.[[magicwords]]) < median(as.numeric(.[[magicwords]]), na.rm = TRUE) ~ "lowlevel"))
   df$group <- factor(df$group,levels = c("highlevel","lowlevel"))
  }
  return(df)
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
    initial_data <- mellon::lung
  } else {
    paths <- path
    initial_data <- readxl::read_excel(paths #,col_types = col_types
                             )
  }
  return(initial_data)
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
        tabPanel(title = "数据查看", 
                  DT::DTOutput("datasetview")
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
    initial_data <- force_numeric_excel(input$file$datapath)
  })
  
  magic_classify <- reactive({
    data <- initial_data()
    check <- data[[de_magic_power(input$magicformula)]] %>% 
      unlist()  %>% 
      factor() %>% 
      levels() %>% 
      length() %>% 
      magic_word_classification()
  })
  
  data <- reactive({
    newdata <- initial_data() %>% 
      magic_table_process(magic_classify(),de_magic_power(input$magicformula))
  })
 
  #因为目前算法上有bug,所以在连续变量无法转换成功时，使用总的生存图
  magic_assessment_again <- reactive({
    data <- data()
    if (magic_classify() == TRUE){
      magic_assessment_again = TRUE
    } else if (length(levels(data$group)) == 2){
      magic_assessment_again = TRUE
    } else {
      magic_assessment_again = FALSE
    }
  })
   
  output$datasetview <- DT::renderDT({
    if (!magic_assessment_again()){
       initial_data()
    }else{
      data()
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
    print(paste("This application is under developed by Yanhua Zheng and Dr.Qinchuan Yu from the department of hematological bioinformatic special interest group(HBSig),Gmade Studio, and originally inspired by Dr.Zhenghua Liu. If you use our aplication in your research,please cite us as the following: Yanhua Zheng. (2023). rainoffallingstar/survival-analysis-shiny: release (v1.0). Zenodo. https://doi.org/10.5281/zenodo.8248064"))
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
                   #surv.median.line = "hv",# 增加中位生存时间
                   conf.int = TRUE ,# 增加置信区间
                   pval = TRUE, # 添加P值
                   #add.all = TRUE , # 添加总患者生存曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability" # 指定x轴标签
                   #break.x.by = 100 # 设置x轴刻度间距
        )
      }else {
        ggsurvplot(fit, 
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability" # 指定x轴标签
                   #break.x.by = 100 # 设置x轴刻度间距
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
                   #add.all = TRUE , # 添加总患者生存曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability" # 指定x轴标签
                   #break.x.by = 100# 设置x轴刻度间距
        )
      }else {
        ggsurvplot(fit, 
                   fun = "cumhaz", # 绘制累计风险曲线
                   palette = input$palettes,  # 自定义调色板
                   xlab = "Follow up time(d)", # 指定x轴标签
                   ylab = "Survival probability" # 指定x轴标签
                   #break.x.by = 100 # 设置x轴刻度间距
        )
      }
      
      
    })
  })
  
}

# 运行应用
shinyApp(ui = ui, server = server)
