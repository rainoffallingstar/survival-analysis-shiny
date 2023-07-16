data <- read_excel("lung.xlsx")
datas <- data %>% mutate(
  times = time,
  status = status,
  variable = sex
) %>% 
  select(all_of(c("times","status","variable"))) %>% 
  na.omit()

fit <- survfit(Surv(as.numeric(times), factor(status)) ~ factor(variable), data = datas)
summary(fit)
ggsurvplot(fit, data = datas, 
           risk.table = TRUE,
           surv.median.line = "hv",# 增加中位生存时间
           conf.int = TRUE ,# 增加置信区间
           pval = TRUE, # 添加P值
           add.all = TRUE , # 添加总患者生存曲线
           palette = "hue",  # 自定义调色板
           xlab = "Follow up time(d)", # 指定x轴标签
           ylab = "Survival probability",# 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "", # 设置图例标题
           #legend.labs = levels(variable), # 指定图例分组标签
           break.x.by = 100 # 设置x轴刻度间距
)
ggsurvplot(fit, data = datas
)

ggsurvplot(fit, data = datas, 
           conf.int = TRUE, # 增加置信区间
           fun = "cumhaz")

datas <- within(datas,
                status = factor(datas$status,levels = c(1,2),
                
                variable = factor(datas$variable,levels = c(1,2))
                
