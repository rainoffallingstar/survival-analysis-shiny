
library(readxl)
library(survival)
library(survminer)
library(stringr)
library(dplyr)
library(tidyr)
fit <- survfit(Surv(time, status) ~ cd97inblast, 
               data = survival)
fit
summary(fit)
ggsurvplot(fit, data = survival, 
           risk.table = TRUE,
           #surv.median.line = "hv",# 增加中位生存时间
           conf.int = TRUE ,# 增加置信区间
           pval = TRUE, # 添加P值
           #add.all = TRUE , # 添加总患者生存曲线
           palette = "hue",  # 自定义调色
           xlab = "Follow up time(d)", # 指定x轴标签
           ylab = "Survival probability",# 指定x轴标签
           legend = c(0.8,0.75), # 指定图例位置
           legend.title = "", # 设置图例标题
           #legend.labs = 3, # 指定图例分组标签
           #break.x.by = 100 # 设置x轴刻度间距
)
