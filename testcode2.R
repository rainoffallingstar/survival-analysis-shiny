# 加载生存分析 survminer，survival 包
library(survminer) 
library(survival) 
data(package="survival")  #查看内置数据集
#1. 导入内置数据集
lung # 加载lung数据集
str(lung)
#2. 拟合生存曲线
fit <- surv_fit(Surv(time, status) ~ group,  data = totla)

fit
#结果
Call: 
survfit(formula = Surv(time, status) ~ lung[,5], data = lung)
fit
n events median 0.95LCL 0.95UCL
sex=1 138    112    270     212     310
sex=2  90     53    426     348     550
summary(fit) #查看生存分析结果
#3. 绘制基础曲线
ggsurvplot(fit, # 创建的拟合对象
           data = totla,  # 指定变量数据来源
           conf.int = TRUE, # 显示置信区间
           pval = TRUE, # 添加P值
           risk.table = TRUE, # 绘制累计风险曲线
           surv.median.line = "hv", # 添加中位生存时间线
           add.all = TRUE, # 添加总患者生存曲线
           palette = "hue")  # 自定义调色板

ggsurvplot(fit, data = totla, 
           risk.table = TRUE,
           surv.median.line = "hv",# 增加中位生存时间
           conf.int = TRUE ,# 增加置信区间
           pval = TRUE, # 添加P值
           add.all = TRUE , # 添加总患者生存曲线
           palette = "hue",  # 自定义调色板
           xlab = "Follow up time(d)", # 指定x轴标签
           ylab = "Survival probability",# 指定x轴标签
           break.x.by = 100 # 设置x轴刻度间距
)
ggsurvplot(fit, data = lung, 
           risk.table = TRUE,
           surv.median.line = "hv",# 增加中位生存时间
           conf.int = TRUE ,# 增加置信区间
           pval = TRUE, # 添加P值
           add.all = TRUE , # 添加总患者生存曲线
           palette = "hue",  # 自定义调色板
           xlab = "Follow up time(d)", # 指定x轴标签
           ylab = "Survival probability",# 指定x轴标签
           break.x.by = 100 # 设置x轴刻度间距
)
