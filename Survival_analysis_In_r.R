# ========Use R to do survival test ========
# 本篇来自 https://shixiangwang.github.io/home/cn/post/r-survival/#fn1, 感谢王诗翔大哥
# 生存分析 - 追踪直至死亡的时间

# 生存分析可让我们分析事件发生的速率，而不会假设速率不变。一般而言，生存分析可以让我们对事件发生之前的时间进行建模1或比较不同组之间的事件时间，或者事件时间与定量变量之间的相关性。


# 比例风险回归也称为Cox回归，是评估不同变量对生存率影响的最常用方法。

# 确保在导入前安装好
library(survival)
library(dplyr)
library(survminer)

?lung

lung <- as_tibble(lung)
lung

# 使用surv构建生存对象
s <- Surv(time = lung$time, event = lung$status)
class(s)


# 现在，让我们使用survfit()函数拟合一条生存曲线。这里让我们先创建一条不考虑任何比较的生存曲线，所以我们只需要指定survfit()在公式里期望的截距（比如~1）
# 使用summary看模型的详细情况
summary(survfit(s~1))

# 看性别的存活率是否一样
sfit <- survfit(Surv(time, status)~sex, data=lung)

# summary()函数中可以设定时间参数用来选定一个时间区间，我们可以以此比对男生是不是比女生有更高的风险：
summary(sfit, times=seq(0, 1000, 100))


# ============ 现在我们使用Kaplan-Meier曲线来可视化这一结果

ggsurvplot(sfit)

# 增加risk table以及long-rank检验，看是否存在显著差异
ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Male", "Female"), legend.title="Sex",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curve for Lung Cancer Survival", 
           risk.table.height=.15)

# 生存曲线在面对很多分类变量的时候不太实用，而且生存曲线另外不能可视化的是连续型变量的风险。
# 所以我们可以使用 coxPH cox风险比例模型
# exp(coef) （也叫HR） 在结果中的它就是风险比率——该变量对风险率的乘数效应（对于该变量每个单位增加的）。因此，对于像性别这样的分类变量，从男性（基线）到女性的结果大约减少约40％的危险。
fit <- coxph(Surv(time, status)~sex, data=lung)
fit
summary(fit)

# 比较所有的因素，结果显示在分类变量中ecog 和sex 是强大的预测指标。
fit <- coxph(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=lung)
fit


# ====  使用cox风险模型比较连续变量 

coxph(Surv(time, status)~age, data=lung)


mean(lung$age)
hist(lung$age)
ggplot(lung, aes(age)) + geom_histogram(bins=20)

# 正如前面所说的，我们没法使用KM可视化的方法来研究一个连续性变量，那样会变的很难看，我们这里将不同的年龄进行分类，然后再绘图
# the base r way:
lung$agecat <- cut(lung$age, breaks=c(0, 62, Inf), labels=c("young", "old"))
# or the dplyr way:
lung <- lung %>% 
  mutate(agecat=cut(age, breaks=c(0, 62, Inf), labels=c("young", "old")))
head(lung)

# == 按照62岁分割的年龄显示不重要
ggsurvplot(survfit(Surv(time, status)~agecat, data=lung), pval=TRUE)

# == 然而按照70岁显示的重要
# the base r way:
lung$agecat <- cut(lung$age, breaks=c(0, 70, Inf), labels=c("young", "old"))
# or the dplyr way:
lung <- lung %>% 
  mutate(agecat=cut(age, breaks=c(0, 70, Inf), labels=c("young", "old")))
# plot!
ggsurvplot(survfit(Surv(time, status)~agecat, data=lung), pval=TRUE)

# 请记住，Cox回归分析整个分布范围内的连续变量，其中Kaplan-Meier图上的对数秩检验可能会根据您对连续变量进行
# 分类而发生变化。他们以一种不同的方式回答类似的问题：回归模型提出的问题是“年龄对生存有什么影响？”，
# 而对数秩检验和KM图则问：“那些不到70岁和70岁以上的人有差异吗？”。


# === 如何找到最佳切割点？
# 在survminer包中可以找到最佳切割点

res.cut <- surv_cutpoint(lung, time = "time", event = "status",
                         variables = c("age"))
summary(res.cut)
plot(res.cut, "age", palette = "npg")

# 根据之前的切割点进行分类
res.cat <- surv_categorize(res.cut)
head(res.cat)

# 
library("survival")
fit <- survfit(Surv(time, status) ~ age, data = res.cat)
ggsurvplot(fit, risk.table = TRUE, conf.int = TRUE, pval = T)


