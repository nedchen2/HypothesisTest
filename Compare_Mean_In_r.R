# Comparing the mean in r

require(ggpubr)
data("ToothGrowth")
head(ToothGrowth)

compare_means(len ~ supp, data = ToothGrowth)

p <- ggboxplot(ToothGrowth, x="supp",
               y = "len", color = "supp",
               palette = "jco", add = "jitter")
# 添加p值
p + stat_compare_means()

# 成对比较 Paired sample comparison
ggpaired(ToothGrowth, x="supp", y="len",
         color="supp", line.color="gray",
         line.size=0.4, palette = "jco") + 
  stat_compare_means(paired = TRUE)


# 设置一个参考组进行多组比较
ggboxplot(ToothGrowth, x="dose", y="len",
          color="dose", palette = "jco") + 
  stat_compare_means(method="anova", label.y=40) + 
  stat_compare_means(label="p.signif", method="t.test",
                     ref.group = "0.5")


# 两两比较多组样本
my_comparisons <- list(c("0.5","1"), c("1", "2"),
                       c("0.5", "2"))
ggboxplot(ToothGrowth, x="dose", y="len",
          color="dose", palette = "jco") +
  stat_compare_means(comparisons = my_comparisons) + #添加成对p值
  stat_compare_means(label.y = 50) # 添加全局p值

# Base mean
# 以所有值为基准(base-mean)进行多个成对比较
# 如果出现很多组别，两两比较过于复杂，通过将所有数据汇总创建一个虚拟的样本，以它为基准进行比较。
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova", label.y = 40)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")                  # Pairwise comparison against all

# 这个方法有时候会非常有用，比如下面这个例子中，我们可以通过每个样本均值与所有样本的均值进行比较，判断基因水平是过表达还是下调了。
# Load myeloma data from GitHub
myeloma <- read.delim("https://raw.githubusercontent.com/kassambara/data/master/myeloma.txt")
# 执行检验
compare_means(DEPDC1 ~ molecular_group,  data = myeloma,
              ref.group = ".all.", method = "t.test")

ggboxplot(myeloma, x="molecular_group", y="DEPDC1",
          color="molecular_group", add="jitter",
          legend="none") + 
  rotate_x_text(angle = 45) + 
  geom_hline(yintercept = mean(myeloma$DEPDC1),
             linetype=2) + # 添加base mean的水平线
  stat_compare_means(method = "anova", label.y = 1600)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")                      # Pairwise comparison against all


# 再次分组 -- 多个分组变量
p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = supp),label = "p.signif")
# label = "p.format"
# 

# 分组柱状图加标准误差
ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
          color = "supp", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)

# 分组线图加标准误差以及显著性
ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se",
       color = "supp", palette = "jco")+
  stat_compare_means(aes(group = supp), label = "p.signif", 
                     label.y = c(16, 25, 29))


