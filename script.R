##Reliability
library(readxl)
Malaysia <- read_excel("C:/Users/smontalvo/Desktop/Malaysia.xlsx", 
                       sheet = "Reliability")
View(Malaysia)
library(dplyr)

## Subset for ICC pre and post
df_pre <- subset(Malaysia, Period == "Pre", 
                 select = c("JH_1","JH_2","JH_3"))

df_post <- subset(Malaysia, Period == "Post", 
                 select = c("JH_1","JH_2","JH_3"))

library(psych)
ICC(df_pre)
ICC(df_post)



ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"),
          ylab = "Weight", xlab = "Groups")


##Main Analysis
library(readxl)
df <- read_excel("C:/Users/smontalvo/Desktop/Malaysia.xlsx", 
                       sheet = "Data")
View(df)
attach(df)
Period <- as.factor(Period)

df_pre2 <- subset(df, Period == "Pre")

##Descriptives
describeBy(df$Peak_Relative_Propulsive_Power, df$Period)


library(ggpubr)
library(ggplot2)
ggpaired(df, x = "Period", y = "Peak_Relative_Propulsive_Power", 
          color = "Period", palette = c("#00AFBB", "#E7B800"),
          order = c("Pre", "Post"),
          ylab = "Peak Relative Propulsive Power (w/kg)", xlab = "Period") + 
  stat_compare_means(method = "t.test", paired = TRUE, 
                     label.x = 1.3, label.y = 75) +
ggsave(filename = "Peak_Relative_Propulsive_Power.png")

t.test(Peak_Relative_Propulsive_Power ~ Period, data = df, paired = TRUE)

