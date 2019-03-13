#group_df <- read.csv("df_add_jordan_9_maxstep30.csv")
group_df <- read.csv("df_sub_jordan_9_maxstep30.csv")

sapply(group_df, class) 

# transform from 'integer' to 'factor'
group_df <- transform(group_df, carries = factor(carries)) 
sapply(group_df, class)

# (1) ANOVA test
aov_model <- aov(mean_anwer_steps ~ carries, data = group_df)
summary(aov_model)

#install.packages("agricolae")
#library(agricolae) 
#duncan.test(aov_model, "carries", alpha = 0.05, console = TRUE)

TukeyHSD(aov_model)
