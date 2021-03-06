install.packages("userfriendlyscience")
install.packages("car")
install.packages("RVAideMemoire")
install.packages("gplots")

library(userfriendlyscience) # oneway
library(car) # leveneTest
library(RVAideMemoire) # byf.shapiro
library(gplots) # plotmeans

file.name <- "df_add_jordan_9_maxstep30.csv"
file.name <- "df_add_jordan_9_relu_maxstep30.csv"
#file.name <- "df_sub_jordan_9_relu_maxstep30.csv"
#file.name <- "df_add_jordan_9_relu_maxstep40.csv"
#file.name <- "df_sub_jordan_9_relu_maxstep40.csv"
#file.name <- "df_add_jordan_9_relu_maxstep50.csv"
#file.name <- "df_sub_jordan_9_relu_maxstep50.csv"
#file.name <- "df_add_jordan_9_relu_maxstep60.csv"
#file.name <- "df_sub_jordan_9_relu_maxstep60.csv"

#file.name <- "df_add_jordan_9_tanh_maxstep30.csv"
#file.name <- "df_sub_jordan_9_tanh_maxstep30.csv"
#file.name <- "df_add_jordan_9_tanh_maxstep40.csv"
#file.name <- "df_sub_jordan_9_tanh_maxstep40.csv"
#file.name <- "df_add_jordan_9_tanh_maxstep50.csv"
#file.name <- "df_sub_jordan_9_tanh_maxstep50.csv"
#file.name <- "df_add_jordan_9_tanh_maxstep60.csv"
#file.name <- "df_sub_jordan_9_tanh_maxstep60.csv"


df1 <- read.csv("df_add_jordan_9_maxstep30.csv")
df2 <- read.csv("df_add_jordan_9_relu_maxstep30.csv")
group_df <- read.csv(file.name)
sapply(group_df, class) 

# transform from 'integer' to 'factor'
group_df <- transform(group_df, carries = factor(carries)) 
sapply(group_df, class)


# ANOVA condition 1: Check normality ##########
# Shapiro-Wilk test to check normality.
byf.shapiro(mean_anwer_steps ~ carries, data = group_df)
# If p-value of a group is greater than 0.05, then the group follows normality.
# Otherwise, it does not follow normality.

# See the histogram of a particular number of carries.
# hist(group_df[which(group_df$carries==1),]$mean_anwer_steps)

# ANOVA condition 2: Independency ##########
# Independency is guaranteed.

# ANOVA condition 3: Homogeneity of variances ##########
# Levene's test to check the homogeneity of variances
leven.test.result <- leveneTest(mean_anwer_steps ~ carries, data = group_df)
print(leven.test.result)

# If Pr(>F) is greater than 0.05, then go to (1) and perform ANOVA and the Tukey post-hoc test.
# Otherwise, go to (2) and perform the Welch's ANOVA and the Games-Howell post-hoc test.

# (1) For equal variances ##########
# (1.1) ANOVA test for equal variances ##########
aov_model <- aov(mean_anwer_steps ~ carries, data = group_df)
summary(aov_model)
print(aov_model)

# (1.2) Post-hoc: TukeyHSD ##########
tukey.hsd <- TukeyHSD(aov_model)
print(tukey.hsd)

# (2) For equal variances ##########
# (2.1) Welch’s anova for unequal variances ##########
welch_aov_model <- oneway.test(mean_anwer_steps ~ carries, data = group_df, var.equal=TRUE)
print(welch_aov_model)

# (2.2) Post-hoc: games-howell ##########
one.way <- oneway(y = group_df$mean_anwer_steps, x = group_df$carries, posthoc = 'games-howell')
print(one.way)

# (3) Post-hoc: duncan test
#install.packages("agricolae")
#library(agricolae) 
#duncan.test(aov_model, "carries", alpha = 0.05, console = TRUE)


plotmeans(mean_anwer_steps ~ carries, data = group_df, frame = TRUE)
