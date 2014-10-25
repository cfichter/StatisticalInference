#####Load and Know data
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
tail(ToothGrowth)
table(ToothGrowth$dose,ToothGrowth$supp)

library(ggplot2)

# visualization of tooth growth as function of supplement type
ggplot(aes(x=supp, y=len), data=ToothGrowth) + geom_boxplot(aes(fill=supp))

#####Provide a basic summary of the data
summary(ToothGrowth)
summary(ToothGrowth[ToothGrowth$supp == "OJ",])
summary(ToothGrowth[ToothGrowth$supp == "VC",])

#####Confidence Intervals and Hypothesis Testing
#Test by Supplement
# check for group differences due to different supplement type 
# assuming unequal variances between the two groups
t.test(len ~ supp, data = ToothGrowth)

#The p-value is 0.06, and the confidence interval contains zero.
#This indicates that we can not reject the null hypothesis that
#the different supplement types have no effect on tooth length.

#Test by Dosage
  
# first create three sub-groups as per dose level pairs
ToothGrowth.doses_0.5_1.0 <- subset (ToothGrowth, dose %in% c(0.5, 1.0)) 
ToothGrowth.doses_0.5_2.0 <- subset (ToothGrowth, dose %in% c(0.5, 2.0)) 
ToothGrowth.doses_1.0_2.0 <- subset (ToothGrowth, dose %in% c(1.0, 2.0)) 

# Check for group differences due to different dose levels (0.5, 1.0)
# assuming unequal variances between the two groups
t.test(len ~ dose, data = ToothGrowth.doses_0.5_1.0)
# Check for group differences due to different dose levels (0.5, 2.0)
# assuming unequal variances between the two groups
t.test(len ~ dose, data = ToothGrowth.doses_0.5_2.0)
# Check for group differences due to different dose levels (1.0, 2.0)
# assuming unequal variances between the two groups
t.test(len ~ dose, data = ToothGrowth.doses_1.0_2.0)
