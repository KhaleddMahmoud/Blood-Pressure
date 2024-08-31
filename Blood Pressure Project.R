# Set the working directory
setwd("F:/Courses/Bioinformatics Diploma/Statistical Analysis and Visualisation/Labs/Project/Final Project")
# Verify the working directory has been set correctly
getwd()
# Load the .RData file
load("F:/Courses/Bioinformatics Diploma/Statistical Analysis and Visualisation/Labs/Project/Final Project/BloodPressure.RData", verbose = TRUE)
BloodPressure2 <- BloodPressure
# List files in the working directory to verify the file has been loaded
list.files()
####################################################################

#Load Packages
library(corrplot)
library(dunn.test)
library(report)
library(car)
library(multcomp)
library(ggstatsplot)
library(ggplot2)
####################################################################
##0. READING DATA

#Just analysing and skimming through the data variables
#Converted (Dose & Gender) to a factor as they are categorical variables.
View(BloodPressure)
str(BloodPressure)
BloodPressure$dose <- factor(BloodPressure$dose , labels = c('control' , 'low' , 'middle' , 'high'))
BloodPressure$gender <- factor(BloodPressure$gender ,labels=c('male' , 'female'))

View(BloodPressure)
str(BloodPressure)

#Now We can summarize our data using summary()
summary(BloodPressure)

####################################################################################################
##1. Descriptive statistics

#Summarizing data for bp.reduction as it is the only continous one
mean(BloodPressure$bp.reduction , na.rm = TRUE)
median(BloodPressure$bp.reduction , na.rm = TRUE)
min(BloodPressure$bp.reduction , na.rm = TRUE)
max(BloodPressure$bp.reduction , na.rm = TRUE)
quantile(BloodPressure$bp.reduction , na.rm = TRUE, c(0.25,0.75))

#Calculating Frequency table for both of the categorical variables (Dose & Gender)
table(BloodPressure$dose)
table(BloodPressure$gender)

#Calculate the correlation coefficient between bp-reduction and dose

#Correlation coeffecient
cor(BloodPressure$bp.reduction , as.numeric(BloodPressure$dose) , use = "complete.obs")

#Let's try Spearman as I guess it is more suitable for our data
cor(BloodPressure$bp.reduction , as.numeric(BloodPressure$dose) , use = "complete.obs" , method = "spearman")
###################################################################################################
##2. Graphics

#Generate a bar chart of a categorical variable for the gender.
barplot(table(BloodPressure$gender), main = "Bar Chart of Gender", xlab = "Gender" ,
        ylab = "Frequency" , col = c("blue", "pink"))

#Generate a bar chart graph with mean bp.reduction in  males and females.
barplot(tapply(BloodPressure$bp.reduction, list(Gender = BloodPressure$gender),
               mean, na.rm = TRUE), xlab ="Gender" , ylab="Mean BP Reduction",
        main = "Mean Blood Pressure Reduction by Gender", col = c("blue", "pink"))



#Make a histogram of a continuous variable “Dose”, “bp.reduction”.
par(mfrow=c(1,2))

hist(as.numeric(BloodPressure$dose) ,main = "Distribution of Dose",
     xlab = "Dose (mg/day)" , ylab = "Frequency",
     col = "blue" , xlim = c(0,10))

hist(BloodPressure$bp.reduction , main = "Distribution of Blood Pressure Reduction",
     xlab = "Blood Pressure Reduction (mmHg)" , ylab = "Frequency" ,
     col = "red" , xlim = c(-10 , 30))


par(mfrow = c(1, 1))#To reset the current view


#Make a scatterplot of 2 continuous variables Dose and bp.reduction, and add the regression lines for each gender
plot(as.numeric(BloodPressure$dose), BloodPressure$bp.reduction,
     main = "Scatterplot of Dose vs bp.reduction by Gender",
     xlab = "Dose (mg/day)",
     ylab = "Blood Pressure Reduction (mmHg)",
     col = "blue", pch = 16)

# Fitiing linear regression models by gender
model_male <- lm(bp.reduction ~ as.numeric(dose), data = subset(BloodPressure, gender == "male"))
model_female <- lm(bp.reduction ~ as.numeric(dose), data = subset(BloodPressure, gender == "female"))
model_male
model_female
# Adding regression lines
abline(model_male, col = "green", lty = 2)
abline(model_female, col = "red", lty = 2)

# Adding legend
legend("topright", legend = c("Male", "Female"), col = c("green", "red"), lty = 2)



#Make a boxplot of age  and a separate boxplots per Doses (as.factors). 
#The data does not include age..
boxplot(bp.reduction ~ dose, data = BloodPressure, 
        main = "Boxplots of bp.reduction by Dose",
        xlab = "Dose (mg/day)",
        ylab = "Blood Pressure Reduction (mmHg)",
        col = "lightblue")
################################################################################################################
#3. Outlier detection

#Exploring the outliers for bp.reduction

bp.outliers<- boxplot(BloodPressure$bp.reduction, plot = FALSE)$out
bp.outliers<- boxplot(BloodPressure$bp.reduction, plot = TRUE)$out
bp.outliers
#No outliers detected after confirming with the boxplot


#My interpretation
#No outliers were detected, This suggests that the data points are relatively consistent and within expected ranges. The absence of outliers enhances the reliability of our findings, indicating that the observed effects of different drug dosages on blood pressure reduction are likely representative of the broader population of high blood pressure patients sampled.

######################################################################################################
#4.Testing for normality/ homoscedasticity

# Extract the blood pressure reduction variable
bp_reduction <- BloodPressure$bp.reduction

# Normality Test
# Method 1: Shapiro-Wilk Test
shapiro_test <- shapiro.test(bp_reduction)
shapiro_test
# the p-value is 0.806, which is much greater than 0.05, so we fail to reject the null hypothesis. This suggests that the blood pressure reduction data is normally distributed.

# Method 2: Q-Q Plot
qqnorm(bp_reduction)
qqline(bp_reduction, col = "red")
# the Q-Q plot shows a normal distribution of the data.

# Homoscedasticity Test
# Method 1: Bartlett's Test
bartlett_test <- bartlett.test(bp.reduction ~ dose, data = BloodPressure)
bartlett_test
# the p-value is 0.2698, which is greater than 0.05, so we fail to reject the null hypothesis. This suggests that the variances of blood pressure reduction across different dose levels are equal.


# Method 2: Levene's Test
levene_test <- leveneTest(bp.reduction ~ dose, data = BloodPressure)
levene_test
#the p-value is 0.426, which is greater than 0.05, so we fail to reject the null hypothesis. This also suggests that the variances of blood pressure reduction across different dose levels are equal.

####################################################################
#5.Statistical Inference
# Calculate the means of bp.reduction per each Dose
means_bp_reduction <- tapply(BloodPressure$bp.reduction, BloodPressure$dose, mean)
means_bp_reduction

# Calculate the standard deviation per each Dose
sds_bp_reduction <- tapply(BloodPressure$bp.reduction, BloodPressure$dose, sd)
sds_bp_reduction 

## Calculate the number of samples  per each Dose
n_samples <- tapply(BloodPressure$bp.reduction, BloodPressure$dose, length)
n_samples
x <- bp_reduction
x

#Calculate the intervals for the means of bp.reduction per each Dose

#Calculate the 90%  confidence interval
Left.value01 <- means_bp_reduction - 1.64 * sds_bp_reduction / sqrt(n_samples)
Right.value01 <- means_bp_reduction + 1.64 * sds_bp_reduction / sqrt(n_samples)
Left.value01
Right.value01

#plotting the 4 intervals
n.intervals <- 4
plot(c(mean(x),mean(x)),c(1,n.intervals),col="blue",typ="l",ylab="Samples",xlab="Interval",xlim=c(0,100))
segments(Left.value01[1:n.intervals],1:n.intervals,Right.value01[1:n.intervals], 1:n.intervals, lwd =0.4)
outside <- ifelse(Left.value01 > mean(x) | Right.value01 < mean(x), 2, 1)
segments(Left.value01[1:n.intervals],1:n.intervals,Right.value01[1:n.intervals],1:n.intervals, col=outside, lwd =1)
length(which(cbind(Left.value01 <= mean(x) & Right.value01 >= mean(x)) == TRUE))
#25%% as we have  4 and the result is  1


#Calculate the 95%  confidence interval
Left.value02 <- means_bp_reduction - 1.98 * sds_bp_reduction / sqrt(n_samples)
Right.value02 <- means_bp_reduction + 1.98 * sds_bp_reduction / sqrt(n_samples)
Left.value02
Right.value02

#plotting the 4 intervals
n.intervals <- 4
plot(c(mean(x),mean(x)),c(1,n.intervals),col="green",typ="l",ylab="Samples",xlab="Interval",xlim=c(0,100))
segments(Left.value02[1:n.intervals],1:n.intervals,Right.value02[1:n.intervals], 1:n.intervals, lwd =0.4)
outside <- ifelse(Left.value02 > mean(x) | Right.value02 < mean(x), 2, 1)
segments(Left.value02[1:n.intervals],1:n.intervals,Right.value02[1:n.intervals],1:n.intervals, col=outside, lwd =1)
length(which(cbind(Left.value02 <= mean(x) & Right.value02 >= mean(x)) == TRUE))
#25%% as we have  4 and the result is  1


#Calculate the 99%  confidence interval
Left.value03 <- means_bp_reduction - 2.57* sds_bp_reduction / sqrt(n_samples)
Right.value03 <- means_bp_reduction + 2.57 * sds_bp_reduction / sqrt(n_samples)
Left.value03
Right.value03

#plotting the 4 intervals
n.intervals <- 4
plot(c(mean(x),mean(x)),c(1,n.intervals),col="red",typ="l",ylab="Samples",xlab="Interval",xlim=c(0,100))
segments(Left.value03[1:n.intervals],1:n.intervals,Right.value03[1:n.intervals], 1:n.intervals, lwd =0.4)
outside <- ifelse(Left.value03 > mean(x) | Right.value03 < mean(x), 2, 1)
segments(Left.value03[1:n.intervals],1:n.intervals,Right.value03[1:n.intervals],1:n.intervals, col=outside, lwd =1)
length(which(cbind(Left.value03 <= mean(x) & Right.value03 >= mean(x)) == TRUE))
#25%% as we have  4 and the result is  1


# Observations on interval width:
#As we increase the confidence level (from 90% to 99%), the width of the confidence interval increases. This is because higher confidence levels require a wider range to ensure that the true mean lies within the interval.  

####################################################################
####################################################################
#5. Hypothesis Testing part one
#Performing 2 sample paired t.test assuming normality and homoscedasticity
control_group <- BloodPressure[BloodPressure$dose == "control",]
t.test(bp.reduction ~ gender, data = control_group,var.equal = TRUE)
#We have enough evidence to reject null hypothesis in support of alternative ,there is significant different between male and female blood pressure under control dose
#Testing normality for male control dose
hist(BloodPressure$bp.reduction[BloodPressure$gender == "male"],main = "male_hist",xlab = "male_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$gender == "male"],main = "male_bp")
qqline(BloodPressure$bp.reduction[BloodPressure$gender == "male"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$gender == "male"])
#We dont have enough evidence to reject null hypothesis , normal male data
#Testing normality for female control dose
hist(BloodPressure$bp.reduction[BloodPressure$gender == "female"],main = "female_hist",xlab = "female_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$gender == "female"],main = "female_bp")
qqline(BloodPressure$bp.reduction[BloodPressure$gender == "female"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$gender == "female"])
#We dont have enough evidence to reject null hypothesis , normal female data
#Checking variance 
boxplot(BloodPressure$bp.reduction[BloodPressure$gender == "male"],BloodPressure$bp.reduction[BloodPressure$gender == "female"])
var.test(bp.reduction~gender, data=BloodPressure)   #F test of equal variance
#We dont have enuogh evidence to reject null hypothesis ,the variance between male and female is equal

###################################################################################################################

#Part 2
#Performing 2 sample welch t.test assuming heteroscedasticity
high_group <- BloodPressure[BloodPressure$dose == "high",]
t.test(high_group$bp.reduction,control_group$bp.reduction,alternative = "greater",var.equal = FALSE)
#We have enough evidence to reject null hypothesis in support of alternative,so blood pressure reduction under high dose(10) is higher than blood pressure under control dose
#Testing assumptions#######################
#Testing normality of blood pressure under high dose 
hist(high_group$bp.reduction,main = "High_dose",xlab = "Blood Pressure")
qqnorm(high_group$bp.reduction,main = "High_dose")
qqline(high_group$bp.reduction)
shapiro.test(high_group$bp.reduction)
#We dont have enough evidence to reject null hypothesis, normal data for high dose
#Testing normality of blood pressure under control dose
hist(control_group$bp.reduction,main = "Control_dose",xlab = "Blood Pressure")
qqnorm(control_group$bp.reduction,main = "Control_dose")
qqline(control_group$bp.reduction)
shapiro.test(control_group$bp.reduction)
#We dont have enough evidence to reject null hypothesis, normal data for control dose
#Checking variance
boxplot(high_group$bp.reduction,control_group$bp.reduction)
var.test(high_group$bp.reduction,control_group$bp.reduction)
var.test(BloodPressure$bp.reduction[BloodPressure$dose == "high"],BloodPressure$bp.reduction[BloodPressure$dose == "high"])
#We dont have enough evidence to reject null hypothesis, equal variance
#Performing t.test assuming equal variance
t.test(high_group$bp.reduction,control_group$bp.reduction,alternative = "greater",var.equal = TRUE)
#We dont have enough evidence to reject null hypothesis, equal variance
#############################################################################################

#Part 3
#Testing assumptions#######################
#Checing variance 
ggplot(BloodPressure) +
  aes(x = dose, y = bp.reduction, color = "bp.reduction") +
  geom_boxplot()

plot(bp.reduction~dose,data = BloodPressure,main="equal variances")

ggbetweenstats(
  data = BloodPressure,
  x = dose,
  y = bp.reduction,
  type = "parametric",
  var.equal = TRUE,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  centrality.plotting = T,
  bf.message = FALSE,
)

leveneTest(bp.reduction~dose,data=BloodPressure)
#We dont have enough evidence to reject null hypothesis, equal variance groups

#Testing normality for blood pressure under control dose 
hist(BloodPressure$bp.reduction[BloodPressure$dose == "control"],main = "control_hist",xlab = "control_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$dose == "control"],main = "control_qq")
qqline(BloodPressure$bp.reduction[BloodPressure$dose == "control"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$dose == "control"])
#We dont have enough evidence to reject null hypothesis, normal data for control dose
#Testing normality for blood pressure under low dose 
hist(BloodPressure$bp.reduction[BloodPressure$dose == "low"],main = "low_hist",xlab = "low_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$dose == "low"],main = "low_qq")
qqline(BloodPressure$bp.reduction[BloodPressure$dose == "low"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$dose == "low"])
#We dont have enough evidence to reject null hypothesis, normal data for low dose
#Testing normality for blood pressure under middle dose 
hist(BloodPressure$bp.reduction[BloodPressure$dose == "middle"],main = "middle_hist",xlab = "middle_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$dose == "middle"],main = "middle_qq")
qqline(BloodPressure$bp.reduction[BloodPressure$dose == "middle"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$dose == "middle"])
#We dont have enough evidence to reject null hypothesis, normal data for middle dose
#Testing normality for blood pressure under high dose 
hist(BloodPressure$bp.reduction[BloodPressure$dose == "high"],main = "high_hist",xlab = "high_bp")
qqnorm(BloodPressure$bp.reduction[BloodPressure$dose == "high"],main = "high_qq")
qqline(BloodPressure$bp.reduction[BloodPressure$dose == "high"])
shapiro.test(BloodPressure$bp.reduction[BloodPressure$dose == "high"])
#We dont have enough evidence to reject null hypothesis, normal data for high dose

#Performing comparison between the different groups by:
#ANOVA TEST
ANOVA_MODEL <- aov(bp.reduction~dose,data = BloodPressure)
ANOVA_MODEL
summary(ANOVA_MODEL) #to know p-value
report(ANOVA_MODEL)
coef(ANOVA_MODEL)
#We have enough evidence to reject null hypothesis in support of alternative, so there is signficant difference between groups
#Tukey honest test(post hoc test)
composition <- TukeyHSD(ANOVA_MODEL)
composition
plot(composition)
summary(glht(ANOVA_MODEL,linfct = mcp(dose="Tukey")))
plot(glht(ANOVA_MODEL,linfct = mcp(dose="Tukey")))
#Dunnet test
summary(glht(ANOVA_MODEL,linfct = mcp(dose="Dunnett")))
plot(glht(ANOVA_MODEL,linfct = mcp(dose="Dunnett")))
#Bonferronitest
pairwise.t.test(BloodPressure$bp.reduction,BloodPressure$dose,p.adjust.method = "bonf")
#There is significant differenct between all groups except betwwen miidle and low, there is small similarity
##########################################################################################################
### Linear Regression part ###
### First Question in Regression ###

# Subset data based on dose levels
control_data <- subset(BloodPressure, dose == 'control')
dosage_10mg <- subset(BloodPressure, dose == 'high')

# Combine subsets
New_data <- rbind(dosage_10mg, control_data)

# Fit models
model1 <- lm(bp.reduction ~ gender, data = BloodPressure)
summary(model1)

model2 <- lm(bp.reduction ~ dose, data = New_data)
summary(model2)

model3 <- lm(bp.reduction ~ dose, data = BloodPressure)
summary(model3)

### Second Question ###
# Confidence intervals
confint(model1, level = 0.95)
confint(model2, level = 0.95)

confint(model3, level = 0.95)


### Third Question ###

# Correct prediction with model3 which includes all levels of dose
dose_3mg_2 <- data.frame(dose = 'middle') # Use a valid factor level name
bp_reduction_3mg_2 <- predict(model3, newdata = dose_3mg_2)
bp_reduction_3mg_2

