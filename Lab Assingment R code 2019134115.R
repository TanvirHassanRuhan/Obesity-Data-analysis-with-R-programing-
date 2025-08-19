#Load necessary libraries 
library(dplyr)
library(ggplot2)
library(psych)
##Question 1
#Load the dataset 
data<-read.csv(file.choose(),header = T)
View(data)
#Read the dataset 
head(data)

## Question2
categorical_variables <- c("Gender", "family_history_with_overweight", "FAVC", "CAEC", "SCC", "CALC", "MTRANS","SMOKE")
# Calculate frequencies, percentages, and cumulative frequencies for categorical variables
for (var in categorical_variables ) {
  cat("\nSummary for:", var, "\n")
  
  freq_table <- table(data[[var]])
  percent_table <- prop.table(freq_table) * 100
  cum_freq_table <- cumsum(freq_table)
  
  print(data.frame(Frequency = freq_table, Percentage = percent_table, Cumulative = cum_freq_table))
}
#Qestion3
#present categorical variables using bar charts
categorical_vars <- c("Gender", "family_history_with_overweight", "FAVC", 
                      "CAEC", "SMOKE", "SCC", "CALC", "MTRANS")

for (var in categorical_vars) {
  p <- ggplot(data, aes_string(x = var, fill = var)) +  # Map fill to variable
    geom_bar() +
    ggtitle(paste("Bar Chart of", var)) +
    theme_minimal() +
    theme(legend.position = "right")  # Ensure legend is shown on the right
  
  print(p)  # Explicitly print the plot
}


# Pie chart of categorical variable 
categorical_vars <- c("Gender", "family_history_with_overweight", "FAVC", 
                      "CAEC", "SMOKE", "SCC", "CALC", "MTRANS")

# Loop through each categorical variable
for (var in categorical_vars) {
  tab <- table(data[[var]])  # Frequency table
  percent <- round(prop.table(tab) * 100, 1)  # Convert to percentage
  labels <- paste(names(tab), percent, "%")  # Labels with category & percentage
  
  # Create Pie Chart
  pie(tab, main = paste("Pie Chart of", var), labels = labels, 
      col = rainbow(length(tab)))
  
  # Add Legend
  legend("bottomright", legend = labels, fill = rainbow(length(tab)), cex = 0.7)
}


# Question4
install.packages("statip")
library(statip)
numerical_variables <- c('Age','Height','Weight','FCVC','NCP','CH2O','FAF','TUE')

for (x in numerical_variables) {
  cat("\n\nSummary for variable:", x, "\n")
  z <-data[[x]]
  
  # Calculate statistics
  stats_df <- data.frame(
    min = min(z),
    max = max(z),
    mean = mean(z),
    median = median(z),
    mode = mfv(z) , # Output: 3  (mfv = most frequent value)
    q1 = quantile(z, 0.25),
    q3 = quantile(z, 0.75),
    variance = var(z),
    sd = sd(z),
    cv = sd(z) / mean(z) * 100,
    iqr = IQR(z),
    cqd = (quantile(z, 0.75) - quantile(z, 0.25)) / (quantile(z, 0.75) + quantile(z, 0.25)) # Corrected CQD calculation
  )
  
  print(stats_df) # Print as a data frame for better readability
}
.#Question 5
#Present numercal variables using suitable graphs 

numerical_variables <- c('Age','Height','Weight','FCVC','NCP','CH2O','FAF','TUE')  # Replace with actual numerical variables

# Loop through numerical variables
for (i in numerical_variables) {
  tab <-data[[i]]
  
  # Create a data frame for ggplot
  df <- data.frame(Value = tab)
  
  # Histogram
  p1 <- ggplot(df, aes(x = Value)) +
    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
    ggtitle(paste("Histogram of", i)) +
    xlab(i) + ylab("Frequency") +
    theme(legend.position = "top")
  print(p1)
  
 
  # Histogram with normal curve
  p2 <- ggplot(df, aes(x = Value)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "lightgreen", color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(tab, na.rm = TRUE), 
                                           sd = sd(tab, na.rm = TRUE)), 
                  color = "red", size = 1, aes(linetype = "Normal Curve")) +
    ggtitle(paste("Histogram with Normal Curve of", i)) +
    xlab(i) + ylab("Density") +
    scale_linetype_manual(name = "Legend", values = c("Normal Curve" = "solid")) +
    theme(legend.position = "top")
  print(p2)
  # Histogram with frequency curve
  p3 <- ggplot(df, aes(x = Value)) +
    geom_histogram(bins = 20, fill = "orange", color = "black") +
    geom_density(color = "blue", size = 1, aes(linetype = "Frequency Curve")) +
    ggtitle(paste("Histogram with Frequency Curve of", i)) +
    xlab(i) + ylab("Frequency") +
    scale_linetype_manual(name = "Legend", values = c("Frequency Curve" = "solid")) +
    theme(legend.position = "top")
  print(p3)
}
  
  
# Question 6
# Explain summary measures(Normality test using Shapiro-wilk test)

# Load required libraries
library(ggplot2)
install.packages("rstatix")
library(rstatix)  # For confidence interval calculation

# Normality test with 95% Confidence Interval
for (var in numerical_variables) {
  cat("\nShapiro-Wilk test for", var, "\n")
  shapiro_test_result <- shapiro.test(data[[var]])
  
  # Compute confidence interval
  n <- length(data[[var]])
  se <- sqrt((1.0 - shapiro_test_result$statistic^2) / (n - 1))
  lower_ci <- shapiro_test_result$statistic - 1.96 * se
  upper_ci <- shapiro_test_result$statistic + 1.96 * se
  
  print(shapiro_test_result)
  cat("95% CI for W-statistic:", round(lower_ci, 4), "to", round(upper_ci, 4), "\n\n")
}

# Median, IQR, and CQD
for (var in numerical_variables) {
  z <- data[[var]]
  cat("\n\nSummary for variable:", var, "\n")
  print(cbind(
    median = median(z, na.rm = TRUE),
    iqr = IQR(z, na.rm = TRUE),
    cqd = IQR(z, na.rm = TRUE) / (quantile(z, 0.25, na.rm = TRUE) + quantile(z, 0.75, na.rm = TRUE))
  ))
}

# Boxplots using ggplot2 with legend
for (var in numerical_variables) {
  print(
    ggplot(data, aes(y = .data[[var]], fill = var)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", var),
           y = "Value",
           fill = "Variable") +
      theme_bw()
  )
}

#Question 7
#Perform subgroup analysis
install.packages("tidyverse")
library(tidyverse)
fun <- function(x,y)
{
  data %>%
    group_by(!!sym(x)) %>%
    summarise(min=min(!!sym(y)),
              max=max(!!sym(y)),
              mean=mean(!!sym(y)),
              median=median(!!sym(y)),
              mode=as.numeric(names(sort(table(!!sym(y)),decreasing=T)[1])),
              q1=as.numeric(quantile(!!sym(y),0.25)),
              q3=as.numeric(quantile(!!sym(y),0.75)),
              sd=sd(!!sym(y)),
              var=var(!!sym(y)),
              cv=sd(!!sym(y))/mean(!!sym(y))*100,
              iqr=IQR(!!sym(y)),
              cqd=IQR(!!sym(y))/(as.numeric(quantile(!!sym(y),0.25))+as.numeric(quantile(!!sym(y),0.75)))
    )
}
for(x in categorical_variables)
  for(y in numerical_variables)
  {
    print(paste(x,"X",y))
    print(fun(x,y))
  }

#Question 8
#Scatter plot between age & weight, height & weight
# Age vs Weight
library(ggplot2)
ggplot(data, aes(x = Age, y = Weight)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression with confidence interval
  ggtitle("Age vs Weight") +
  xlab("Age") +  
  ylab("Weight") +
  theme_minimal()  # Clean theme for better visualization

# Height vs Weight (Corrected order for consistency with original code)
ggplot(data, aes(x = Weight, y = Height)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "green4") +
  ggtitle("Weight vs Height") +
  xlab("Weight") +
  ylab("Height")

# Age vs Height
ggplot(data, aes(x = Age, y = Height)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue4") +
  ggtitle("Age vs Height") +
  xlab("Age") +
  ylab("Height")
#Scatter matrix
pairs(data[, c('Age','Height','Weight','FCVC','NCP','CH2O','FAF','TUE')], pch = 16) # pch=16 for filled circles
#Question 9
#Recode variable
data$MTRANS_RC <- ifelse(data$MTRANS %in% c("Walking", "Bike"), "Ownself", "Car")
data$FCVC_factor <- factor(data$FCVC, levels = c(1, 2, 3), labels = c("Never", "Sometimes", "Always"))
#Question 10
# Calculate BMI
data$BMI<-data$Weight/(data$Height^2)
data$BMI_cat<-cut(data$BMI,breaks = c(0,18.5,24.9,29.9,Inf),labels =c("Underweight","Normal","Overweight","Obisity"))
view(data)
#Question 11
#BMI for specific conditions
subset_data <- data %>% filter(Age > 30, SMOKE == "no", FAF >= 2, CH2O >= 1)
View(subset_data)
subset_data$BMI
mean(subset_data$BMI)
#Question 12
#Subset for height > 1.8m & frequent high-calorie food consumption
obesity_sub <- data %>% filter(Height > 1.8, FAVC == "yes")
view(obesity_sub)
mean(obesity_sub$BMI)
sd(obesity_sub$BMI)
#Question 13
#Correlation between (i) age & weight, (ii) height & weight iii)age & height
cor(data$Age, data$Weight, use = "complete.obs")
cor(data$Age,data$Weight)
cor(data$Height, data$Weight, use = "complete.obs")
cor(data$Height,data$Weight)
cor(data$Age, data$Height, use = "complete.obs")
cor(data$Age,data$Height)
#Question 14
#Correlation between Age and BMI
correlation_value <- cor(data$Age, data$BMI, use = "complete.obs")
correlation_value
correlation_test<- cor.test(data$Age, data$BMI,use ="complete.obs",conf.level = 0.95)
correlation_test
correlation_test<-cor.test(data$Age,data$BMI,conf.level = 0.95)
correlation_test
P_value<-correlation_test$p.value
P_value
ifelse(P_value<0.05,"significantly differ from zero","Not significantly differ from zero")
# Correlation matrix for numerical variables
cor_matrix <- cor(data[numerical_variables], use = "complete.obs")
print(cor_matrix)
# Calculate correlation matrix
correlation_matrix <- cor(data[, c("Age", "Height", "Weight")], use = "complete.obs")
# Print the correlation matrix
print(correlation_matrix)

#question 15i
b<-wilcox.test(data$Age,alternative = "two.sided",mu = 30,conf.int = TRUE, conf.level = 0.95)
b
b$p.value

print(paste('P value =',b$p.value ))
ifelse(b$p.value<=0.05,"Mean of Age is not equal to 30","Mean of age is equal to 30")
#question 15ii
b<-wilcox.test(data$Height,alternative = 'greater',mu = 1.7,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Mean of Height is greater than 1.7","Mean of Height is equal to 1.7")
#question 15iii
b<-wilcox.test(data$CH2O,alternative = "less",mu=2,conf.int = TRUE, conf.level = 0.95)
b
b$p.value 
print(paste('P valur=',b$p.value))
ifelse(b$p.value<=0.05,"Mean of CH20 is not equal to 2","Mean of CH20 is equal to 2")
#question 15iv
b<-wilcox.test(data$BMI,alternative ="less",mu =30,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value=',b$p.value))
ifelse(b$p.value<=0.05,"Mean of BMI is less than 30","Mean of BMI is equal to 30")#one sample test  

#Question 16i
b<-wilcox.test(data$Age~data$Gender,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P valu =',b$p.value))
ifelse(b$p.value<=0.05 ,"Average age significantly differs between male and female","Average age doesn't significantly differ between male and female ")
b<-wilcox.test(data$Height~data$Gender,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value=',b$p.value))
ifelse(b$p.value<=0.05,"Average height significantly differs between male and female","Average height doesn't significantly differ between male and female")
b<-wilcox.test(data$BMI~data$Gender,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Average BMI significantly differs between male and female","Average BMI doesn't significantly differ between male and female")
#Question 16 ii
b<-wilcox.test(data$Age~data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Average age significantly differs between smoker and non smoker","Average age doesn't significantly differs between smoker and non smoker")
b<-wilcox.test(data$Height~data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value=',b$p.value))
ifelse(b$p.value<=0.05,"Average height significantly differ between smoker and non smoker","Average height doestn't significantly differ between smoker and non smoker")
b<-wilcox.test(data$BMI~data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
ifelse(b$p.value<=0.05,"Average BMI significantly differ between smoker and non smoker","Average BMI doesn't significantly differ between smoker and non smoker")

#Question 17
b<-kruskal.test(Age~CAEC,data = data)
b<-kruskal.test(data$Age~data$CAEC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of age significantly differs between different groups of CAEC","Median of age doesn't significantly differs between different groups of CAEC")
b<-kruskal.test(Height~CAEC,data = data)
b<-kruskal.test(data$Height~data$CAEC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of height significantly differs between different groups of CAEC","Median of height doesn't significantly differs between different groups of CAEC")
b<-kruskal.test(BMI~CAEC,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of bmi significantly differs between different groups of CAEC","Median of bmi doesn't significantly differs between different groups of CAEC")
b<-kruskal.test(Age~CALC,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of age significantly differs between different groups of CALC","Median of age doesn't significantly differs between different groups of CALC")
b<-kruskal.test(Height~CALC,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of height significantly differs between different groups of CALC","Median of height doesn't significantly differs between different groups of CALC")
b<-kruskal.test(BMI~CALC,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of bmi significantly differs between different groups of CALC","Median of bmi doesn't significantly differs between different groups of CALC")
b<-kruskal.test(Age~MTRANS,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of age significantly differs between different groups of MTRANS","Median of age doesn't significantly differs between different groups of MTRANS")
b<-kruskal.test(Height~MTRANS,data = data)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of height significantly differs between different groups of MTRANS","Median of age doesn't significantly differs between different groups of MTRANS")
b<-kruskal.test(BMI~MTRANS,data = data)
b<-kruskal.test(data$BMI~data$MTRANS)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Median of BMI significantly differs between different groups of MTRANS","Median of BMI doesn't significantly differs between different groups of MTRANS")

#Multiple comparison 
install.packages("DescTools")
library(DescTools)
DunnTest(Age~CAEC,data = data,method = "bonferroni")
DunnTest(data$Age~data$CAEC,method = "bonferroni")
DunnTest(data$Age~data$CAEC,method = "bonferroni")
DunnTest(Age~CALC,data = data,method = "bonferroni",)
DunnTest(Age~MTRANS,data = data,method = "bonferroni")
DunnTest(Height~CAEC,data = data,method = "bonferroni")
DunnTest(Height~CALC,data = data,method = "bonferroni")
DunnTest(Height~MTRANS,data = data,method = "bonferroni")
DunnTest(BMI~CAEC,data = data,method = "bonferroni")
DunnTest(BMI~CALC,data = data,method = "bonferroni")
DunnTest(BMI~MTRANS,data = data,method = "bonferroni")
DunnTest(data$BMI~data$MTRANS,method = "bonferroni")
#Question 18 i
b<-kruskal.test(data$BMI~data$Gender)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and gender","There is no association between bmi and gender")
#Question 18 ii
b<-kruskal.test(data$BMI~data$family_history_with_overweight)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and family_history_with_overweight","There is no association between bmi and family_history_with_overweight")
#Question 18 iii
b<-kruskal.test(data$BMI~data$SMOKE)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and smoke","There is no association between bmi and smoke")
#Question 18 (iv)
b<-kruskal.test(data$BMI~data$FAVC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and FAVC","There is no association between bmi and FAVC")
#Question 18(v)
b<-kruskal.test(data$BMI~data$CAEC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and CAEC","There is no association between bmi and CAEC")
#Question 18(vi)
b<-kruskal.test(data$BMI~data$SCC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and SCC","There is no association between bmi and SCC")
#Question 18(vii)
b<-kruskal.test(data$BMI~data$CALC)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"There is association between bmi and CALC","There is no association between bmi and CALC")
#Question18(viii)
b<-kruskal.test(data$BMI~data$MTRANS)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b<=0.05,"There is association between bmi and MTRANS","There is no association between bmi and MTRANS")
#Question 19
obesity_small<-select(data,'Gender','Age','Height','Weight','family_history_with_overweight','FAVC','CH2O',
                      'SCC','FAF','TUE','CALC')
View(obesity_small)
obesity_new<-select(obesity_small,-'FAF',-'TUE',-'CALC')
View(obesity_new)

model1<-lm(Weight~Gender+Age+Height+family_history_with_overweight+FAVC+CH2O+SCC,
           data = obesity_new) 
model1
modsum<-summary(model1)
modsum
confint(model1, level = 0.95)  # 95% CI for regression coefficients
r_squared<-modsum$r.squared
r_squared
adjusted_r_squared<-modsum$adj.r.squared
adjusted_r_squared

prediction<-data.frame(Gender = 'Male',Age = 23,family_history_with_overweight='yes',Height =1.77,FAVC='yes',CH2O=1,SCC='no')
predict(model1,newdata = prediction,interval = "confidence", level = 0.95)

#Question 20
#Question 20(i)
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(MASS)
# Load the dataset
data1 <- read.csv(file.choose(),header = T)
View(data1)
# Compute BMI
data1$BMI <- data1$Weight / (data1$Height^2)
# Convert categorical variables to factors
categorical_vars <- c("Gender", "family_history_with_overweight", "FAVC", "CAEC", "SMOKE", "SCC", "CALC", "MTRANS")
data1[categorical_vars] <- lapply(data1[categorical_vars], as.factor)
# Initial full model
full_model <- lm(BMI ~ ., data = data1)
full_model

# Stepwise selection using AIC
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(stepwise_model)
confint(stepwise_model, level = 0.95)
# Model performance
AIC(stepwise_model)
# Predicted values and residuals
data1$Predicted <- predict(stepwise_model)
data1$Residuals <- residuals(stepwise_model)

# Model diagnostics
par(mfrow = c(2, 2))
par(mfrow = c(1, 1))
plot(stepwise_model)
plot(stepwise_model,5)
#Question 20(ii)
# Normality check
b<-shapiro.test(data1$Residuals) #Shapiro wilk test 
b
P_value<-b$p.value
P_value
ifelse(P_value>0.05,"Normal","Not Normal")

install.packages("lmtest")  
library(lmtest)          
# Homoscedasticity check
b<-bptest(stepwise_model) # Breusch-Pagan Test (from lmtest package)
b
p_value<-b$p.value
p_value
ifelse(p_value>0.05,"Homoscedasticity present","Heteroscedasticity present")
#Multicollinearity test 
vif(stepwise_model)
#Question 20(iii)
# Outlier detection
influence <- influence.measures(stepwise_model)
cooksD <- cooks.distance(stepwise_model)
cooksD
cooks_out<-which(cooksD >1)
cooks_out
influential <- which(cooksD > (4 / nrow(data)))
influential
cooksD<-cooks.distance(lm_model)

#Question 21
main_data<-read.csv(file.choose(),header = T)
View(main_data)
set.seed(115)
sampled_data<-main_data[sample(nrow(main_data),50,replace =F),]
sampled_data
View(sampled_data)
#Repeat 10
# Calculate BMI
sampled_data$BMI<-sampled_data$Weight/(sampled_data$Height^2)
sampled_data$BMI_cat<-cut(sampled_data$BMI,breaks = c(0,18.5,24.9,29.9,Inf),labels =c("Underweight","Normal","Overweight","Obisity"))
#Repeat 11
subset_data1 <- sampled_data %>% filter(Age > 30, SMOKE == "no", FAF >= 2, CH2O >= 1)
View(subset_data1)
mean(subset_data1$BMI)
#Repeat 12
#Subset for height > 1.8m & frequent high-calorie food consumption
obesity_sub1 <- sampled_data %>% filter(Height > 1.8, FAVC == "yes")
View(obesity_sub1)
mean(obesity_sub$BMI)
sd(obesity_sub$BMI)
#Repeat 13
#Correlation between (i) age & weight, (ii) height & weight ii)age & height
cor(sampled_data$Age, sampled_data$Weight, use = "complete.obs")
cor(sampled_data$Age,sampled_data$Weight)
cor(sampled_data$Height, sampled_data$Weight, use = "complete.obs")
cor(sampled_data$Age,sampled_data$Height, use = "complete.obs")

#Repeat 14
#Correlation between Age and BMI
correlation_value <- cor(sampled_data$Age, sampled_data$BMI, use = "complete.obs")
correlation_test<- cor.test(sampled_data$Age, sampled_data$BMI,use ="complete.obs",conf.level = 0.95)
correlation_test
print(paste("Correlation:", correlation_value))
p_value<-correlation_test$p.value
print(paste("p_value:",p_value))
# Check significance
ifelse(p_value<=0.05,"Correlation is significant (not equal to 0)","Correlation is not significant (close to 0)")

# Correlation matrix for numerical variables
numerical_variables <- c('Age','Height','Weight','FCVC','NCP','CH2O','FAF','TUE')
cor_matrix <- cor(sampled_data[numerical_variables], use = "complete.obs")
print(cor_matrix)

#repeat 16 i 
b<-wilcox.test(sampled_data$Age~sampled_data$Gender,exact=F,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05 ,"Average age significantly differs between male and female","Average age doesn't significantly differ between male and female ")
b<-wilcox.test(sampled_data$Height~sampled_data$Gender,exact =F,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value=',b$p.value))
ifelse(b$p.value<=0.05,"Average height significantly differs between male and female","Average height doesn't significantly differ between male and female")
b<-wilcox.test(sampled_data$BMI~sampled_data$Gender,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P valu =',b$p.value))
ifelse(b$p.value<=0.05,"Average BMI significantly differs between male and female","Average BMI doesn't significantly differ between male and female")
#Repeat 16 ii
b<-wilcox.test(sampled_data$Age~sampled_data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value =',b$p.value))
ifelse(b$p.value<=0.05,"Average age significantly differs between smoker and non smoker","Average age doesn't significantly differs between smoker and non smoker")
b<-wilcox.test(sampled_data$Height~sampled_data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b
b$p.value
print(paste('P value=',b$p.value))
ifelse(b$p.value<=0.05,"Average height significantly differ between smoker and non smoker","Average height doestn't significantly differ between smoker and non smoker")
b<-wilcox.test(sampled_data$BMI~sampled_data$SMOKE,conf.int = TRUE, conf.level = 0.95)
b

b$p.value
print(paste('P value = ',b$p.value))
ifelse(b$p.value<=0.05,"Average BMI significantly differ between smoker and non smoker","Average BMI doesn't significantly differ between smoker and non smoker")

