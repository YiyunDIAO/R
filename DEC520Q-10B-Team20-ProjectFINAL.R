setwd("G:/Duke MQM/Study/Data Science for Business/Team Project")
# read the data 
watson <- read.csv("Watson.csv")
# Basic exploration of the dataset 
summary(watson)
str(watson)

##### Data Preparation #####
# After checking the datasets, we found out that our dataset is actually quite clean
# there is no NA and all the data types are clean, so we don't need to further clean the datasets
# For usage, we make Response=1 if there is "Yes" and 0 if there is "no" 
library(dplyr)
library(tidyverse)
watson$Response <- as.factor(case_when(
  watson$Response=="No"~ 0,
  watson$Response=="Yes"~1
))


##### EDA Plotting ##### 

marketing <- watson

library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#Response Type based on Education Level
plot7 <- ggplot(marketing, aes(x=Education)) + geom_bar(aes(fill = Response), position = "dodge")
plot7 <- plot7 + labs(title = "Response Type based on Education Level", x= "Education Level", y= "Number of Responses")
plot7<-plot7 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))
plot7

#Response Type based on Income 
plot12 <- ggplot(marketing, aes(x=Response, y=Income, fill=Response)) 
plot12<- plot12 + geom_boxplot() + labs(title = "Response Type based on Income", x= "Response Type", y= "Income")
plot12<-plot12 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))
plot12

figure <- ggarrange(plot7, plot12, ncol = 2, nrow = 1)
figure

#Response Type based on Number of Open Complaints
plot20 <- ggplot(marketing, aes(x=as.factor(Number.of.Open.Complaints))) + geom_bar(aes(fill = Response), position = "dodge")
plot20 <- plot20 + labs(title = "Response Type based on Number of Open Complaints", x= "Number of Open Complaints", y= "Number of Responses")
plot20<- plot20 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))

#Response Type based on Total Claim Amount
plot26 <- ggplot(marketing, aes(x=Response, y=Total.Claim.Amount, fill=Response)) 
plot26<- plot26 + geom_boxplot() + labs(title = "Response Type based on Total Claim Amount", x= "Response Type", y= "Total Claim Amount")
plot26<- plot26 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))

#Response Type based on Months since Policy Inceptions
plot19 <- ggplot(marketing, aes(x=Response, y=Months.Since.Policy.Inception, fill=Response))
plot19<- plot19 + geom_boxplot() + labs(title = "Response Type based on Months since Policy Inception", x= "Response Type", y= "Months Since Policy Inception")
plot19<- plot19 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))

#Response Type based on Monthly Payment
plot17 <- ggplot(marketing, aes(x=Response, y=Monthly.Premium.Auto, fill=Response))
plot17<- plot17 + geom_boxplot() + labs(title = "Response Type based on Monthly Payment", x= "Response Type", y= "Monthly Payment")
plot17<- plot17 + scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod2"))

figure1 <- ggarrange(plot20, plot26, plot19, plot17, ncol = 2, nrow = 2)
figure1

##### Model Building ##### 
### in the following parts, we are going to build different models for the prediction of Response Probability 
### Model 1: Simple Logistic Regression 
# here we didn't include Customer and Effective.To.Date because in reality it makes no sense to predict the response rate using these 2 variables
lm1 <- glm(formula = Response ~ . - Customer-Effective.To.Date, family = "binomial", data = watson)
summary(lm1)

# We use the 1st model to predict the probability of response 
Prob1 <- predict(lm1, newdata = watson, type="response")
# We set threshold 0.5 to predict either 1 or 0 
val = 0.5
result1 <- case_when(
  Prob1>=val~1,
  Prob1<val~0
)

### Model 2: Classification Tree 
# install relevant packages 
install.packages("tree")
library("tree")
install.packages("partykit")
library("partykit")

lm2<- tree(formula = Response~.-Customer-Effective.To.Date, data=watson)
summary(lm2)
lm2
# plot the classification tree 
plot(lm2)
text(lm2, label="yval")
text(lm2,label="yprob")

# predict the probability and then binary results 
prob2 <- predict(lm2, newdata = watson)
result2 <- case_when(
  prob2[,2] >= val ~ 1,
  prob2[,2]< val ~ 0)

### Model 3: Random Forest 
install.packages("randomForest")
library(randomForest)

lm3 <- randomForest(formula=Response~.-Customer-Effective.To.Date, data=watson)
prob3 <- predict(lm3, newdata = watson, type="prob")
result3 <- case_when(
  prob3[,2] >= val ~1,
  prob3[,2] < val ~0
)

##### Model Evaluation ##### 
### Evaluation of im-sample Performance based on TPR/FPR and Accuracy 
# TPR/FPR 
source("DataAnalyticsFunctions.R")

Performance1 <- FPR_TPR(result1==1, watson$Response==1)
Performance2 <- FPR_TPR(result2==1, watson$Response==1)
Performance3 <- FPR_TPR(result3==1, watson$Response==1)


insPerform <- rbind(Performance1, Performance2, Performance3)

# we could see that random forest has perfect in-sample performance, which is highly possible due to overfitting

### Evaluation of OSS Performance 
### k-fold cross-validation 
# here we would like to use 10-fold cross-validation to check the OSS performance
nfold <- 10
n <- nrow(watson)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
watson_fold <- watson %>% 
  mutate(foldid=as.factor(foldid))
# create a dataframe to store the performance results 
store1 <- data.frame(Total=rep(NA,10),FPR=rep(NA,10),TPR=rep(NA,10),ACC=rep(NA,10))
store2 <- data.frame(Total=rep(NA,10),FPR=rep(NA,10),TPR=rep(NA,10),ACC=rep(NA,10))
store3 <- data.frame(Total=rep(NA,10),FPR=rep(NA,10),TPR=rep(NA,10),ACC=rep(NA,10))

for(k in 1:nfold){
  training <- watson_fold %>% 
    filter(foldid != k)
  testing <- watson_fold %>% 
    filter(foldid == k)
  # train the model
  oslm1 <- glm(formula = Response ~ ., family = "binomial", data = training[,c(2:6,8:24)])
  oslm2 <- tree(formula = Response~.-Customer-Effective.To.Date, data=training)
  oslm3 <- randomForest(formula=Response~.-Customer-Effective.To.Date, data=training)
  
  # predict the probability of testing sample 
  osprob1 <- predict(oslm1, newdata = testing, type="response", se.fit = FALSE)
  osprob2 <- predict(oslm2, newdata = testing)
  osprob3 <- predict(oslm3, newdata = testing, type="prob")
  
  # predict the actual results 
  osresult1 <- case_when(
    osprob1 >= val ~ 1,
    osprob1 < val ~ 0
  )
  osresult2 <- case_when(
    osprob2[,2] >= val ~ 1,
    osprob2[,2] < val ~ 0
  )
  osresult3 <- case_when(
    osprob3[,2] >= val ~ 1,
    osprob3[,2] < val ~ 0
  )
  
  OSPerformance1 <- FPR_TPR(osresult1==1, testing$Response==1)
  OSPerformance2 <- FPR_TPR(osresult2==1, testing$Response==1)
  OSPerformance3 <- FPR_TPR(osresult3==1, testing$Response==1) 
  
  store1$Total[k] <- OSPerformance1$Total
  store1$FPR[k] <- OSPerformance1$FPR
  store1$TPR[k] <- OSPerformance1$TPR
  store1$ACC[k] <- OSPerformance1$ACC
 
  store2$Total[k] <- OSPerformance2$Total
  store2$FPR[k] <- OSPerformance2$FPR
  store2$TPR[k] <- OSPerformance2$TPR
  store2$ACC[k] <- OSPerformance2$ACC   
  
  store3$Total[k] <- OSPerformance3$Total
  store3$FPR[k] <- OSPerformance3$FPR
  store3$TPR[k] <- OSPerformance3$TPR
  store3$ACC[k] <- OSPerformance3$ACC
  }

store1$model <- rep("Logistic Regression",10)
store2$model <- rep("Classification Tree",10)
store3$model <- rep("Random Forest",10)
store <- rbind(store1,store2,store3)

library(ggplot2)
ggplot(data=store, aes(x=as.factor(model),y=ACC))+geom_boxplot() + 
  labs(title ="OOS Performance:Accuracy", x = "Model Type", y = "Accuracy") + 
  theme(plot.title = element_text(hjust = 0.5))

# Regarding Accuracy, Random Forest is much better than the other 2 models, 
# so we would like to use random forest for the prediction of probability

##### Business Deployment #####
### Clustering ### 
# here we define risk: probability of not answering the renew call 
watson$risk <- prob3[,1]
ggplot(data=watson, aes(x=risk, y=Customer.Lifetime.Value))+geom_point()
# our goal is to cluster customrs into 4 groups based on value and risk 
seg <- watson[,c(1,3,25)]
scale_seg <- scale(seg[,2:3])
cus_seg <- kmeans(scale_seg, 4, nstart = 10)

ggplot(data=seg, aes(x=risk, y=Customer.Lifetime.Value, color=cus_seg$cluster)) +
  geom_point() + scale_fill_continuous(name = "Clusters") + 
  labs(title ="Customer Segmentation Based on Risk and CLV", x = "Risk", y = "Customer Lifetime Value", color = "Clusters") + 
  theme(plot.title = element_text(hjust = 0.5))

# Using Information Criteria to choose number of clusters 
seg2 <- lapply(1:30, function(k) kmeans(scale_seg, k, nstart=10))
kaic <- sapply(seg2,kIC)
kbic  <- sapply(seg2,kIC,"B")
kHDic  <- sapply(seg2,kIC,"C")

#plotting IC
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)
# Vertical line where AIC is minimized
abline(v=which.min(kaic))
# Next we plot BIC
lines(kbic, col=4, lwd=2)
# Vertical line where BIC is minimized
abline(v=which.min(kbic),col=4)
# Next we plot HDIC
lines(kHDic, col=3, lwd=2)
# Vertical line where HDIC is minimized
abline(v=which.min(kHDic),col=3)

# Insert labels
text(c(which.min(kaic),which.min(kbic),which.min(kHDic)),c(mean(kaic),mean(kbic),mean(kHDic)),c("AIC","BIC","HDIC"))
# both AICc and BIC chose more complicated models

# the results (how many clusters?)
which.min(kaic) #29
which.min(kbic) #17
which.min(kHDic) #6

### Customer Group Description ####

watson$cluster = cus_seg$cluster

# stacked bar plot of education (bachelor and above vs some college and below)
watson$education2 = ifelse(watson$Education == "Bachelor" | 
                             watson$Education == "Doctor" | 
                             watson$Education == "Master", 1, 0)
s1 = ggplot(watson, aes(cluster, fill = (as.factor(education2))))
s1 + geom_bar(position = "fill") + scale_fill_discrete(name = "Customers' Education Level", 
                                                       labels = c("Bachelor and Above", "Some College and Below")) + 
  labs(title ="Education Level Across Clusters", x = "Clusters", y = "Percentage of Customers") +
  scale_y_continuous(labels = scales::percent) + 
  theme(plot.title = element_text(hjust = 0.5))

# stacked bar plot of employment status (retirement vs everything else)
watson$employmentstatus2 = ifelse(watson$EmploymentStatus == "Retired",
                                  1, 0)
s2 = ggplot(watson, aes(cluster, fill = (as.factor(employmentstatus2))))
s2 + geom_bar(position = "fill") + scale_fill_discrete(name = "Employment Status",
                                                       labels = c("Employed, Unemployed, Disabled, or Medical Leave", "Retired")) + 
  labs(title ="Customers' Employment Status", x = "Clusters", y = "Percentage of Customers") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5))

# stacked bar plot of location code (rural and urban vs suburban)
watson$location.code2 = ifelse(watson$Location.Code == "Rural" | 
                                 watson$Location.Code == "Urban", 1, 0)
s3 = ggplot(watson, aes(cluster, fill = as.factor(location.code2)))
s3 + geom_bar(position = "fill") + scale_fill_discrete(name = "Community Type",
                                                       labels = c("Rural or Urban", "Suburban")) + 
  labs(title ="Customers' Community Type", x = "Clusters", y = "Percentage of Customers") +
  scale_y_continuous(labels = scales::percent) + 
  theme(plot.title = element_text(hjust = 0.5))


### Solution ###

# After getting those insights, we want to use causal modelling to discuss the solutions 
group1 <- watson%>% 
  filter(cluster==1)
group2 <- watson%>% 
  filter(cluster==2)
group3 <- watson%>% 
  filter(cluster==3)
group4 <- watson%>% 
  filter(cluster==4)

### Solution 1: How to keep high risk high value customers 
# There is sth we could maybe change: Number of Open Claims, Monthly Premium, 
y <- watson$risk
x <- model.matrix(risk~.-Customer-Customer.Lifetime.Value, data = watson)
d1 <- watson$Monthly.Premium.Auto
d2 <- watson$Number.of.Open.Complaints
source("causal_source.R")
install.packages("glmnet")
library(glmnet)
CausalLinear(y, d1, x)
CausalLinear(d1,y,x)
CausalLinear(y, d2, x)
# We found out that Monthly.Premium and Number of Open Claims have negative impact on risk, so there is actually not much we could do 
# we want to use linear regression to draw some useful insights 
risklm <- lm(risk~Sales.Channel+Number.of.Open.Complaints+Monthly.Premium.Auto, data=watson)
summary(risklm)
# This linear regression is not that useful for making causal statement, but it gives us an important assumption to test:

# Will agent sales be more useful for reducing the risks?? 
watson$Sales.Channel <- case_when(
  watson$Sales.Channel == "Agent"~1,
  TRUE~0
)
D <- as.numeric(watson$Sales.Channel)
CausalLinear(y,D,x)

# it could be better to choose agent sales than other sales channel 
# we would also like to visualize this effect
channel_effect <- aggregate(risk~Sales.Channel, FUN=mean, data=watson)
colnames(channel_effect)[1] <- "Agent Sales or not"
channel_effect
# proposal: try sales channel Agent 



### Solution 2: How to price higher for existing customers: which is cluster 4 in this case 

#################  Note for TAs who review the code ##################
# cluster numbers are different everytime a k-mean cluster runs so we have to change the which group we pick 
# if the low risk customers are clustered as group4, then we should pick group4 here 
# if clustered as group 1, then we should prick group1 here 
# We want to price higher among existing customers 
CurrentRevenue <- sum(group4$Monthly.Premium.Auto)
CurrentRevenue

simulation <- data.frame(increase = rep(NA, 50), Revenue_increase = rep(NA, 50), customer_lose=rep(NA, 50), CLV_lose=rep(NA,50))

# what if we increase the price by 10% 
for(i in 1:50){
  increase <- i/100
  newgroup1 <- group4
  newgroup1$Monthly.Premium.Auto <- group4$Monthly.Premium.Auto*(1+increase)
  newgroup1$Response <- predict(lm3, newdata = newgroup1, type = "response")
  summary(newgroup1)
  newgroup1 <- newgroup1 %>% 
    filter(Response==1)
  NewRevenue <- sum(newgroup1$Monthly.Premium.Auto)
  CLVLose <- sum(group4$Customer.Lifetime.Value)-sum(newgroup1$Customer.Lifetime.Value)
  simulation$increase[i]=increase
  simulation$Revenue_increase[i]=NewRevenue-CurrentRevenue
  simulation$customer_lose[i]=nrow(group4)-nrow(newgroup1)
  simulation$CLV_lose[i] = CLVLose
  }
# Visualize the results 
simu1 <- ggplot()+geom_col(mapping=aes(x=simulation$increase,y=simulation$Revenue, fill=1-simulation$increase))+geom_line(mapping=aes(x=simulation$increase, y=simulation$customer_lose*5000))
simu1 <- simu1 + scale_y_continuous(name="Achieved Revenue", sec.axis = sec_axis(~.*10000, name="Number of Customers Lost", labels = function(b){paste0(b/50000000)}))
simu1 <- simu1 + ggtitle("Simulation of Pricing Effect on Monthly Base") + scale_x_continuous(name="Price increase")
simu1 <- simu1 + geom_area(mapping=aes(x=simulation$increase, y=simulation$CLV_lose), fill="light blue", alpha=0.5)
simu1 <- simu1 + annotate(label="CLV lose",x=0.25, y=90000, geom="text")+annotate(label="Num of Customers lost", x=0.25, y=52000, geom="text")
simu1


