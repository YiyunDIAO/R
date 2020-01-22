# load relevant packages which may be needed
library(tidyverse)
library(gridExtra)
library(dslabs)
library(ggplot2)
library(dplyr)
library(magrittr)
setwd("G:/Duke MQM/Study/Applied Statistics and Probability/Final Project")

# Step 1: Load the dataset 
dataset <- read.csv('kc_house_data.csv')

# Step 2(Data Cleaning): basic exploratory of the dataset
head(dataset)
dim(dataset)
summary(dataset)

# Data Cleaning 1 - Examine the NULL values 
new_DF <- dataset[rowSums(is.na(dataset))>0,]
dim(new_DF)
# After the examination, we figured out that there are no NULL/NA values in our dataset

# Data Cleaning 2 - Data type of the columns 
str(dataset)

# After checking the data types, we think the following 3 variables should be converted into categorical variables 
# waterfront: 1 - with waterfront, 0 - without waterfront 
# floors: we only have 6 values for floors: 1, 1.5, 2, 2.5, 3, 3.5 
# condition: we only have 5 values: 1, 2, 3, 4,5 
clean_data <- transform(dataset, waterfront=as.factor(waterfront), floors=as.factor(floors), condition = as.factor(condition), grade=as.factor(grade))
str(clean_data)
summary(clean_data)

# Data Cleaning 3 - Remove outliers of the target variable
# Explore the distribution of target variable: price 
ggplot(dataset, aes(x=price)) + geom_histogram(binwidth = 500000, boundary=50000)+ggtitle('Distribution of price') + 
  stat_bin(binwidth = 500000, boundary=50000, geom="text", aes(label=..count..),hjust = 0.5,vjust = -0.8)+
  scale_x_continuous(breaks = seq(from=0, to=8000000,by=500000))+
  theme(axis.text.x=element_text(angle=90))
# From this plot we can infer that most of the house prices are lower than $ 2,000,000 
# Therefore we need to remove those outliers in further analysis to avoid deviations caused by unusual outliers 
clean_data <- clean_data %>%
  filter(clean_data$price <= 2000000)

# Data Cleaning 4 - Remove Duplicate house ids 
dupl <- clean_data %>% 
  mutate(dup = duplicated(clean_data$id))
summary(dupl)
# We have also detected 177 duplicate ids, which is considered as primary key in this dataset.
# These 177 duplicate rows are then removed in the data cleaning process using the distinct function 
clean_data <- clean_data %>% 
  filter(!duplicated(clean_data$id))
dim(clean_data)

# After data cleaning, the dataset "clean_data" is the dataset we want for our further analysis 

# Step 3 (EDA): Explore bedrooms, bathrooms and floors vs. price 
# bedroom vs. price 
dataset_bedroom <- clean_data %>% 
  group_by(bedroom_factor=as.factor(bedrooms))
bedroom <- ggplot(dataset_bedroom, aes(x=bedroom_factor, y=price, fill=bedroom_factor))+geom_boxplot()+ggtitle("bedroom vs. price")
# Conclusion: from this plot we can get that as bedroom increases, house prices also increase
# since there are many outliers, we can also see that bedroom is not the only factor that influences house prices
# We also identified the outlier in the bedroom_factor - 33 

# bathroom vs. price 
dataset_bathroom <- clean_data %>%
  group_by(bathroom_factor=as.factor(bathrooms))
bathroom <- ggplot(dataset_bathroom, aes(x=bathroom_factor, y=price, fill=bathroom_factor)) + geom_boxplot() + ggtitle("bathroom vs. price")
# From this boxplot we can see that bathroom numbers are not all integers, they contain values like 2.25,3.75
# These values could be especially puzzling, therefore we want to see if there are not so many such values so that we can remove them
bathroom_factor=as.factor(dataset$bathrooms)
summary(bathroom_factor)
# From the summary statistics we can see that there are a large portion of such values, such as 3048 '1.75' and 5380 '2.5',
# if we remove these non-integers, they will have large influence on the accuracy of our analysis, so we decide to keep them 
# Conculsion: As bathroom increases, house prices increases 

# floors vs. price 
dataset_floor <- clean_data %>% 
group_by(floors)
floor <- ggplot(dataset_floor, aes(x=floors, y=price, fill=floors)) + geom_boxplot()+ggtitle("floor vs. price")
# conclusion: as floor increases, house prices increase, but after 2.5, house prices decrease with increasing floors 

grid.arrange(bathroom,bedroom, floor, nrow=2)

# Step 3 (EDA): Explore sqft vs. prices 
# sqft_living vs. price 
sqft_living <- ggplot(clean_data, aes(x=sqft_living, y=price)) + geom_point(alpha=0.2) + 
  geom_smooth(method='lm') + ggtitle('sqft_living vs. price')
# Conclusion: relationship clear via linear regression

# sqft_lot vs. price 
# for sqft_lot, outliers are obvious, we try to remove sqft_lot higher than 500000
# We also try to log the sqft_lot, since the data for sqft_lot is widely seperated 
dataset_lot <- clean_data %>% 
  filter(sqft_lot <=500000)
sqft_lot <- ggplot(dataset_lot, aes(x=log(sqft_lot), y=price)) + geom_point(alpha=0.2) +
  geom_smooth(method='lm') + ggtitle('sqft_lot vs. price')
# the relationship is not that clear even with log scale 

# sqft_above vs. price 
sqft_above <- ggplot(clean_data, aes(x=sqft_above, y=price)) + geom_point(alpha=0.2) + 
  geom_smooth(method ='lm') + ggtitle('sqft_above vs. price')
# obvious linear relationship, as sqft_above increases, the price increases

# sqft_basement vs. price 
# We recognized that some houses don't have basement, so we first filter those 0 data out
dataset_basement <- clean_data %>% 
  filter(sqft_basement > 0)
sqft_basement <- ggplot(dataset_basement, aes(x=sqft_basement, y=price)) + geom_point(alpha=0.2) + 
  geom_smooth(method ='lm') + ggtitle('sqft_basement vs. price')
# Linear Relationship with price  

#sqft_living15 vs. price 
sqft_living15 <-ggplot(clean_data, aes(x=sqft_living15, y=price)) + geom_point(alpha=0.2)+
  geom_smooth(method ='lm') + ggtitle('sqft_living15 vs. price')
# the correlation is very similar to that between sqft_living and price 

#sqft_lot15 vs. price
sqft_lot15<-ggplot(clean_data, aes(x=log(sqft_lot15), y=price)) + geom_point(alpha=0.2)+
  geom_smooth(method ='lm') + ggtitle('sqft_lot15 vs. price')
# correlation very similar to that of sqft_lot vs. price 

grid.arrange(sqft_living, sqft_lot, sqft_above, sqft_basement, sqft_living15, sqft_lot15, 
             nrow=3, top='price vs. sqft')

# Step 3 (EDA): explore waterfront, condition, grade, yr_built, yr_renovated vs. price 
# waterfront vs. price 
waterfront <- ggplot(clean_data, aes(x=waterfront, y=price, fill=waterfront)) + geom_boxplot()+ggtitle("waterfront vs. price")
# based on the plot it is obvious that the house with water front on average more expensive than those without 

# condition vs. price 
condition <- ggplot(clean_data, aes(x=condition, y=price, fill=condition)) + geom_boxplot() + ggtitle("condition vs. price")
# based on this we can see the better the condition, the higher the price 

# grade vs. price
grade <- ggplot(clean_data, aes(x=as.factor(grade), y=price, fill=grade)) + geom_boxplot() + ggtitle("grade vs. price")
# the higher the grade, the higher the price 

#price' yr_built vs. price 
dataset_yr <- clean_data %>% 
  mutate(year_period = case_when(
    yr_built <= 1925 ~ 'before 1925',
    yr_built > 1925 & yr_built <= 1950 ~ '1925-1950',
    yr_built > 1950 & yr_built <= 1975 ~ '1950-1975',
    yr_built > 1975 & yr_built <= 2000 ~ '1975-2000',
    yr_built > 2000 ~ 'after 2000'
  ))
yr_built <- ggplot(dataset_yr, aes(x=year_period, y=price, fill=year_period))+geom_boxplot() + ggtitle('yr_built vs. price')
# we can see that the price increase slightly with yr_built.

# the line graph shows that there is no obviouse correlation between yr_built and price, when measuring by one specific year

# yr_renovated vs. price
dataset_yr_r <- clean_data %>% 
  filter(yr_renovated != 0) %>%
  mutate(yr_period = case_when(
    yr_renovated <= 1925 ~ 'before 1925',
    yr_renovated > 1925 & yr_renovated <= 1950 ~ '1925-1950',
    yr_renovated > 1950 & yr_renovated <= 1975 ~ '1950-1975',
    yr_renovated > 1975 & yr_renovated <= 2000 ~ '1975-2000',
    yr_renovated > 2000 ~ 'after 2000'
  ))

yr_renovated <- ggplot(dataset_yr_r, aes(x=yr_period, y=price, fill=yr_period)) + geom_boxplot() + ggtitle("yr_renovated vs. price") 

grid.arrange(waterfront, condition, grade, yr_built, yr_renovated, nrow=3)

# Step 3 £¨EDA): explore zip code vs. price 
library(sf)
library(raster)
library(spData)
library(tmap)
library(zipcode)
library(ggmap)
library(maps)
counties <- map_data("county")
kingcounty<- subset(counties, region =="king county")

ggplot(clean_data,aes(long,lat)) +
  geom_polygon(data=kingcounty,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = zipcode),size=1,alpha=0.25) + scale_colour_gradient(high="dark blue", low="light blue") +
  theme_classic()

ggplot(clean_data,aes(long,lat)) +
  geom_polygon(data=kingcounty,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = price),size=1,alpha=0.25) + scale_colour_gradient(high="dark blue", low="light blue") +
  theme_classic()
# While there does seem to be a relationship between the intersection of a certain range of latitudes and longitudes and price,
# it is not linear in any way.
# We can also verify by exploring zipcode vs. price 
zip <- clean_data %>% 
  group_by(zipcode) %>%
  summarise(avg=mean(price))
ggplot(zip, aes(x=zipcode, y=avg))+ geom_point() + ggtitle("zipcode vs. price")
# This does not seem to have linear relationship. 

# Step 4(Correlation): Make the correlation plot 
library(corrplot)
correlation <- cor(clean_data[ ,c(3,4,5,6,13,20)])
corrplot(correlation)

# Step 5(Regression1): simple linear regression model 
lm1 <- lm(price ~ waterfront + floors + sqft_living + sqft_above + sqft_living15 + bedrooms + bathrooms, data=clean_data)
summary(lm1)
extractAIC(lm1)

# Step 5(Regression2): with log transformation
lm2 <- lm(price ~ waterfront + floors + log(sqft_living) + sqft_above + sqft_living15 + bedrooms + bathrooms, data=clean_data)
summary(lm2)
extractAIC(lm2)

# Step 5(Reression3) : consider interaction between floors vs. sqft_living
lm3 <- lm(price ~ waterfront + floors + sqft_living + sqft_above + sqft_living15 + bedrooms + bathrooms + floors*sqft_living, data=clean_data)
summary(lm3)
extractAIC(lm3)

# we also tried other methods like colinearity and binning to optimize the model, but the results are not that good.
# after discussion, we thought maybe we were "underfitting" the data, since we gave up too many other variables during the selection
# we want to use automatic selection to verify if we are "underfitting"
# Step 5(Regression-automatic selection)
pri <- lm(price~1, data=clean_data)
end <- lm(price~., data=clean_data)
step(pri,direction='forward',scope= ~waterfront + floors + sqft_living + sqft_above + sqft_living15 + bedrooms + bathrooms)
# the result of automatic selection is the same as our base model, which means for those 7 variables, 
# the best scenario is to include all 7 variables, we think we might be underfitting, and decide to include grade + condition 

# Step 5(Regression4): consider grade + condition 
lm4 <- lm(price ~ waterfront + floors + sqft_living + sqft_above + sqft_living15 + bedrooms + 
               bathrooms + floors*sqft_living + grade + condition, data=clean_data)
summary(lm4)
extractAIC(lm4)

# Step 5(Regression5): consider bedroom vs. bathroom, sqft_living vs. floors, sqft_living15*floors
lm5 <- lm(price ~ waterfront + floors + sqft_living + sqft_above + sqft_living15 + bedrooms + 
               bathrooms + floors*sqft_living + grade + condition + sqft_living*floors
             + sqft_living15*floors + bedrooms*bathrooms, data=clean_data)
summary(lm5)
extractAIC(lm5)

# Additional plot for those interactions included in the regression 
# explore interaction: floor vs. sqft_living
ggplot(clean_data, aes(x=sqft_living, y=price, color=floors)) + geom_point(alpha=0.1)+geom_smooth(method="lm") + ggtitle("interaction floor vs. sqft_living")
# explore interaction: floor vs. sqft_living15 
ggplot(clean_data, aes(x=sqft_living15, y=price, color=floors)) + geom_point(alpha=0.1)+geom_smooth(method="lm") + ggtitle("interaction floor vs. sqft_living15")
# explore interaction: bedroom vs. bathroom 
dataset_bathroom <- clean_data %>%
  group_by(bathroom_factor=as.factor(bathrooms)) %>% 
  mutate(bedroom_f = case_when(
    bedrooms <=4 ~ "below 4",
    bedrooms <=8 ~ "between 5-8",
    bedrooms >=9 ~ "above 9"
  ))
bathroom <- ggplot(dataset_bathroom, aes(x=bathroom_factor, y=price, fill=bedroom_f)) + geom_boxplot() + ggtitle("bathroom vs. bedroom")
show(bathroom)
