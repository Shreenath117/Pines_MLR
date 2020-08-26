# Case Study Solutions : Pines.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('Pines.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:16]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 1000 rows and 15 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

#Summarising the dataset : 
summary(data)

# Observations :
# Max spacing is for 15 and Fertilizer for 1
# There are NA's visible in the numeric data

# Check the datatypes
str(data)

sapply(data, class)

# Here : all the variables are either integer or numeric

#-------------------------------------------------------------------------------------------------
# Soln. to Question 3:

# Imputing NA's with column means

data$Hgt90[is.na(data$Hgt90)] <- mean(data$Hgt90, na.rm = T)
data$Hgt96[is.na(data$Hgt96)] <- mean(data$Hgt96, na.rm = T)
data$Diam96[is.na(data$Diam96)] <- mean(data$Diam96, na.rm = T)
data$Grow96[is.na(data$Grow96)] <- mean(data$Grow96, na.rm = T)
data$Hgt97[is.na(data$Hgt97)] <- mean(data$Hgt97, na.rm = T)
data$Diam97[is.na(data$Diam97)] <- mean(data$Diam97, na.rm = T)
data$Spread.97[is.na(data$Spread.97)] <- mean(data$Spread.97, na.rm = T)
data$Needles97[is.na(data$Needles97)] <- mean(data$Needles97, na.rm = T)

summary(data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

corrdata = data[,c(3:10)]
mydata.cor = cor(corrdata, method = c("spearman"))
mydata.cor

#-------------------------------------------------------------------------------------------------
# Soln. to Question 5:

# As observed from the previous output : 94% : Hgt96 is the most correlated variable with Hgt97

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Creating a 2-way frequency distribution table

freq_table <- table(data$Spacing,data$Fert)
freq_table

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# 15 spacing has the most number of rows out of the total observations
# With respect to Fertilizer intake (1) : 

freq_table <- table(data$Fert,data$Spacing)
freq_table

# Fertilizer Intake (1) has the most number of rows out of the total observations
# With respect to fertilizer intake (1) , 10 spacing has the most no. of observations

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

# Creating a frequency distribution table
table(data$Cover95)

counts <- table(data$Cover95)
barplot(counts, main="Thorny cover in September 1995")

# Thorny cover in September 1995 (None  has the most number of observations in the given dataset)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

#Plotting a scatterplot matrix

pairs(~Hgt90+Hgt96, data=data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

model1<-lm(data$Hgt96~data$Hgt90)
model1

# The line means that hat(height in 1996 in cm)=236.612 + 2.22(height in 1990 in cm). 
# That means that for each increase in cm in 1990 the tree in 1996 will be 2.22 cm higher in the prediction.

summary(model1)

#Looking at the summary of the simple linear model, I am not satisfied with it. 
#The Multiple R-squared & the Adjusted R Squared value is ~0.03 
# This means that the height in 1990 in cm only counts for ~3% of the tree's height in 1996 in cm variation that is explained by the linear regression model. 
#This is not enough for me to feel satisfied with the linear model. 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

# Residuals vs Fitted Line
plot(model1,which=1)

# Q-Q plot
plot(model1,which=2)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# Multiple Linear Regression Model :

model2=lm(Hgt97~Hgt90+Fert, data=data)
model2

# The line means that hat(height in 1997 in cm)=316.761 + 2.485(height in 1990 in cm) - 35.214(fertilizer use)
# That means that for each increase in cm in 1990 the tree in 1997 will be 2.485 cm higher in the prediction and 
# For each increase in fertilizer intake ; the tree in 1997 will be 35.214 cm lower in prediction

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

summary(model2)

#Looking at the summary of the simple linear model, I am not satisfied with it. 
#The Multiple R-squared & the Adjusted R Squared value is ~0.07
# This means that the height in 1990 in cm only counts for ~7% of the tree's height in 1997 in cm variation that is explained by the linear regression model. 
#This is not enough for me to feel satisfied with the linear model. 


#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# Residuals vs Fitted Line
plot(model2,which=1)

# Q-Q plot
plot(model2,which=2)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# Multiple Linear Regression Model :

model3=lm(Hgt97~Hgt96+Hgt90+Fert, data=data)
model3

# The line means that hat(height in 1997 in cm)=49.9166 + 1.0705(height in 1996) -0.0736 (height in 1990 in cm) - 1.6475 (fertilizer use)
# That means that for each increase in cm in 1996 the tree in 1997 will be ~1 cm higher in the prediction 
# For each increase in cm in 1990 the tree in 1997 will be 0.07 cm lower in the prediction 
# For each increase in fertilizer intake ; the tree in 1997 will be 1.65 cm lower in the prediction 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

summary(model3)

# Yes, this model does a much better job of predicting tree height in 1997

#Looking at the summary of the simple linear model, I am pretty satisfied with it. 
#The Multiple R-squared & the Adjusted R Squared value is 0.891
#This means that the model counts for 89% of the tree's height in 1997 in cm variation that is explained by the linear regression model. 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# Residuals vs Fitted Line
plot(model3,which=1)

# Q-Q plot
plot(model3,which=2)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# Multiple Linear Regression Model :

model4=lm(Hgt97~Hgt96+Diam97+Fert, data=data)
model4

# The line means that hat(height in 1997 in cm)=50.1179 + 0.9179(height in 1996) +6.8870 (tree trunk diameter in 1997 in cm) - 1.5023 (fertilizer use)
# That means that for each increase in cm in 1996 the tree in 1997 will be ~1 cm higher in the prediction 
# For each increase in diameter cm in 1997 the tree in 1997 will be ~7 cm higher in the prediction 
# For each increase in fertilizer intake ; the tree in 1997 will be 1.5 cm lower in the prediction 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

summary(model4)

# Yes, this model does a much better job of predicting tree height in 1997

#Looking at the summary of the simple linear model, I am pretty satisfied with it. 
#The Multiple R-squared & the Adjusted R Squared value is 0.8979
#This means that the model counts for ~90% of the tree's height in 1997 in cm variation that is explained by the linear regression model. 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Model 4 is the best fit out of the all 4 models with the highest R-Square value of 90%

# Model4 (R^2 of 0.8982) > Model 3 (R^2 of 0.8913) > Model 2 ( R^2 of 0.075) > Model 1 (R^2 of 0.028)

#-------------------------------------------------------------------------------------------------





