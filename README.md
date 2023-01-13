## Packages

```{r}
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(class)
library(caret)
library(corrplot)
```

## Loading dataset

I found this dataset on: https://www.kaggle.com/datasets/prachi13/employeeattritionrate. It is about the employee dropout rate. 

Below, I will load the dataset:

```{r}
Attrition <- read_csv(file="C:/Users/foeke/OneDrive - Office 365 Fontys/Semester 7/R github/data-mining-s1y2223-Jurrefoekens/datasets/Attrition-Data.csv")
```

## Business Understanding

The dataset contains information about employees at a company and whether or not they have left the company. This information can be used by a business to identify and address factors that affect employee retention and predict which employees are likely to leave. By understanding and addressing these factors, a business can keep its workers and make them more efficient, resulting in more money.

## Data Understanding

Below, I will show the data and then explain it:

```{r}
str(Attrition)
```

The dataset has 13 variables/columns and 1470 oberservations/rows. Each row in dataset represents an employee. 

Below I will explain every column:

- Age: The age of the employee.
- Attrition: Whether or not the employee has left the company.
- Department: The department the employee works in.
- DistanceFromHome: The distance of the employee's home from the office.
- Education: The level of education of the employee.
- EducationField: The field of education of the employee.
- EnvironmentSatisfaction: The employee's satisfaction with the working environment.
- JobSatisfaction: The employee's satisfaction with their job.
- MaritalStatus: The marital status of the employee.
- MonthlyIncome: The employee's monthly income.
- NumCompaniesWorked: The number of companies the employee has worked for before.
- WorkLifeBalance: The employee's work-life balance.
- YearsAtCompany: The number of years the employee has worked at the company.

Below, I will summarise the data using the code and explain how the data fits together:

```{r}
summary(Attrition)
```

- Age: The age of the employees ranges from 18 to 60 with a mean of 36.92.
- Attrition: 237 employees have left the company and 1233 employees are still working there.
- Department: The majority of employees work in the Research & Development department.
- DistanceFromHome: The distance of employees' homes from the office ranges from 1 to 29 miles with a mean of 9.19 miles.
- Education: 
- EducationField: The majority of employees have a background in life sciences.
- EnvironmentSatisfaction: The employees are generally satisfied with their environment.
- JobSatisfaction: The employees are generally satisfied with their job.
- MaritalStatus: The most employees are married.
- MonthlyIncome:
- NumCompaniesWorked:
- WorkLifeBalance: The employees have different levels of work-life balance, with a range from 1 to 4 and a mean of 2.72. Which means that the WorkLifeBalance overall is good.
- YearsAtCompany: The number of years that the employees have worked at the company ranges from 0 to 40 and a mean of 7.01.


Since this is about attrition, I would like to see how often there is attrition and how often there isn't.

```{r}
attrition.ratio = table(Attrition$Attrition)
attrition.ratio

attrition.prop = prop.table(attrition.ratio)
attrition.prop   
```

The table shows that 1233 employees are staying and 237 leaving employees left the company. This means that about 84% of employees stayed and 16% left the company.

First, I will use visualisations to investigate what kind of people leave the company most often and whether I can already see something of a correlation in this. 

```{r}
plottable1=table(Attrition$Attrition,Attrition$Education)
plottable2=table(Attrition$Attrition,Attrition$EnvironmentSatisfaction)
plottable3=table(Attrition$Attrition,Attrition$YearsAtCompany)
plottable4 = table(Attrition$Attrition, Attrition$Department)
plottable5 = table(Attrition$Attrition, Attrition$MaritalStatus)
plottable6 = table(Attrition$Attrition, Attrition$Age)

barplot(plottable1, main="Employees left vs Education", xlab="Education Level",col=c("Red","Green"),legend=rownames(plottable1),beside = TRUE)
barplot(plottable2, main="Employees left vs Environment Satisfaction", xlab="Evoriment Satisfaction Level", col=c("Red","Green"),legend=rownames(plottable2), beside = TRUE)
barplot(plottable3, main="Employees left vs Num of Years at Company", xlab="Num of Years", col=c("Red","Green"),legend=rownames(plottable3),beside = TRUE)
barplot(plottable4, main="Employees left vs Department", xlab="Department ", col=c("Red","Green"),legend=rownames(plottable4),beside = TRUE)
barplot(plottable5, main="Employees left vs MaritalStatus", xlab="Marital Status ", col=c("Red","Green"),legend=rownames(plottable5),beside = TRUE)
barplot(plottable6, main="Employees left vs Age", xlab="Age", col=c("Red","Green"),legend=rownames(plottable6),beside = TRUE)
```

- Education: Those with a bachelors degree (level 3) have the highest attrition. Important to note with this is that there are few people who have a doctoral degree (level 5). This could have an impact on the amount that left in the Doctorate category.
-Environment satisfaction: Here it can be seen that proportionally most of the people who leave the company are not or not really enjoying themselves (level 1 and 2). The people who are enjoying themselves more may also leave the company occasionally. However, proportionally, this happens much less. 
- Years worked at company: Here it can be seen that most people do not work at the same company for more than 10 years. It can also be seen here that the people who do work at the same company for more than 10 years also do not leave the company.
- Department: Most people who leave the company work in the Sales and Research & Development departments. However, it is important to note here that the HR department is comparatively smaller than the other departments.
- Marital status: Of those who are single, they are most likely to leave the company. Those who are divorced are less likely to leave a company.
- Age: On average, most people leaving the company are around 30 years old and the most people that work for the company are within 30 to 45 years old.

## Data Preparation

### Change categorial variables

To better define the data I have assigned numbers to the categorial (in)dependent variables in the code below. I am doing this because R can use letters as input, but this can make interpreting the results more difficult. 

```{r}
Attrition$Attrition = ifelse(Attrition$Attrition == "Yes", 1, 0)
Attrition$MaritalStatus= factor(Attrition$MaritalStatus,
                              levels= c('Single','Married','Divorced'),
                              labels = c(1,2,3))
Attrition$EducationField = factor(Attrition$EducationField,
                                levels= c('Human Resources','Life Sciences','Marketing','Medical','Other','Technical Degree'),
                                          labels= c(1,2,3,4,5,6))
Attrition$Department <- factor(Attrition$Department,
                           levels = c("Human Resources",
                                      "Research & Development",
                                      "Sales"),
                           labels = c(0,1,2))
```

AS seen in the code above, the variables are still factors. Because of that, I am going to convert all columns to numeric in the code below.

```{r}
Attrition$Attrition<- as.numeric(Attrition$Attrition)
Attrition$Age<- as.numeric(Attrition$Age)
Attrition$Department <- as.numeric(Attrition$Department)
Attrition$DistanceFromHome<- as.numeric(Attrition$DistanceFromHome)
Attrition$Education <- as.numeric(Attrition$Education)
Attrition$EducationField <- as.numeric(Attrition$EducationField)
Attrition$EnvironmentSatisfaction <- as.numeric(Attrition$EnvironmentSatisfaction)
Attrition$JobSatisfaction <- as.numeric(Attrition$JobSatisfaction)
Attrition$MaritalStatus <- as.numeric(Attrition$MaritalStatus)
Attrition$MonthlyIncome <- as.numeric(Attrition$MonthlyIncome)
Attrition$NumCompaniesWorked<- as.numeric(Attrition$NumCompaniesWorked)
Attrition$WorkLifeBalance <- as.numeric(Attrition$WorkLifeBalance)
Attrition$YearsAtCompany <- as.numeric(Attrition$YearsAtCompany)
```

### Check for outliers

First of all I am going to create a model to use cook's distance.I am using the GLM function because Attrition is an binary variabile (yes or no).That's also why the 'binomial' family statement is used, this is because the outcome variable is binary.   

```{r}
model <- glm(Attrition ~.,
             data=Attrition, 
             family='binomial')
```

In the code below, I am creating a visualization to evaluate the performance of a logistic regression model that has been built using the Attrition dataset.
First, I calculate the standardized residuals by taking the residuals of the model using the "deviance" method and dividing them by the square root of the ratio of deviance to the residual degrees of freedom.
Then, I create a new dataframe by combining the original Attrition dataset with the standardized residuals.
Finally, I use the ggplot2 package to create a scatter plot of the fitted values of the model against the standardized residuals. I also add a horizontal line at y = 0 to represent a perfect fit and label the x and y axis accordingly.

```{r}
standardized_residuals <- residuals(model, type = "deviance") / sqrt(model$deviance/model$df.residual)

Attrition_residuals <- data.frame(Attrition, standardized_residuals)

ggplot(Attrition_residuals, aes(x = fitted(model), y = standardized_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Standardized residuals")
```

The code below is identifying observations with standardized residuals greater than 3 and extracting them from the "Attrition_residuals" dataframe, creating a new dataframe with only those observations.

```{r}
greater_than_3 <- which(standardized_residuals > 3)
Attrition_residuals[greater_than_3,]
```

The below code filters the "Attrition_residuals" dataframe by removing observations with standardized residuals greater than 3, which indicates poor model fit for those observations. This cleaning step aims to improve the model's performance by removing outliers.

```{r}
Attrition <- filter(Attrition_residuals, standardized_residuals <= 3)
```

Now I am going to delete the "standardized_residuals" from the dataset as it is not an predictor variable.

```{r}
Attrition <- Attrition %>% select(-standardized_residuals)
```

#### Check data values

I want to know if there are missing values. 

```{r}
is.null(Attrition)
```

There are no NA values.

Next, I want to know if there are duplicate values within the dataset.

```{r}
duplicate_rows <- duplicated(Attrition)
number_of_duplicates <- sum(duplicate_rows)
number_of_duplicates
```

As you can see in the table above there a 0 duplicate rows.

### Check correlations

Now I am going to investigate whether there is mulitcollinearity between the independent variables. There is talk of multicollinearity between two independent variables if the correlation is between them is higher than 0.7. If there is, this could make the results less reliable.

In the code below I use the abs() method to get the absolute value that is positive value doesn’t change and negative value converted into positive value. I do this to ensure that negative multicollinearity is not overlooked.

```{r}
f <- select(Attrition, -Attrition)
corr <- abs(cor(f))
multicol<- (corr >= 0.7) & (corr < 1)
multicol
```

As shown in the table above, there is no multicollinearity between the independent variables.

## Modeling

### Random number generation

The below code is setting a seed for a random number generator. This means that it is setting a starting point for the random numbers generated by the computer, so that the results are reproducible.

```{r}
set.seed(123)
```

### Split data into training and testing set

In the code below I am creating a random split the dataset into two new datasets. The "training" dataset will contain 80% of the rows from the original "Attrition" dataset, while the "testing" dataset will contain the remaining 20% of the rows. This split is done using the createDataPartition() function, which randomly selects rows to be included in the "training" dataset. The split is done only once (times = 1) and the results are not put in a list (list = FALSE).

```{r}
splitIndex <- createDataPartition(Attrition$Attrition, p = .8, list = FALSE, times = 1)
training <- Attrition[ splitIndex,]
testing <- Attrition[-splitIndex,]
```


### Generalized Linear Model(s)

##### Model1: all variables

First of all I am going to create a model with all variables and see how that does. As explained before, I am using the GLM function because the Attrition variable is binary.

```{r}
model1 <- glm(Attrition ~ .,
               data=training,
               family='binomial')
```

```{r}
summary(model1)
```

In the summary above I can see which variables are insignificant. This are the variables with a p-value greater than 0.05. 
Based on the p-values from the summary of the linear regression model, the following variables are insignificant:
- Education
- EducationField
- Department

#### Model2: without the insignificant variables

Now I am going to create a model without the insignificant variables in it.

```{r}
Attrition_significant <- training %>% select(-EducationField, -Education, -YearsAtCompany)
model2 <- glm(Attrition ~ ., data = Attrition_significant, family = "binomial")
summary(model2)
```


The independent variables are all significant, as indicated by the small p-values (all less than 0.05).

## Evaluation and Deployment

In the first row of code below I am creating a new column in the data frame called predicted and assigning the values of the predictions made by the model. If the predicted value is greater than 0.5, it is assigned the value of 1, otherwise it is assigned the value of 0.

Below the code I will explain what the table means and how accurate this model is.

```{r}
Attrition_significant$predicted <- ifelse(predict(model2, type = "response") > 0.5, 1, 0)

confusion_matrix <- table(Attrition_significant$Attrition, Attrition_significant$predicted)
confusion_matrix

```

- The number 987 in the top left corner represents the number of observations that were actually negative (0) and were correctly predicted as negative by the model.
- The number 6 in the top right corner represents the number of observations that were actually negative (0) but were incorrectly predicted as positive (1) by the model.
- The number 156 in the bottom left corner represents the number of observations that were actually positive (1) but were incorrectly predicted as negative (0) by the model.
- The number 23 in the bottom right corner represents the number of observations that were actually positive (1) and were correctly predicted as positive by the model.

The accuracy model is used following the following formula: (number of true positives + number of true negatives) / (number of true positives + number of false positives + number of true negatives + number of false negatives) (https://www.jeremyjordan.me/evaluating-a-machine-learning-model/#:.).

The accuracy of this model is = (987 + 23) / (987 + 6 + 156 + 23) = (1010) / (1172) = 0.86 or 86%.

The accuracy of this model is quite good, as it means that the model is correctly predicting the outcome for 86 out of 100 observations.
