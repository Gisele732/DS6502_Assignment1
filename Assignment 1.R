#set the working directory to the correct folder
setwd("C:/Users/Owner/OneDrive - Whitireia and WelTec/DS6502 Data Visualisation")
#creating SalaryData dataset from the csv file
SalaryData <- read.csv("Salary Data 2.csv")

View(SalaryData)
head(SalaryData)
summary(SalaryData)

#function to calculate the mode of different columns
ModeSalary <- names(sort(-table(SalaryData$Salary)))[1]
ModeSalary
ModeAge <- names(sort(-table(SalaryData$Age)))[1]
ModeAge

#Creating some graphs
library(ggplot2)
library(tidyverse)

#Visualising gender, Education and job distribution using pie charts
# Pie plots with ggplot2
#Gender Distribution in the Business
GenderDistributionPie <- ggplot(data=SalaryData, aes(x="", fill=Gender)) +
  geom_bar(position="fill") +
  ggtitle("Gender Distribution In the Workplace")+
  geom_text(
    stat='count', 
    aes(y=after_stat(..count..),
        label=after_stat(scales::percent(..count../sum(..count..),1))),
        position=position_fill(0.5)) +
  coord_polar(theta="y") +
  labs(x=NULL, y=NULL) +
  scale_fill_manual(values = c("#FFB6C1","cadetblue1"))+
  theme_void()
GenderDistributionPie

#Let's compare education level by gender using a bar graph
GenderEducationPlot <- ggplot(SalaryData, aes(fill=Gender, x=Education.Level)) + 
  ggtitle("Education Level by Gender")+
  geom_bar(position="dodge", stat="count") +
  scale_fill_manual(values = c("#FFB6C1","cadetblue1"))+
  labs(x= "Education Level") +
  theme_minimal()
GenderEducationPlot

# Calculate mean salary
MeanSalary <- mean(SalaryData$Salary, na.rm = TRUE)

# Create a bar chart showing mean salaries by gender with a line representing overal mean salary
ggplot(SalaryData, aes(x = Gender, y = Salary, fill = Gender, color = Gender)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.5) +
  scale_fill_manual(values = c("#FFB6C1","cadetblue1")) +
  geom_hline(yintercept = MeanSalary, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Mean Salary by Gender",
       x = "Gender",
       y = "Mean Salary",
       caption = paste("Mean Salary Overall: $", round(MeanSalary, 2))) +
  theme_minimal()

#Box plot of salary by education level and gender
SalaryByGenderBoxPlot <- ggplot(data = SalaryData, aes( x = Education.Level, y = Salary, fill = Gender)) + 
  geom_boxplot() +
  labs(title = "Salaries by gender and education level", x= "Education level")+
  scale_fill_manual(values = c("#FFB6C1","cadetblue1"))+
  theme_minimal()
SalaryByGenderBoxPlot

#Years of Experience vs Salary by Gender 
#Remove outliers (no effect in this case)
Q1 <- quantile(SalaryData$Salary, .25)
Q3 <- quantile(SalaryData$Salary, .75)
IQR <- IQR(SalaryData$Salary)

SalaryData_no_outliers <- subset(SalaryData, Salary> (Q1 - 1.5*IQR) & Salary< (Q3 + 1.5*IQR))

YearsExperienceSalaryScatterPlot <- ggplot(SalaryData_no_outliers, aes(x = Years.of.Experience, y= Salary, color = Gender)) +
  geom_point() +
  labs(title = "Years of Experience vs Salary", subtitle = "by gender", x= "Years of Experience",y = "Salary", color = "Gender") +
  theme_minimal()+
  geom_smooth(method = "lm", se= FALSE)
YearsExperienceSalaryScatterPlot

#We create two subsets, one for each gender
FemaleEmployees <- subset(SalaryData, Gender == "Female")
MaleEmployees <- subset(SalaryData, Gender == "Male")

#are experience and salaty correlated? Is the correlation the same for each gender?
summary(lm(Salary ~ Years.of.Experience, data = FemaleEmployees))
summary(lm(Salary ~ Years.of.Experience, data = MaleEmployees))

#trying to understand if there is a stastistically significant difference between men and women salaries
t.test(Salary ~ Gender, SalaryData, paired=FALSE)
