# read in data
survey = read.csv("survey_question_averages.csv")
survey
str(survey)
summary(survey)

# correlation matrix
cor(survey[,2:7])

# messing around with other correlation matrix visualizations...
C = cor(survey[,2:7])
#install.packages("corrplot")
library(corrplot)
corrplot(C, method = "number")
corrplot(C, method = "circle")
corrplot(C, method = "ellipse")

library(PerformanceAnalytics)
chart.Correlation(survey[,2:7], histogram = TRUE, method = "pearson")
