# Load the necessary packages to perform analysis
library(dplyr)
library(tidyr)
library(GPArotation)
library(psych)
library(corrplot)
library(car)
library(lavaan)
library(semPlot)
library(ltm)
library(eRm)

# Set working directory to location containing simulated data
#setwd("C:/Users/bhart/Dropbox/MSSP/Consulting")

# Load the data as a data frame
# To load this on your machine, simply change the file path to where the csv file with the data is located.

survey <- read.csv("~/GradSchool/Practicum I 675/Consulting Projects/Education Survey/simulated.csv")


#####Data Manipulation and Cleaning#############

# Convert to tbl_df so that dplyr and tidyr functions can be used to manipulate the data
survey = tbl_df(survey)

# Separate the Q2A and Q4A columns into their own columns
survey = survey %>% separate(Q2A, c("Q2ignore","Q2A1","Q2A2","Q2A3","Q2A4","Q2A5","Q2A6"), sep = c(1,2,3,4,5,6))
survey = survey %>% separate(Q4A, c("Q4ignore","Q4A1","Q4A2","Q4A3","Q4A4","Q4A5","Q4A6","Q4A7"), sep = c(1,2,3,4,5,6,7))

#Drops the two columns that contained the dummy x values
survey = survey %>% dplyr::select(-c(Q2ignore, Q4ignore))


# Create Binary Version of Q4 where 1,2,3 = 0 and 4,5 = 1. 
# Could easily be altered to set different thresholds, or unique thresholds per question
survey$Q4A1_bin = recode(survey$Q4A1, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A2_bin = recode(survey$Q4A2, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A3_bin = recode(survey$Q4A3, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A4_bin = recode(survey$Q4A4, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A5_bin = recode(survey$Q4A5, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A6_bin = recode(survey$Q4A6, '1=0; 2=0; 3=0; 4=1; 5=1')
survey$Q4A7_bin = recode(survey$Q4A7, '1=0; 2=0; 3=0; 4=1; 5=1')


# Convert question values from characters to numeric
for(i in 6:ncol(survey)) {
  survey[,i] <- as.numeric(unlist(survey[,i]))
}

#### Exploratory Analysis: Correlations Between Responses ####

# Check correlations between items; note items that have negative correlations...Before reverse coding
all_items = cor(survey[,c(6:11,19:25)])
corrplot(all_items, method = "ellipse")

Q2A = cor(survey[,6:11])
corrplot(Q2A, method = "ellipse")

Q4A = cor(survey[,12:18])
corrplot(Q4A, method = "ellipse")

Q4A_bin = cor(survey[,19:25])
corrplot(Q4A_bin, method = "ellipse")

#### Reverse Codes ####
# Reverse Coding of items Q2A6 and Q4A1, as they had been negatively correlated with the rest of the questions
survey$Q2A6 = recode(survey$Q2A6, '0=1; 1=0')
survey$Q4A1 = recode(survey$Q4A1, '1=5; 2=4; 3=3; 4=2; 5=1')
survey$Q4A1_bin = recode(survey$Q4A1_bin, '0=1; 1=0')


#### Factor Analysis on Q2 and Q4
#Factor Analysis on Q2A
pcaQ2 = princomp(~ Q2A1 + Q2A2 + Q2A3 + Q2A4 + Q2A5 + Q2A6, data = survey, cor = TRUE, score = TRUE)
summary(pcaQ2)
screeplot(pcaQ2, type = "lines")
pcaQ2$loadings


# Decided on 2 Factors for Q2A
attach(survey)
varsQ2 = data.frame(Q2A1, Q2A2, Q2A3, Q2A4, Q2A5, Q2A6)
pcaQ2_components = principal(varsQ2, nfactors = 2, rotate = "varimax") #notice negative loading for Q2A6 (before reverse-coding)

# 1, 2 and 6 load together. 3, 4 and 5 load together
pcaQ2_components
cor(pcaQ2_components$scores)

#Factor Analysis on Q4A
pcaQ4 = princomp(~ Q4A1 + Q4A2 + Q4A3 + Q4A4 + Q4A5 + Q4A6 + Q4A7, data = survey, cor = TRUE, score = TRUE)
summary(pcaQ4)
screeplot(pcaQ4, type = "lines")

# Decided on 2 Factors for 
varsQ4 = data.frame(Q4A1, Q4A2, Q4A3, Q4A4, Q4A5, Q4A6, Q4A7)
pcaQ4_components = principal(varsQ4, nfactors = 2, rotate = "varimax") 

# 1, 3, 5 and 7 load together. 2, 4 and 6 load together
pcaQ4_components


##### Item Response Theory Rasch Model: Question 2 #####

# Q2 1 Factor Model
irt_Q2 = ltm(survey[,c(6:11)] ~ z1)
summary.ltm(irt_Q2)
IRT_1F_Scores <- factor.scores(irt_Q2) #match these to each person based on their response pattern
plot(IRT_1F_Scores)

# Q2: 2 Factor Model: Factor 1
irt_Q2F1 = ltm(survey[,c(6,7,11)] ~ z1)
summary.ltm(irt_Q2F1)
IRT2_F1_Scores <- factor.scores(irt_Q2F1) #match these to each person based on their response pattern
plot(IRT2_F1_Scores)

# Q2 2 Factor Model: Factor 2
irt_Q2F2 = ltm(survey[,c(8:10)] ~ z1)
summary(irt_Q2F2)
IRT2_F2_Scores <- factor.scores(irt_Q2F2) #match these to each person based on their response pattern
plot(IRT2_F2_Scores)



#### Rating Scale Model: Question 4
# Q4, Factor 1: items 1, 3, 5, 7
rsm_F1 = RSM(survey[,c(12,14,16,18)])
summary(rsm_F1)
plotPImap(rsm_F1)
# A person-item map displays the location of item (and threshold) parameters as well as the distribution of person parameters
# along the latent dimension. Person-item maps are useful to compare the range and position of the item measure distribution 
# (lower panel) to the range and position of the person measure distribution (upper panel). Items should ideally be located 
# along the whole scale to meaningfully measure the `ability' of all persons.

plotICC(rsm_F1, item.subset = 1:4, mplot = TRUE, legpos = FALSE)
# Item Characteristic Curves Represent the Probability of a participant receiving a certain
# score on an item, based on their latent ability measured by the Rating Scale Model


plotINFO(rsm_F1, type = "item")
plot(test_info(rsm_F1))
#Test Info calculates the information of a test as the sum of the information from all items


personParameters_F1 = person.parameter(rsm_F1)
# Estimates the person parameters based on the RSM model

summary(as.numeric(unlist(personParameters_F1$thetapar)))
plot(personParameters_F1$theta.table$`Person Parameter`)
hist(as.numeric(unlist(personParameters_F1$thetapar)))
confint(personParameters_F1)

# Transform the Person Parameter Score into a vector the length of the data frame
# This allows us to bind it as a column value to the data frame
Q4_Fac1_RSM_Score <- unlist(personParameters_F1$theta.table$`Person Parameter`)

# Q4, Factor 2: items 2, 4, 6
rsm_F2 = RSM(survey[,c(13, 15,17)])
summary(rsm_F2)
plotPImap(rsm_F2)
plotICC(rsm_F2, item.subset = 1:4, mplot = TRUE, legpos = FALSE)
plotINFO(rsm_F2, type = "item")
plot(test_info(rsm_F2))
personParameters_F2 = person.parameter(rsm_F2)
summary(as.numeric(unlist(personParameters_F2$thetapar)))
plot(personParameters_F2)
hist(as.numeric(unlist(personParameters_F2$thetapar)))
confint(personParameters_F2)

Q4_Fac2_RSM_Score <- unlist(personParameters_F2$theta.table$`Person Parameter`)


######### Attaching Scores to Individuals
survey_scores <- survey
# Gets rid of the binary variables we created for Q4
survey_scores <- survey_scores[, -c(19:25)]

# Creates a new column and attaches it to the data frame
survey_scores$Q4_Fac1_Score <- Q4_Fac1_RSM_Score
survey_scores$Q4_Fac2_Score <- Q4_Fac2_RSM_Score

# Take only the scoring matrix scores and combinations of answer patterns
IRT_1F_Scores_Combos <- IRT_1F_Scores$score.dat[, c(1,2,3,4,5,6,9)]
IRT2_F1_Scores_Combos <- IRT2_F1_Scores$score.dat[, c(1, 2, 3, 6)]
IRT2_F2_Scores_Combos <- IRT2_F2_Scores$score.dat[, c(1, 2, 3, 6)]

library(plyr)

# Renames the last column in the dataframes to something more identifiable
# Gets rid of just the z1 column name across the board
colnames(IRT_1F_Scores_Combos)[7] <- "Q2_1_Fac_Score"
colnames(IRT2_F1_Scores_Combos)[4] <- "Q2_2Fac_Fac1_Score"
colnames(IRT2_F2_Scores_Combos)[4] <-  "Q2_2Fac_Fac2_Score"


# Can easily join the newly made dataframes to original dataset using join
survey_scores <- join(survey_scores, IRT_1F_Scores_Combos)
survey_scores <- join(survey_scores, IRT2_F1_Scores_Combos)
survey_scores <- join(survey_scores, IRT2_F2_Scores_Combos)

# Can easily export this to a csv
write.csv(survey_scores, "SurveyScores.csv")

# Can easily save this to an R DataSet
saveRDS(survey_scores, "SurveyScores")
