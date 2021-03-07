# Partition the data into training (60%) and validation (40%) sets.A.Create a pivot table for 
# the training data with Online as a column variable, CC as a row variable, and Loan as a 
# secondary row variable. The values inside the table should convey the count. In R use 
# functions melt()and cast(), or function table(). In Python, use panda dataframe methods 
# melt()and pivot().B.Consider the task of classifying a customer who owns a bank credit card 
# and is actively using online banking services. Looking at the pivot table, what is the 
# probability that this customer will accept the loan offer? [This is the probability of 
# loan acceptance (Loan = 1) conditional on having a bank credit card (CC = 1) and being an 
# active user of online banking services (Online = 1)].C.Create two separate pivot tables for 
# the training data. Onewill have Loan (rows) as a function of Online (columns) and the other 
# will have Loan (rows) as a function of CC.D.Compute the following quantities [P(A | B) means 
# "the probability ofA given B"]: i.P(CC = 1 | Loan = 1) (the proportion of credit card holders 
# among the loan acceptors)ii.P(Online = 1 | Loan = 1) iii.P(Loan = 1) (the proportion of 
# loan acceptors) iv.P(CC = 1 | Loan = 0) v.P(Online = 1 | Loan = 0)vi.P(Loan = 0)E.Use the 
# quantities computed above to compute the naive Bayes probability P(Loan = 1 | CC = 1, 
# Online = 1).F.Compare this value with the one obtained from the pivot table in (B). 
# Which is a more accurate estimate?G.Which of the entries in this table are needed for 
# computing P(Loan = 1 | CC = 1, Online = 1)? Run naive Bayes on the data. Examine the model 
# output on training data, and find the entry that corresponds to P(Loan = 1 | CC = 1, 
# Online = 1). Compare this to the number you obtained in (E).

## load everything from before
library(readr)
library(caret)
library(reshape2)
library(ISLR)
library(e1071)
library(tidyverse)
library(pivottabler)
library(class)
library(gmodels)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(fastDummies)
library(rmarkdown)
## load the file
UniversalBank <- read_csv("GitHub/64060_mgeist/Assignment 3/UniversalBank.csv")
UniversalBank$PersonalLoan = as.factor(UniversalBank$PersonalLoan)
UniversalBank$Online = as.factor(UniversalBank$Online)
UniversalBank$CreditCard = as.factor(UniversalBank$CreditCard)
View(UniversalBank)
set.seed(123)


## partitiion data 60/40
Index_Train <- sample(row.names(UniversalBank), 0.6*dim(UniversalBank)[1])  
Index_Test <- setdiff(row.names(UniversalBank), Index_Train) 

##Part A
print("Part A")
partA <- UB_Train
table("CC"=partA$CreditCard,"PL"=partA$PersonalLoan,"O/L"=partA$Online)
table1 = recast(partA,partA$CreditCard+partA$PersonalLoan~partA$Online)
table1

##Part B
print("Part B")
print("The probability of the customer accepting the loan is 82/882 or 9.2%")

##Part C
print("Part C")
table2 = recast(partA,partA$PersonalLoan~partA$Online)
table2
table3 = recast(partA,partA$CreditCard~partA$Online)
table3

##Part D
print("Part D")
print("CC=credit card, PL=personal loan, OL= online account")
print("Part 1 is P(CC|PL) = 77/(77+198) is 28.00%")
print("Part 2 is P(OL|PL) = 166/(166+109) is 60.30%")
print("Part 3 is P(PL) = 275/(275+2725) is 10.09%")
print("Part 4 is P(CC|PL') = 801/(801+1924) is 29.39%")
print("Part 5 is P(OL|PL') = 1588/(1588+1137) is 58.27%")
print("Part 6 is P(PL') = 2725/(275+2725) is 90.83%")

##Part E
print("Part E")
print("P(CC|PL)*P(OL|PL)*P(PL)")
print("_______________________")
print("P(CC|PL)*P(OL|PL)P(PL)")
print("+")
print("P(CC|PL')*P(OL|PL')*P(PL')")
((77/(77+198))*(166/(166+109))*(275/(275+2725)))/(((77/(77+198))*(166/(166+109))*(275/(275+2725)))+((801/(801+1924))*(1588/(1588+1137))*2725/(2725+275)))

##Part F
print("Part F")
print("Part B = 9.29% and Part E = 9.05%")
print("They are very similar.  I believe that we the most accurate one is part be because it was an actual calculation, in this instance Naive Bayes is not needed.")

##Part H
print("Part H")

train.df <- UniversalBank[Index_Train, ]
test.df <- UniversalBank[Index_Test, ]
train <- UniversalBank[Index_Train, ]
test = UniversalBank[Index_Test,]

nb_train = train.df[,c(10,13:14)]
nb_test = test.df[,c(10,13:14)]
naivebayes = naiveBayes(PersonalLoan~.,data=nb_train)
naivebayes