rm(list=ls())

# bf SD, DD, Lace (order)
# Packages ----------------------------------------------------------------

library(pROC)
library(tidyverse)
library(ggplot2)
library(caret)
library(readxl)

# tidying -----------------------------------------------------------------
testDat <- read_excel('C:/Users/Daniel.Feeney/Documents/Proprioception/04_Proprioception.xlsx', skip = 1)

barefoot <- testDat[,1:2]
barefoot <- rename(barefoot, ACTUAL = `Order:...1`, GUESS = `Guess:...2`)
SD <- testDat[,3:4]
SD <- rename(SD, ACTUAL = `Order:...3`, GUESS = `Guess:...4`)
DD <- testDat[,5:6]
DD <- rename(DD, ACTUAL = `Order:...5`, GUESS = `Guess:...6`)
SL <- testDat[,7:8]
SL <- rename(SL, ACTUAL = `Order:...7`, GUESS = `Guess:...8`)


# Create confusion matrices -----------------------------------------------

confusBF <- confusionMatrix(
  as.factor(barefoot$ACTUAL),
  as.factor(barefoot$GUESS)
)

confusSD <- confusionMatrix(
  as.factor(SD$ACTUAL),
  as.factor(SD$GUESS)
)

confusDD <- confusionMatrix(
  as.factor(DD$ACTUAL),
  as.factor(DD$GUESS)
)

confusSL <- confusionMatrix(
  as.factor(SL$ACTUAL),
  as.factor(SL$GUESS)
)


# calcualte cumulative sums -----------------------------------------------

#Calcualte cumulative sums
cumProbBF <- apply(confusBF$table, 2, cumsum)
cumProbLace <- apply(confusSL$table, 2, cumsum)
cumProbSD <- apply(confusSD$table, 2, cumsum)
cumProbDD <- apply(confusDD$table, 2, cumsum)

#Calculate cumulative probabilties
cumProbBF <- as.data.frame(apply(cumProbBF, 2, function(x) x/max(x)))
colnames(cumProbBF) <- c("one","two","three","four","five")

cumProbLace <- as.data.frame(apply(cumProbLace, 2, function(x) x/max(x)))
colnames(cumProbLace) <- c("one","two","three","four","five")

cumProbSD <- as.data.frame(apply(cumProbSD, 2, function(x) x/max(x)))
colnames(cumProbSD) <- c("one","two","three","four","five")

cumProbDD <- as.data.frame(apply(cumProbDD, 2, function(x) x/max(x)))
colnames(cumProbDD) <- c("one","two","three","four","five")

# Do some tidying to make into tidy DFs -----------------------------------
createDF <- function(inputDat, col1, col2, Condition, Comparison) {
  tmpDF <- inputDat %>%
    select(col1,col2) %>%
    rename("FPR" = col1, "TPR" = col2) %>%
    cbind(Cond = rep(Condition, length(inputDat))) %>%
    cbind(Comparison = rep(Comparison, length(inputDat)))
  return(tmpDF)
}

# Create four five comparisons
fourFiveBF <- createDF(cumProbBF,'five','four','BF','fourFive')
fourFiveLace <- createDF(cumProbLace, 'five','four','lace','fourFive')
fourFiveSD <- createDF(cumProbSD,'five','four','SD','fourFive')
fourFiveDD <- createDF(cumProbDD, 'five','four','DD','fourFive')
# Create three four comparisons
threeFourBF<- createDF(cumProbBF,'four','three','BF','threeFour')
threeFourLace <- createDF(cumProbLace, 'four','three','lace','threeFour')
threeFourSD<- createDF(cumProbSD,'four','three','SD','threeFour')
threeFourDD <- createDF(cumProbDD, 'four','three','DD','threeFour')
# Create two three comparisons
twoThreeBF <- createDF(cumProbBF,'three','two','BF','twoThree')
twoThreeLace <- createDF(cumProbLace, 'three','two','lace','twoThree')
twoThreeSD <- createDF(cumProbSD,'three','two','SD','twoThree')
twoThreeDD <- createDF(cumProbDD, 'three','two','DD','twoThree')
# Create three four comparisons
oneTwoBF <- createDF(cumProbBF,'two','one','BF','oneTwo')
oneTwoLace <- createDF(cumProbLace, 'two','one','lace','oneTwo')
oneTwoSD <- createDF(cumProbSD,'two','one','SD','oneTwo')
oneTwoDD <- createDF(cumProbDD, 'two','one','DD','oneTwo')

ROCdat <- rbind(fourFiveBF, fourFiveLace, fourFiveSD, fourFiveDD, threeFourBF, threeFourLace, threeFourSD, threeFourDD, 
                twoThreeBF, twoThreeLace, twoThreeSD, twoThreeDD, oneTwoBF, oneTwoLace, oneTwoSD, oneTwoDD)

ggplot(data = ROCdat, mapping = aes(x = FPR, y = TPR, color = Cond)) + geom_line() + geom_abline(slope = 1, intercept = 0) +
  xlab('False Positive Rate (1-specificity)') + ylab('True Positive Rate (sensitivity)') + facet_wrap(~ Comparison)

