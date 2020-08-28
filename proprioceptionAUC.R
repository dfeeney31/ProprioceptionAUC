library(tidyverse)
library(caret)
library(pROC)

# Load data ---------------------------------------------------------------
rm(list=ls())
#Brett
dat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/LiteratureReview/Proprioception/BrettTri.csv')
laceDat <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/LiteratureReview/Proprioception/BrettLace.csv')

# multiclass example

# Use confusion to create matrix ------------------------------------------


confusDat <- confusionMatrix(
  as.factor(dat$ACTUAL),
  as.factor(dat$GUESS)
)

confusDat2 <- confusionMatrix(
  as.factor(laceDat$ACTUAL),
  as.factor(laceDat$GUESS)
)

# Calculate cumulative probabilities --------------------------------------
confusDat
confusDat2

#Calcualte cumulative sums
cumProb <- apply(confusDat$table, 2, cumsum)
cumProbLace <- apply(confusDat2$table, 2, cumsum)

#Calculate cumulative probabilties
cumProb <- as.data.frame(apply(cumProb, 2, function(x) x/max(x)))
colnames(cumProb) <- c("one","two","three","four","five")


cumProbLace <- as.data.frame(apply(cumProbLace, 2, function(x) x/max(x)))
colnames(cumProbLace) <- c("one","two","three","four","five")


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
fourFiveTri <- createDF(cumProb,'five','four','tri','fourFive')
fourFiveLace <- createDF(cumProbLace, 'five','four','lace','fourFive')
# Create three four comparisons
threeFourTri <- createDF(cumProb,'four','three','tri','threeFour')
threeFourLace <- createDF(cumProbLace, 'four','three','lace','threeFour')
# Create two three comparisons
twoThreeTri <- createDF(cumProb,'three','two','tri','twoThree')
twoThreeLace <- createDF(cumProbLace, 'three','two','lace','twoThree')
# Create three four comparisons
oneTWoTri <- createDF(cumProb,'two','one','tri','oneTwo')
oneTwoLace <- createDF(cumProbLace, 'two','one','lace','oneTwo')

ROCdat <- rbind(fourFiveTri, fourFiveLace, threeFourTri, threeFourLace, twoThreeTri, twoThreeLace, oneTWoTri, oneTwoLace)


ggplot(data = ROCdat, mapping = aes(x = FPR, y = TPR, color = Cond)) + geom_line() + geom_abline(slope = 1, intercept = 0) +
  xlab('False Positive Rate (1-specificity)') + ylab('True Positive Rate (sensitivity)') + facet_wrap(~ Comparison)

try2 <- rbind(fourFiveTri, fourFiveLace)


# Section for Dan data with 6 categories ----------------------------------
## Dan
rm(list=ls())
dat <- read.csv('C:/Users/Daniel.Feeney/Desktop/DanTri.csv')
laceDat <- read.csv('C:/Users/Daniel.Feeney/Desktop/DanLace.csv')
dat <- na.omit(dat)
laceDat <- na.omit(laceDat)

## confusion matrices
confusDat <- confusionMatrix(
  as.factor(dat$ACTUAL),
  as.factor(dat$GUESS)
)

confusDat2 <- confusionMatrix(
  as.factor(laceDat$ACTUAL),
  as.factor(laceDat$GUESS)
)


#Calcualte cumulative sums
cumProb <- apply(confusDat$table, 2, cumsum)
cumProbLace <- apply(confusDat2$table, 2, cumsum)


#Calculate cumulative probabilties
cumProb <- as.data.frame(apply(cumProb, 2, function(x) x/max(x)))
colnames(cumProb) <- c("one","two","three","four","five", "six")


cumProbLace <- as.data.frame(apply(cumProbLace, 2, function(x) x/max(x)))
colnames(cumProbLace) <- c("one","two","three","four","five", "six")

createDF <- function(inputDat, col1, col2, Condition, Comparison) {
  tmpDF <- inputDat %>%
    select(col1,col2) %>%
    rename("FPR" = col1, "TPR" = col2) %>%
    cbind(Cond = rep(Condition, length(inputDat))) %>%
    cbind(Comparison = rep(Comparison, length(inputDat)))
  return(tmpDF)
}

# Create five six comparisons
fiveSixTri <- createDF(cumProb,'six','five','tri','fiveSix')
fiveSixLace <- createDF(cumProbLace, 'six','five','lace','fiveSix')
# Create four five comparisons
fourFiveTri <- createDF(cumProb,'five','four','tri','fourFive')
fourFiveLace <- createDF(cumProbLace, 'five','four','lace','fourFive')
# Create three four comparisons
threeFourTri <- createDF(cumProb,'four','three','tri','threeFour')
threeFourLace <- createDF(cumProbLace, 'four','three','lace','threeFour')
# Create two three comparisons
twoThreeTri <- createDF(cumProb,'three','two','tri','twoThree')
twoThreeLace <- createDF(cumProbLace, 'three','two','lace','twoThree')
# Create three four comparisons
oneTWoTri <- createDF(cumProb,'two','one','tri','oneTwo')
oneTwoLace <- createDF(cumProbLace, 'two','one','lace','oneTwo')

ROCdat <- rbind(fiveSixTri, fiveSixLace, fourFiveTri, fourFiveLace, threeFourTri, threeFourLace, twoThreeTri, twoThreeLace, oneTWoTri, oneTwoLace)


ggplot(data = ROCdat, mapping = aes(x = FPR, y = TPR, color = Cond)) + geom_line() + geom_abline(slope = 1, intercept = 0) +
  xlab('False Positive Rate (1-specificity)') + ylab('True Positive Rate (sensitivity)') + facet_wrap(~ Comparison)



