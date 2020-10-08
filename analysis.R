rm(list=ls())

library('tidyverse')
library('readxl')
library('lme4')

dat <- read_excel('C:/Users/Daniel.Feeney/Dropbox (Boa)/Endurance Health Validation/Proprioception/Proprioception_Results_MaherAdams.xlsx')
dat$Subject <- as.factor(dat$Subject)
dat$Comparison <- as.factor(dat$Comparison)

dat %>%
  group_by(Subject, Condition) %>%
  summarize(meanAUC = mean(AUC))

ggplot(data = dat) +
  geom_boxplot(mapping = aes(x = Subject, y = AUC, fill = Condition))

mixedMod <- lmer(AUC ~ Condition + (1 | Subject), data = dat)
summary(mixedMod)

fullMod <- lmer(AUC ~ Condition * Comparison + (1 | Subject), data = dat)
summary(fullMod)
