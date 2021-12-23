library(texreg)
act <- read.csv(file = 'act_merged.csv')

# Data EDA:
## Individual Regression Plots
plot(act$married,act$Composite.score)
abline(lm(act$Composite.score~act$married))
plot(act$bachelors_higher,act$Composite.score)
abline(lm(act$Composite.score~act$bachelors_higher))
plot(act$diff_house,act$Composite.score)
abline(lm(act$Composite.score~act$diff_house))
plot(act$other_language,act$Composite.score)
abline(lm(act$Composite.score~act$other_language))
plot(act$median_wage,act$Composite.score)
abline(lm(act$Composite.score~act$median_wage))

model <- lm(lm(Composite.score~married, data=act))
texreg(model)

model <- lm(lm(Composite.score~bachelors_higher, data=act))
texreg(model)

model <- lm(lm(Composite.score~diff_house, data=act))
texreg(model)

model <- lm(lm(Composite.score~other_language, data=act))
texreg(model)

act.wage_scaled = act$median_wage/10000
model <- lm(lm(Composite.score~act.wage_scaled, data=act))
texreg(model)

## Correlation Matrix
act.cor <- cor(act[2:21])
library(corrplot)
corrplot(act.cor)
## QQ Plot of ACT Scores
qqnorm(act$Composite.score)
qqline(act$Composite.score)
## Kolmogorov Smirnov Test
act.standardized_scores <- (act$Composite.score-mean(act$Composite.score))/sd(act$Composite.score)
ks.test(act.standardized_scores, 'pnorm')
# Modeling
model <- lm(Composite.score~bachelors_higher+diff_house+married+other_language+act.wage_scaled, data=act)
summary(model)
texreg(model)

#Influential data via DFBETAS
thresh <- 2/sqrt(51)
dfbetas <- as.data.frame(dfbetas(model))

plot(dfbetas$bachelors_higher, type='h')
abline(h = thresh, lty = 1)
abline(h = -thresh, lty = 1)

plot(dfbetas$diff_house, type='h')
abline(h = thresh, lty = 1)
abline(h = -thresh, lty = 1)

plot(dfbetas$married, type='h')
abline(h = thresh, lty = 1)
abline(h = -thresh, lty = 1)

plot(dfbetas$other_language, type='h')
abline(h = thresh, lty = 1)
abline(h = -thresh, lty = 1)

plot(dfbetas$act.wage_scaled, type='h')
abline(h = thresh, lty = 1)
abline(h = -thresh, lty = 1)



