# Core Tidyverse
library(tidyverse)
library(lubridate)
library(glue)
library(forcats)
library(magrittr)
library(scales)

# Visualization
library(ggplot2)
library(cowplot)
library(scales)
library(vcd)
library(grid)
library(ggraph)



df<- read.csv("usa.csv")
df1<- df[,c("student_id","age","bin","full_part","classification","appl_response","gpa","educ","exp","Sex","distance")]
df2 <- df[,c("age","bin","gpa","educ","exp","Sex","distance")]

head(df1)
summary(df2)
summary(df1$exp)
######## cleaning df2 variables ######
sum(is.na(df1$gpa))
df2$gpa[is.na(df2$gpa)] = mean(df2$gpa, na.rm=TRUE)
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.01,0.95))))
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.01,0.95))))
df2$exp <- squish(df2$exp,round(quantile(df2$exp,c(0.01,0.9))))
######## cleaning df1 variables ######
sum(is.na(df1$gpa))
df1$gpa[is.na(df1$gpa)] = mean(df1$gpa, na.rm=TRUE)
df1$gpa <- squish(df1$gpa,round(quantile(df1$gpa,c(0.01,0.95))))
df1$gpa <- squish(df1$gpa,round(quantile(df1$gpa,c(0.01,0.95))))
df1$exp <- squish(df1$exp,round(quantile(df1$exp,c(0.01,0.9))))
## plot all variables together
library(purrr)
library(tidyr)
library(ggplot2)
df2 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value),col="blue") +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
# Check distribution of one field
df1["country"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df1$coutry)) +
  facet_wrap(~ key, scales = "exp")
## replace na with mean value
df2$gpa[is.na(df2$gpa)] = mean(df2$gpa, na.rm=TRUE)
##outlier of gpa variable
##plot the above plot again - saw two outliers, moving them to mean
??squish
summary(df1$exp_n)
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.01,0.95))))
summary(df1$gpa)
df2$distance <- squish(df2$distance,round(quantile(df2$distance,c(0,0.9))))
summary(df1$distance)
# clean exp variable
summary(df1$exp)
df2$exp <- squish(df2$exp,round(quantile(df2$exp,c(0.01,0.9))))
# normalise exp variable
mean_exp <- mean(df1$exp) 
sd_exp <- sd(df1$exp)
df1$exp_n<- (df1$exp-mean_exp)/sd_exp
####clean level of education variable
summary(df1$exp_n)
# Check distribution of one field
df1["exp_n"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df1$exp_n)) +
  facet_wrap(~ key, scales = "free")
## number of empty cells
sum(is.na(df1))
## change all categorical variables to factors
df1<-transform(df1, full_part.f = as.factor(full_part))
df1<-transform(df1, classification.f = as.factor(classification))
df1<-transform(df1, appl_response.f = as.factor(appl_response))
df1<-transform(df1, educ.f = as.factor(educ))
df1<-transform(df1, sex.f = as.factor(Sex))
df2<-transform(df2, bin.f = as.factor(bin))
df1<-transform(df1, bin.f = as.factor(bin))
## check for str
str(df1)

## normalise all numerical variables
mean_dist <- mean(df1$distance) 
sd_dist <- sd(df1$distance)
df1$distance_n<- (df1$distance-mean_dist)/sd_dist

## 
summary(df1$distance_n)
## lpm run
lpm=lm( bin~ age+gpa+educ.f+sex.f+exp_n+distance_n, data=df1)
summary(lpm)

df1<-transform(df1, bin_fitted = fitted.values(lpm))
df1<-transform(df1, bin_tilde = as.numeric(bin_fitted>=0.5))
df1<-transform(df1, num_matched = as.numeric(bin==bin_tilde))
sum(df1$num_matched)
# 464 - 464 out of 601 are the same.
sum(df1$num_matched)/nrow(df1)
##
logLik(logit)

with(logit, pchisq(null.deviance, df.null, lower.tail = FALSE)) 
with(logit, pchisq(deviance, df.residual, lower.tail = FALSE)) 

with(logit, null.deviance - deviance)
with(logit, df.null - df.residual)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


## logit run
logit <- glm(bin~ age+gpa+educ.f+sex.f+exp_n+distance_n, family = binomial(link = "logit"), data = df1)
summary(logit)
### probit run
probit <- glm(bin~ age+gpa+educ.f+sex.f+exp_n+distance_n, family = binomial(link = "probit"), data = df1)
summary(probit)
## all together
library(stargazer)
stargazer(logit,probit,lpm, type="text",out="Logit_lpm_usa.txt", title="Table 1.USA")

###
logit2prob <- function(logit1){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(logit))
#####
logit_gpa <- glm(bin~ gpa, family = binomial(link = "logit"), data = df1)
summary(logit_gpa)
range(df1$gpa)
xweight <- seq(0, 4, 0.1)
yweight <- predict(logit_gpa, list(gpa = xweight),type="response")
plot(df1$gpa, df1$bin, pch = 16, xlab = "GPA", ylab = "Admit")
lines(xweight, yweight)
#####
logit_age <- glm(bin~ age, family = binomial(link = "logit"), data = df1)
summary(logit_age)
range(df1$age)
xweight <- seq(20, 57, 0.01)
yweight <- predict(logit_age, list(age = xweight),type="response")
plot(df1$age, df1$bin, pch = 16, xlab = "age", ylab = "Admit")
lines(xweight, yweight)
###
logit_gre <- glm(bin~ gre_total, family = binomial(link = "logit"), data = df1)
summary(logit_gre)
range(df1$gre_total)
xweight <- seq(200, 324, 0.1)
yweight <- predict(logit_age, list(age = xweight),type="response")
plot(df1$gre_total, df1$bin, pch = 16, xlab = "gre", ylab = "Admit")
lines(xweight, yweight)
#### Goodness-of-fit for LPM - percent correctly predicted
data<-transform(df1, classification_fitted = fitted.values(lpm))
data<-transform(df1, clas_tilde = as.numeric(classification_fitted>=0.5))
data<-transform(df1, num_matched = as.numeric(classification==classification_tilde))
sum(df1$num_matched)

# Goodness-of-fit for Logit Model Estimation
logLik(logit)

with(logit, pchisq(null.deviance, df.null, lower.tail = FALSE)) 
with(logit, pchisq(deviance, df.residual, lower.tail = FALSE)) 

with(logit, null.deviance - deviance)
with(logit, df.null - df.residual)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



## logit with only one variable
df2<- df[,c("classification","gpa")]
df2$gpa[is.na(df2$gpa)] = mean(df$gpa, na.rm=TRUE)
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.05,0.95))))
logit1 <- glm(classification~ gpa, family = binomial(link = "logit"), data = df2)
summary(logit1)

## Plotting all regressions:-
## logit curve with gpa variables:-
quartz(title="admission vs. gpa")
plot(df1$distance_n,df1$classification,xlab="",ylab="Probability") 
curve(predict(logit1,data.frame(gpa=x),type="resp",col="blue"),add= TRUE)


# 2-way contingency tables
xtabs(~educ + gpa, data = df1)


df1<- cbind(df$gpa,df$education)
cotab_mosaic(x=df1,condvars = NULL)
??mosaic
??cotab_mosaic


## decision variable classification
summary(df$classification)
## removing rows with classification = 2


## Sample regression:
ols<-lm(df$classification~df$age + df$gpa + df$education)
summary(ols)
logit1 <- glm(df$classification~df$age + df$gpa + df$education,family ="binomial")


## tree design for us citizens:-
library(rpart)
tree_model<- rpart(formula= bin.f~age+gpa+distance+exp,data =df2,method="class")
summary(tree_model)
plot(tree_model)
printcp(tree_model)
plotcp(tree_model)
plot(tree_model, uniform=TRUE, 
     main="Classification Tree for Lindner")
text(tree_model, use.n=TRUE, all=TRUE, cex=.8)

## random forest
install.packages("randomForest")
install.packages("reprtree")
library(randomForest)
rft<-randomForest(bin~age+exp+distance+gpa,data=df2)
print(rfit)


## conditional inference tree
install.packages("party")
library(party)
cft <- ctree(bin.f ~ age + gpa + distance +exp, 
             data=df2)
plot(cft, main="Conditional Inference Tree for Lindner_USA")

cf1<-  ctree(bin ~ age + gpa + distance +exp, 
             data=df2, controls = ctree_control(maxsurrogate = 3))
plot(cf1, main="Conditional Inference Tree for Lindner_USA")

# Regression Tree Example
library(rpart)

reg <- rpart(bin ~ age + gpa + distance +exp, 
             method="anova", data=df2)

printcp(reg) # display the results 
plotcp(reg) # visualize cross-validation results 
summary(reg) # detailed summary of splits
par(mfrow=c(1,1)) # two plots on one page 
rsq.rpart(reg) # visualize cross-validation results  	

# plot tree 
plot(reg, uniform=TRUE, 
     main="Regression Tree")
text(reg, use.n=TRUE, all=TRUE, cex=.9)

