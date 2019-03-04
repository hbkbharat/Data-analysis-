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


df<- read.csv("foreign_citizen.csv")
head(df)
df1<- df[,c("student_id","age","bin","full_part","Entry.Term","year_of_application","classification","response","gpa","educ","exp","sex","gre_total","country")]
df2 <- df[,c("age","bin","gpa","educ","exp","sex","gre_total","country")]

head(df1)
summary(df1)
######## cleaning df2 variables ######
sum(is.na(df1$gpa))
df2$gpa[is.na(df2$gpa)] = mean(df2$gpa, na.rm=TRUE)
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.1,0.95))))
df2$gre_total <- squish(df2$gre_total,round(quantile(df2$gre_total,c(0.01,0.95))))
df2$exp <- squish(df2$exp,round(quantile(df2$exp,c(0.01,0.9))))
df2$age <- squish(df2$age,round(quantile(df2$age,c(0.01,0.9))))
######## cleaning df1 variables ######
sum(is.na(df1$gpa))
df1$gpa[is.na(df1$gpa)] = mean(df1$gpa, na.rm=TRUE)
df1$gpa <- squish(df1$gpa,round(quantile(df1$gpa,c(0.01,0.95))))
df1$gre_total <- squish(df1$gre_total,round(quantile(df1$gre_total,c(0.01,0.95))))
df1$exp <- squish(df1$exp,round(quantile(df1$exp,c(0.01,0.9))))

######## cleaning gpa variable ######
sum(is.na(df1$gpa))
# Check distribution of one field
df1["gre_total"] %>%
  gather() %>% 
  ggplot() +
  geom_point(mapping = aes(x = value,y=df1$gre_total)) +
  facet_wrap(~ key, scales = "free")
## replace na with mean value if any ,but not needed this case
df2$gpa[is.na(df2$gpa)] = mean(df2$gpa, na.rm=TRUE)
## now remove outliers
df2$gpa <- squish(df2$gpa,round(quantile(df2$gpa,c(0.05,0.95))))
summary(df1$gpa)
#plot again to check if you want

#clean other variables, check plots and summary for all variables
sum(is.na(df1$age))
summary(df1$gre_total)
sum(is.na(df1$gre_total))
sum(is.na(df1$gre_total))
## clean gre variable
df1$gre_total <- squish(df1$gre_total,round(quantile(df1$gre_total,c(0.05,0.95))))
summary(df1$gpa)
## cleaning educ variable
summary(df1)
summary(df1$classification)

## converting to factors
df1<-transform(df1, full_part.f = as.factor(full_part))
df1<-transform(df1, classification.f = as.factor(classification))
df1<-transform(df1, response.f = as.factor(response))
df1<-transform(df1, educ.f = as.factor(educ))
df1<-transform(df1, sex.f = as.factor(sex))
df2<-transform(df2, country.f = as.factor(country))
df1<-transform(df1, year.f = as.factor(year_of_application))
## remove classification = 2 which are incomplete applications
##df1[df1$classification!=2,]
##summary(df1$classification)
## lpm run
lpm=lm( bin~ age+gpa+educ.f+sex.f+exp+gpa+gre_total+year.f, data=df1)
summary(lpm)

## logit run
logit <- glm(bin~ age+gpa+educ.f+sex.f+exp+gpa+gre_total+year.f, family = binomial(link = "logit"), data = df1)
summary(logit)
logitor(bin~ age+gpa+educ.f+sex.f+exp+gpa+distance_n, data = df1)
### probit run
probit <- glm(bin~ age+gpa+educ.f+sex.f+exp+gpa+gre_total, family = binomial(link = "probit"), data = df1)
summary(probit)

## ME
probitmfx(bin~ age+gpa+educ.f+sex.f+exp+gpa+gre_total,data=df1, atmean = FALSE)
probitmfx(bin~ age+gpa+educ.f+sex.f+exp+gpa+gre_total,data=df1, atmean = TRUE)
logitmfx(bin~ age+gpa+educ.f+sex.f+exp+gpa+distance_n,data=df1, atmean = FALSE)
logitmfx(bin~ age+gpa+educ.f+sex.f+exp+gpa+distance_n,data=df1, atmean = TRUE)
###
LogitMr <- glm(bin~ age+gpa+sex.f+exp_n, family = binomial(link = "logit"), data = df1)
waldtest(logit, LogitMr, test="Chisq")
lrtest(logit, LogitMr)
anova(logit,LogitMr, test="Rao")


## tree design for international citizens:-
library(rpart)
tree_model<- rpart(formula= bin~age+gpa+gre_total+exp,data =df2,method="class")
summary(tree_model)
plot(tree_model)
printcp(tree_model)
plotcp(tree_model)
plot(tree_model, uniform=TRUE, 
     main="Classification Tree for Lindner_Int'l")
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
cft <- ctree(bin ~ age + gpa + gre_total +exp, 
             data=df2)
plot(cft, main="Conditional Inference Tree for Lindner_int'l")

cf1<-  ctree(bin ~ age + gpa + distance +exp, 
             data=df2, controls = ctree_control(maxsurrogate = 3))
plot(cf1, main="Conditional Inference Tree for Lindner_USA")

# Regression Tree Example
library(rpart)

reg <- rpart(bin ~ age + gpa + gre_total +exp, 
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

