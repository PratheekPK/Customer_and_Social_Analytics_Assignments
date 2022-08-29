setwd("C:/Users/chaff/Desktop/Boring Adult Stuff/School Work/UC Irvine/Winter Quarter/Customer and Social Analytics/Star Digital Project")
dat = read.csv('star_digital.csv')
dat

summary(dat)
######################### 1 #####################################
## Effectiveness of Advertisement 

ad.test = t.test(dat$purchase ~ dat$test)
ad.test 

ad.test1 = t.test(rowSums(dat[,4:9]) ~ dat$test)
ad.test1

ad.lift = (ad.test$estimate[2] - ad.test$estimate[1]) / ad.test$estimate[1]
ad.lift.perc = ad.lift*100
ad.lift.perc

#Effect size = (mean(test) - mean(control))/std(purchase)

effect_size <- (0.5048792 - 0.4856928)/sd(dat$purchase)
effect_size

corr = cor(dat)
melt_corr = melt(corr)
summary(melt_corr)
ggplot(data = melt_corr, aes(x = Var1, y = Var2,fill = value))+
  geom_tile()

ad.effect = glm(purchase~test, dat, family=binomial())
summary(ad.effect)
######################### 2 #####################################
## Frequency Effect of Advertisement 
attach(dat)

## Creating variables to capture all six impressions for both the groups
Total_imp = dat$imp_1 + dat$imp_2 + dat$imp_3 + dat$imp_4 + dat$imp_5 + dat$imp_6

## Creating Variables for capturing effect of impression on Test group only 
Total_imp_test = Total_imp * test

## Creating Variables for capturing effect of impression on Control group only 
test_bar = abs(test-1)
Total_imp_test_bar = Total_imp * test_bar

dat$Total_imp = Total_imp
dat$Total_imp_test = Total_imp_test
dat$test_bar = test_bar
dat$Total_imp_test_bar = Total_imp_test_bar

## Running Logistic Regression showing effect of impression on both groups 
# and test group only

fit.full = glm(dat$purchase ~ dat$Total_imp + dat$Total_imp_test, dat,family=binomial())
summary(fit.full)
exp(coef(fit.full))

## Running Logistic Regression showing effect of impression on both groups 
# and control group only

freq_model = glm(dat$purchase ~ dat$Total_imp + dat$Total_imp_test_bar, dat,family=binomial())
summary(freq_model)

## Running a logistic regression of total impressions to estimate whether there is correlation between number of 
#impressions and increase of purchase odds

full_imp_model = glm(purchase ~ imp_1 + imp_2 + imp_3 + imp_4 + imp_5 + imp_6, dat,family=binomial())
summary(full_imp_model)
exp(coef(full_imp_model))

total_imp_model = glm(purchase ~ Total_imp, dat,family=binomial())
summary(total_imp_model)
exp(coef(total_imp_model))

sites1to5 = dat$imp_1 + dat$imp_2 + dat$imp_3 + dat$imp_4 + dat$imp_5
five_imp_model = glm(purchase ~ imp_1 + imp_2 + imp_3 + imp_4 + imp_5, dat,family=binomial())
summary(five_imp_model)
exp(coef(five_imp_model))

five_imp_model2 = glm(purchase ~ sites1to5, dat,family=binomial())
summary(five_imp_model2)
exp(coef(five_imp_model2))

imp_six_model = glm(purchase ~ imp_6, dat,family=binomial())
summary(imp_six_model)
exp(coef(imp_six_model))
##############################3#####################################
## Conversion Effectiveness 
dat$i.sum = dat$imp_1 + dat$imp_2 + dat$imp_3 + dat$imp_4 + dat$imp_5
dat$t15_tst = dat$i.sum*dat$test
dat$t6_tst = dat$imp_6*dat$test
str(dat)


fit.full.2 = glm(dat$purchase ~ dat$i.sum + (dat$t15_tst) + dat$imp_6 + (dat$t6_tst), dat, family = binomial())
summary(fit.full.2)

pred.fit = confint(fit.full.2, dat, level = 0.95)
pred.fit
############################## 4 #####################################
## What site should Star Digital Advertise on? 
dat$rowsums5 = rowSums(dat[,4:8])	

imp6_fit <- glm(dat$purchase ~ dat$imp_6, dat, family=binomial())
summary(imp6_fit)
exp(coef(imp6_fit))

imp1to5_fit = glm(dat$purchase ~ dat$rowsums5, dat, family=binomial())
summary(imp1to5_fit)
exp(coef(imp1to5_fit))
