## Read in Data
getwd()
setwd("C:\\Users\\arora\\Desktop\\R Directory")
data=read.csv("train_madhura.csv")
names(data)
rownames(data)=data[,1]  ## remove ID
rownames(data)
data=data[,-1]
data
round(cor(data),2)
## Explore and relabel Data
y=data$CKD
str(y)
class(y)
class(data)
summary(data)
install.packages("faraway")
# to test multicollinearity  and remove the ones which are greater than 10
library("faraway")
vif(data)
#out_sample=which(is.na(data$CKD)==1)
#out_sample
#data_out=data[out_sample,]   ## the ones without a disease status
#data_out
#data_in=data[-out_sample,]   ## the ones with a disease status
#summary(data_in)
chisq.test(data$Hypertension, data$Fam.Hypertension)
chisq.test(data$Anemia, data$Fam.Hypertension)
chisq.test(data$Hypertension, data$CKD)
chisq.test(data$Fam.Hypertension, data$CKD)
chisq.test(data$Smoker, data$Hypertension)

?na.omit
#data_in=na.omit(data_in) #to remove the rows with missing values

## Run the Logistic Regression with one variable

names(data) #gives the column name
#modeltry=glm(CKD ~ 1, family="binomial", data=data) gives null model
#summary(modeltry)
model=glm(CKD~Age, family="binomial",data=data)
summary(model)
# the coeffienct of age is positive indicating that an increase in age is associated
#  with an increase in the probability of someone having CKD


##  Run the Logistic Regression on all data, explore backward elimination
dim(data)
modelfull=glm(CKD~.,family="binomial",data=data)
summary(modelfull)
# notice the logistic regression automatically separated the categorical variables

#model2=step(model,direction="forward")
#summary(model2)
model3=step(modelfull,direction="backward")
# this will run the ENTIRE model with all variables, and then remove one at a time according to a 
#  p-value of the coefficient. it will result in only those variables "which matter"
#   ** note - some consider this unethical because you see the results before you select variables

## Explore your new model
formula(model3)
summary(model3)

datapred=predict(model3, type="response")
datapred
class(datapred)
class(data)
type.convert(data.frame(datapred))
class(datapred)
dataf=cbind(data_in,datapred)
write.csv(dataf,"Datapred.csv")
?cbind
# liklihood ratio test
library(lmtest)
lrtest(model3)
# confidence intervals of the model coefficients (should not include 0 if p-value<.05)
confint.default(model3)
confint(model)
?with
##  Hypotehsis test of model, Compare 2 models, Definition 5-3
## difference in deviance  *this is sort of like R-squared but different because we use 0 or 1 only.
with(model3, null.deviance - deviance)
#null deviance means with no variables - deviance when the variables are added in the model
##df
# deviance is a goodness of the fit model statistic.. used as a sum of squares of residuals 
#layman language distance between two probabalistic models. Higher the number, bad is the fitness of the
#model
?deviance 
with(model3, df.null - df.residual) # df is degree of freedom  i.e. 21 - 1 
## pvalue of difference with(model3, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# if <.05, then model is significant from a null model (model with no variables)

## Alternate. Ho:  Model Fits the Data, Ha: Model does not Fit
-2*logLik(model)  # AIC formula
## test multicollinearity of categorical value using chisquare test
with(model3, pchisq(deviance, df.residual, lower.tail = FALSE))
datacat=data[,c(3:6, 10:13)]
datarace=data[,c(3:6)]
chisq.test(datacat$Racegrp.black,datacat$Racegrp.hispa, correct=FALSE)
chisq.test(data$CVD,data$Fam.CVD, correct=FALSE) # these both are not collinear
chisq.test(data$Hypertension,data$Fam.Hypertension, correct=FALSE) # these both are not collinear
# Race black and hisp are collinear and creates mutliollineaity we will remove less collinear 
#variable black from our model

#finalmodel=glm(CKD~Age+Female+Racegrp.hispa+Height+DBP+Obese+LDL+Total.Chol+PVD+Activity+Hypertension+Diabetes
                    #   +CVD+Fam.CVD+CHF+Anemia,family="binomial",data=data) # with p value<.08/.07
finalmodel1=glm(CKD~Age+Female+Racegrp.hispa+Height+LDL+Total.Chol+PVD+Activity+Hypertension+Diabetes
               +CVD+CHF+Anemia,family="binomial",data=data)  # with only p value <.05
summary(finalmodel1) # final model 
summary(model3)

data$to
datapred=predict(finalmodel1, type="response")
datapred
class(datapred) # since its numeric convert to daatfram
class(data)
datapred=type.convert(data.frame(datapred))
dataf=cbind(data,datapred)
write.csv(dataf,"Datapred.csv")

?pchisq
## Predict probabilities and Odds Ratios of New Data
## predictions of new data
testdata=read.csv("test_rishabh.csv")
testdata
testdata1=testdata[,-1] 
testdata1 
phatnew=predict(finalmodel1, newdata = testdata1, type = "response")
phatnew
summary(phatnew)
str(phatnew)
class(phatnew)
phatnewdf=as.data.frame(phatnew)
# it gives probabilities 

## odds ratios
phatnew/(1-phatnew)
#  it's the prob. of it happening/prob. of it not happening
#  related to gambling and betting on something

##  Predict and Plot in-sample data
phat3=predict(finalmodel1,type="response")  # predicts for ALL in sample data # if data is not mentioned
phat3# it takes the data of the model mentioned in the argument
summary(phat3)  # probabilities range from .01% to 83.4%   - cool!


# let's compare that to a "simple" model with only age
model=glm(CKD~Age,family="binomial",data=data)
Age=seq(0,100,by=.1)
Age
class(Age)
phat=predict(model,list(Age = Age),type="response")
summary(phat)  # range from 0.02% to 76.2%, it's similar but doesn't fit as well
plot(data$Age,data$CKD, pch = 16, xlab = "Age", ylab = "CKD")
lines(Age, phat)
data
## plot the actual probabilities for the "complicated" model
plot(data$Age, data$CKD, pch = 16, xlab = "Age", ylab = "CKD")
points(testdata1$Age, phat3,col="blue")  # this plots all phat3's according to age

## classification
summary(phat3)
classify=ifelse(phat3>.5,1,0)  # this is a threshold, we say if probability >50% , then say "yes"
summary(classify) # notice that not many are "yes"  - is this desirable?
classify

as.data
c_accuracy(data$CKD,classify)  # to run this you must run my code below first.

# notice these are the accuracy results when you use 50% as the threshold. they are kind of extreme
#    the false positive rate is almost 0% BECAUSE you almost NEVER say "yes"
#         true positive rate is 13.9% which isn't that bad because you have almost no false positives

## Caclculate Costs
acc=c_accuracy(testdata1$CKD,classify)
c1=100   # penalize me  $100 for a false positive
c2=200  #  penalize me $200 for a false negatives
cost=acc[9]*c1+acc[10]*c2

cost 
