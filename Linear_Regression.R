setwd("C:\\Users\\arora\\OneDrive\\Desktop\\R Directory\\Project Stats")
# try this code - file=read.csv("..\\R Directory\\Project Stats")
file=read.csv("House.csv")
file[1:15,]
head(House.csv)
plot(Price~Assessed, data= file)
model= lm(Price~Assessed, data= file, y=TRUE)
#y = TURE we can ignore 
model
model2=lm(Price~Assessed+Time, data = file)
plot(model)
summary(model) 
# above will give more information 
model.summary= summary(model)
confint(model) 
predict(model, data.frame(Assessed=170))
predict(model, data.frame(Assessed=170), interval = "prediction")
predict(model, data.frame(Assessed=170), interval = "confidence")
#to calcluate the epsilon error
model.summary
model.summary$sigma
model.summary$r.squared
multiplemodel=lm(Price~Assessed+Type+Time, data=file)
multiplemodel
# to check if there is a relationship b/w dependent and indpendent variable  see F statistic - p value
# to find which independent variable have effect on dep see p value 
#p<alpha no =>significant
pairs(file)
# to sisulaise data in pairs
#type had 0 and 1 
# to test the overall significance see F statistic - p value
#if 0 is included in the CI , the factor is not significant
# Sometime we compare full and reduced model- we anova function to compare models
anova(model,model2, multiplemodel)
# to predict value - compare sigma (residual standard error) but for checking variations see Adjuted R square
model$residuals
#residual analysis - to compute error
plot(file$Assessed, model$residuals)
#the graph implies no relation 
# "Arima" model is used for financial analysis as its very precise

