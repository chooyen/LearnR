#Fit a model using Age and Height
model1 <- lm(LungCap~Age+Height)

#get a summary of model
summary(model1)

#Calculate Pearson corelation for Age and height
cor(Age,Height,method="pearson")

#Ask for a confidence interval for the model
confint(model1,conf.level=0.95)

#fit for all models
model2 <- lm(LungCap~Age+Height+Smoke+Gender+Caesarean)

#Ask for a summary
summary(model2)

#Check the diagnostic plots for this model

plot(model2)
