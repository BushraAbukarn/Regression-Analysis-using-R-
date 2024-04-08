title: "Untitled"
output: html_document
date: "2023-08-26"

```{r Multiple Linear Regression}
job.satisfaction.data<- read.csv(file="C:/Users/bushr/OneDrive/Desktop/Example1.1Data.csv",
                                 header=TRUE, sep=",")
#Plotting histogram with fitted normal density
library(rcompanion)
plotNormalHistogram(job.satisfaction.data$score)
#Testing normality of distribution 
shapiro.test(job.satisfaction.data$score)
#Specifying reference levels
educ.rel<- relevel(job.satisfaction.data$educ, ref="master") 
gender.rel<- relevel(job.satisfaction.data$gender, ref="F")
#Fitting general linear model
summary(fitted.model<- glm(score ~ gender.rel + age + educ.rel, 
                           data=job.satisfaction.data, family=gaussian(link=identity)))
#Outputting estimated sigma
sigma(fitted.model)
#Checking model fit 
null.model<- glm(score ~ 1, data=job.satisfaction.data, 
                 family=gaussian(link=identity))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))
#Using fitted model for prediction
print(predict(fitted.model, data.frame(gender.rel="F", 
                                       age=40, educ.rel="bachelor")))
```


```{r Binary Logistic Regression}
qol.data <- read.csv(file = "C:/Users/bushr/OneDrive/Desktop/KneeOAData.csv", header = TRUE, sep=",")
#Specifying the reference 
gender.rel<- relevel(as.factor(qol.data$gender), ref ="male")
occupation.rel<- relevel(as.factor(qol.data$occupation), ref ="unskilled")
type.rel<- relevel(as.factor(qol.data$type), ref ="acute")
pain.location.rel<- relevel(as.factor(qol.data$pain_location), ref ="unilateral")
qol.rel<-relevel(as.factor(qol.data$QoL), ref = "good")
#Fitting the regression model 
summary(fitted.model <- glm(qol.rel ~ age + gender.rel + occupation.rel + BMI + type.rel + pain.location.rel,
                            data = qol.data, family = binomial(link=logit)))
#Prediction 
print(predict(fitted.model, type="response", data.frame(age=59, gender.rel="male", occupation.rel="skilled", 
                                                        BMI=20.3, type.rel="chronic", pain.location.rel="bilateral")))
```

```{r Poisson Regression}
med.adherence <- read.csv(file = "C:/Users/bushr/OneDrive/Desktop/medadherence_data.csv", header = TRUE, sep=",")
#Specifying the reference 
gender.rel <- relevel(as.factor(med.adherence$gender), ref = "F")
hypertension.rel <- relevel(as.factor(med.adherence$hypertension), ref = "1")
asthma.rel <- relevel(as.factor(med.adherence$asthma), ref = "1")
cholestrol.rel <- relevel(as.factor(med.adherence$cholesterol), ref = "1")
#Fitting the regression model 
summary(fitted.model<- glm(nomeddays ~ gender.rel + age + hypertension.rel + asthma.rel + 
                             cholestrol.rel + othermeds, data=med.adherence, 
                           family = poisson(link = log)))
#Prediction 
print(predict(fitted.model, data.frame(gender.rel="F", age=70, hypertension.rel="1", asthma.rel="1",
                                       cholestrol.rel="0", othermeds=3 ), type = "response"))
```

```{r Zero-Inflated Poisson Regression}
smoking.data<- read.csv(file="C:/Users/bushr/OneDrive/Desktop/Example5.3Data.csv", 
                        header=TRUE, sep=",")
library(pscl)
#Specifying reference category
health.rel<- relevel(smoking.data$health, ref="good")
#Fitting zero-inflated Poisson model
summary(fitted.model<- zeroinfl(cigarettes ~ gender + age|health.rel, 
                                data=smoking.data))
#Checking model fit
null.model<- zeroinfl(cigarettes ~ 1, data=smoking.data)
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))
#Using fitted model for prediction
print(predict(fitted.model, data.frame(gender="M", health.rel="good", 
                                       age=50)))
```

```{r Poisson Regression Incidence Rate}
alzheimers.data <- read.csv("C:/Users/bushr/OneDrive/Desktop/Alzheimers_data.csv", header=TRUE, sep=",")
#Specifying the reference
gender.rel <- relevel(as.factor(alzheimers.data$gender), ref = "female")
group.rel <- relevel(as.factor(alzheimers.data$group), ref = "Tx")
dementia.rel <- relevel(as.factor(alzheimers.data$dementia), ref = "severe")
#Regression Model 
summary(fitted.model <- glm(worsen ~ gender.rel + group.rel + dementia.rel + offset(log(duration)), 
                            data = alzheimers.data, family = poisson(link = log)))
#Prediction 
alzheimers.data$duration <-rep(1000,12)
pred.cases <- predict(fitted.model, alzheimers.data, type = "response")
sort(pred.cases, decreasing = TRUE)
```

```{r Polynomial Regression}

```

```{r Ridge Regression}

```

```{r Lasso Regression}

```

```{r Nonparametric Logistic Regression}
seizures.data<- read.csv(file="C:/Users/bushr/OneDrive/Desktop/seizures_data.csv", header=TRUE, sep=",")
#Specifying the reference
seizures.data$group.rel<- relevel(as.factor(seizures.data$group), ref="Tx")
seizures.data$gender.rel<- relevel(as.factor(seizures.data$gender), ref="M")
#Model 
library(gam)
logistic.fit <- gam(seizures ~ group.rel + gender.rel + ncomorb 
                    + lo(age) + lo(week), data=seizures.data, family=binomial)
coefficients(summary.glm(logistic.fit))
#Prediction
predict(logistic.fit, data.frame(group.rel="Tx", gender.rel="F", ncomorb=2,
                                 age=35, week=8), type="response")
```