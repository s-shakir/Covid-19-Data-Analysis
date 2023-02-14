#--------Libraries---------

library(readr)
library(ggplot2)


#--------Data Set for Task 2-------

dataset <- read.csv("F:\\1.Masters in Data Science (Fast)\\Stats\\Assignments\\assignment no 3\\Covid-19_Dataset.csv")
View(dataset)


#Task 2: Import the "Covid 19 Dataset" in RStudio and perform the following tasks on this data set.

#Task 2.1: Generate scatter plot graphs for each variable with respect to the 'Date' variable.

plot(as.Date(dataset$Date), dataset$Confirmed)

plot(as.Date(dataset$Date), dataset$Deaths)

plot(as.Date(dataset$Date), dataset$Recovered)

plot(as.Date(dataset$Date), dataset$Active)


#Task 2.2: Calculate exponential and logistic regression models for each variable with respect to the 'Date' variable.


#--------Calculating Exponential Model-------

lm_conf <- lm(log(dataset$Confirmed)~dataset$Date)

exp_conf <- exp(predict(lm_conf, dataset$Date))


lm_dth <- lm(log(dataset$Deaths)~dataset$Date)

exp_dth <- exp(predict(lm_dth, dataset$Date))


lm_rec <- lm(log(dataset$Recovered)~dataset$Date)

exp_rec <- exp(predict(lm_rec, dataset$Date))


lm_act <- lm(log(dataset$Active)~dataset$Date)

exp_act <- exp(predict(lm_act, dataset$Date))



#--------Calculating Logistic Model-------

fit_con = glm(Date ~ Confirmed, data = dataset, family = binomial)

newdat_con <- data.frame(Confirmed=seq(min(dataset$Confirmed), max(dataset$Confirmed),len=188))

newdat_con$Date = predict(fit_con, newdata = newdat_con, type = "response")


fit_de = glm(Date ~ Deaths, data = dataset, family = binomial)

newdat_de <- data.frame(Deaths=seq(min(dataset$Deaths), max(dataset$Deaths),len=188))

newdat_de$Date = predict(fit_de, newdata = newdat_de, type = "response")


fit_re = glm(Date ~ Recovered, data = dataset, family = binomial)

newdat_re <- data.frame(Recovered=seq(min(dataset$Recovered), max(dataset$Recovered),len=188))

newdat_re$Date = predict(fit_re, newdata = newdat_re, type = "response")



fit_ac = glm(Date ~ Active, data = dataset, family = binomial)

newdat_ac <- data.frame(Active=seq(min(dataset$Active), max(dataset$Active),len=188))

newdat_ac$Date = predict(fit_ac, newdata = newdat_ac, type = "response")



#Task 2.3: Plot the regression models

#--------Plotting Exponential Model-------

plot(as.Date(dataset$Date), dataset$Confirmed)

lines(as.Date(dataset$Date), exp_conf, lwd=2, col = "red")


plot(as.Date(dataset$Date), dataset$Deaths)

lines(as.Date(dataset$Date), exp_dth, lwd=2, col = "red")


plot(as.Date(dataset$Date), dataset$Recovered)

lines(as.Date(dataset$Date), exp_rec, lwd=2, col = "red")


plot(as.Date(dataset$Date), dataset$Active)

lines(as.Date(dataset$Date), exp_act, lwd=2, col = "red")


#--------Plotting Logistic Model-------

plot(as.Date(dataset$Date), dataset$Confirmed)

ggplot(newdat_con, aes(x=Date, y=Confirmed)) + geom_line()



plot(as.Date(dataset$Date), dataset$Deaths)

ggplot(newdat_de, aes(x=Date, y=Deaths)) + geom_line()


plot(as.Date(dataset$Date), dataset$Recovered)

ggplot(newdat_re, aes(x=Date, y=Recovered)) + geom_line()


plot(as.Date(dataset$Date), dataset$Active)

ggplot(newdat_ac, aes(x=Date, y=Active)) + geom_line()



#Task 2.4: Identify for which plot the exponential model best fits and for which plot logistic model best fits.

# Exponential model fits the given data best as the growth starts off slow and then grows exponentially hence it fits the exponential regression model prefectly
