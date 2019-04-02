# notes ----

# sarah.power@alaska.gov
# 2019.04.01
# WeirSonarfunctions

# load ----
library(tidyverse)
library(calibrate)
library(ggplot2)
library(broom)#for cleaning up data, used in prediction
library(caret)#used for cross validation 
#source('code/functions.R')

# functions ----

graph_10vs60 <- function(data){
  # Linear Regression 
  linear_model<- lm(ten_minute ~ sixty_minute , data=data)
  #Use to make 95% CI and PI 
  minsixty_minute <- min(data$sixty_minute, na.rm = TRUE)
  maxsixty_minute <- max(data$sixty_minute, na.rm = TRUE)
  predx <- data.frame(sixty_minute = seq(from = minsixty_minute, to = maxsixty_minute, by = (maxsixty_minute-minsixty_minute)/19))
  
  # ... confidence interval
  conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
  # ... prediction interval
  pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))


  g.pred <- ggplot(pred.int, aes(x = sixty_minute, y = fit)) +
    geom_point(data = data, aes(x = sixty_minute, y = ten_minute)) + #plots all the points
    geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
    geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
    theme_bw() +
    theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab("sixty_minute") +
    ylab("ten_minute") #+
  #ggtitle("ten_minute vs sixty_minute")
  g.pred  
}

graph_template <- function(data){
  # Linear Regression 
  linear_model<- lm(dep ~ indep , data=data)
  #Use to make 95% CI and PI 
  minindep <- min(data$indep, na.rm = TRUE)
  maxindep <- max(data$indep, na.rm = TRUE)
  predx <- data.frame(indep = seq(from = minindep, to = maxindep, by = (maxindep-minindep)/19))
  
  # ... confidence interval
  conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
  # ... prediction interval
  pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))
  
  
  g.pred <- ggplot(pred.int, aes(x = indep, y = fit)) +
    geom_point(data = data, aes(x = indep, y = dep)) + #plots all the points
    geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
    geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
    theme_bw() +
    theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab("indep") +
    ylab("dep") #+
  #ggtitle("dep vs Indep")
  g.pred  
}
