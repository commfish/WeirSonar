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

#returns a p value from a t distribution testing if the slope of the line is equvalend to 1. (In which case x and y are interchageable.)
pvalue_of_t_test_slope_eq_1 <- function(linear_model = lm){
  pt(-abs((coef(summary(linear_model))[2,1]-1)/coef(summary(linear_model))[2,2]), df = summary(linear_model)$df[2])*2
}
#http://r-statistics.co/Linear-Regression.html

#returns graph of linear model, complete with confidence and predictive intervals, line x = y 
graph_10vs60 <- function(data, linear_model) { #, newpoint){
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
    #geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # add new point optional must specify newpoint when calling function.
    geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
    geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
    geom_abline(intercept = 0, slope = 1) + #line y = x for reference
    theme_bw() +
    theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
    xlab("60 minute per hour count") +
    ylab("10 minute per hour estimate") +
    ggtitle(paste0(this_year, " ", this_method, " ", this_species, " 10 min. vs 60 min."))
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
   #ggtitle("ten_minute vs sixty_minute")
  g.pred  
}

#functions for graphing diagnositics and returning a model
#note data will have to be filtered and arranged appropriately.

lm_10vs60 <- function(data){
  linear_model <- lm(ten_minute ~ sixty_minute , data = data)
  layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  plot(linear_model)
  return(linear_model)
}

lm_60vs10 <- function(data){
  linear_model <- lm(sixty_minute ~ ten_minute , data = data)
  layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  plot(linear_model)
  return(linear_model)
}
lm_weir60vssonar60 <- function(data){
  linear_model <- lm(weir ~ sonar , data = data)
  layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  plot(linear_model)
  return(linear_model)
}
lm_weir10vssonar10 <- function(data){
  linear_model <- lm(weir ~ sonar , data = data)
  layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  plot(linear_model)
  return(linear_model)
}
lm_weir60vssonar10 <- function(data){
  linear_model <- lm(weir60 ~ sonar10 , data = data)
  layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  plot(linear_model)
  return(linear_model)
}