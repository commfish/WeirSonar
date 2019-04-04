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

#returns graph of linear model, complete with confidence and predictive intervals, also graphs model diagnositics
#Option of graphing an additionl point, but must pass data to the function and uncomment a line in ggplot. 
graph_10vs60 <- function(data, linear_model) { #, newpoint){
  # Linear Regression 
  #linear_model<- lm(ten_minute ~ 0 + sixty_minute , data=data)
  
  #print diagnostics
  #layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
  #plot(linear_model)
  
  #use to plot the new predicted point
  #new_data <- data.frame(sixty_minute= newpoint) #put in new data point
  #newpoint <- broom::augment(linear_model, newdata = new_data)
  
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
    xlab("sixty minute ") +
    ylab("ten minute per hour counts to estimate daily passage") +
    ggtitle(paste0(data$species[1], " ", data$year[1], " ", data$method[1], " 10 min. vs. 60 min. daily passage estimation"))
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
   #ggtitle("ten_minute vs sixty_minute")
  g.pred  
}

