# notes ----

# sarah.power@alaska.gov
# 2019.04.01
# WeirSonar1

# load ----
library(tidyverse)
library(calibrate)
library(ggplot2)
library(broom)#for cleaning up data, used in prediction
library(caret)#used for cross validation 
library(cowplot)
library(purrr)
source('code/functions.R')

# data ----
data_given <- read_csv('H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\data\\chigWeirDidson201718sjp.csv')
unique(data_given$species)

data_given  <- data_given  %>% 
  filter(species %in% c("sockeye", "coho")) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y"))

# analysis ----
set.seed(1123)
#options(scipen=999)

regressions <- data_given %>%
  nest(-species, -year, -method) %>%
  mutate(fit = map(data, ~lm(ten_minute ~ sixty_minute, data = .x )),
         tidied = map(fit, tidy),
         glanced = map(fit, glance),
         augmented = map(fit, augment)) 

regressions%>%
  unnest(tidied)

regressions %>%
  unnest(glanced, .drop = TRUE)
#regressions%>%
#  unnest(augmented, .drop = TRUE)

#Consider the no intercept regressions. 
regressions <- data_given %>%
  nest(-species, -year, -method) %>%
  mutate(fit = map(data, ~lm(ten_minute ~ 0+sixty_minute, data = .x )),
         tidied = map(fit, tidy))

regressions%>%
  unnest(tidied)

chignik <- data_given  %>% filter(year == 2017, species == "sockeye", method == "didson sonar") 
#chignik$dependent_var <- chignik$ten_minute
#chignik$independent_var <- chignik$sixty_minute

# Linear Regression 
linear_model<- lm(ten_minute ~ sixty_minute , data=chignik)
summary(linear_model) # show results
r2 = format(summary(linear_model)$r.squared, digits = 3)
RSS <- c(crossprod(linear_model$residuals))
MSE <- RSS / length(linear_model$residuals)
(RMSE <- sqrt(MSE))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(linear_model)

#Use to make 95% CI and PI 
minsixty_minute <- min(chignik$sixty_minute, na.rm = TRUE)
maxsixty_minute <- max(chignik$sixty_minute, na.rm = TRUE)
predx <- data.frame(sixty_minute = seq(from = minsixty_minute, to = maxsixty_minute, by = (maxsixty_minute-minsixty_minute)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
# ... prediction interval
pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))

# Graph
g.pred <- ggplot(pred.int, aes(x = sixty_minute, y = fit)) +
  geom_point(data = chignik, aes(x = sixty_minute, y = ten_minute)) + #plots all the points
  #geom_text(data = chignik, aes(x = sixty_minute, y = ten_minute, label = date), size = 4) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  #expand_limits(y=c(0 , 80000)) +
  #expand_limits(x=c(0 , 50)) + #export, save copy to clip board 1400 X 1200 for power point & 850 x 550 for document.
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  xlab("sixty_minute") +
  ylab("ten_minute") +
  ggtitle("ten_minute vs sixty_minute")
g.pred  




lm_data_fnc <- funtion(data){lm(ten_minute ~ 0 + sixty_minute, data) }

data <- data_given %>% filter(year == 2017, species == "sockeye", method == "didson sonar") 

# Linear Regression with intercept = 0. 
sock_2017_dids_int0<- lm(ten_minute ~ 0 + sixty_minute , data=data)
sock_2017_dids_int<- lm(ten_minute ~ sixty_minute , data=data)
sock_2017_dids_int0_plot <- graph_10vs60(data, sock_2017_dids_int0)
sock_2017_dids_int_plot <- graph_10vs60(data, sock_2017_dids_int)

data <- data_given %>% filter(year == 2018, species == "sockeye", method == "didson sonar") 

# Linear Regression with intercept = 0. 
sock_2018_dids_int0<- lm(ten_minute ~ 0 + sixty_minute , data=data)
sock_2018_dids_int<- lm(ten_minute ~ sixty_minute , data=data)
sock_2018_dids_int0_plot <- graph_10vs60(data, sock_2018_dids_int0)
sock_2018_dids_int_plot <- graph_10vs60(data, sock_2018_dids_int)

plot_grid(sock_2017_dids_int0_plot, sock_2018_dids_int0_plot,sock_2017_dids_int_plot, sock_2018_dids_int_plot)
plot(sock_2017_dids_int0)
plot(sock_2017_dids_int)
plot(sock_2018_dids_int0)
plot(sock_2018_dids_int)


summary(linear_model) # show results
(adj_r2 = format(summary(linear_model)$adj.r.squared, digits = 3))
#RMSE <- format(summary(linear_model)$rmse, digits = 3) # doesn't seem to work in all instances.

RSS <- c(crossprod(linear_model$residuals))
MSE <- RSS / length(linear_model$residuals)
(RMSE <- sqrt(MSE))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(linear_model)
plot(linear_model_int0)

#use to plot the new predicted point
#new_data <- data.frame(independent_var= 5000) #put in new data point
#newpoint <- broom::augment(linear_model, newdata = new_data)
#pred <- predict(linear_model, newdata = new_data, interval = "prediction", level = 0.95)

####Graphing
graph_10vs60(data, linear_model)
graph_10vs60(data, linear_model_int0)
#Repeated K- fold Cross validation # must make sure there are no NA's

# define training control 
train_control <- trainControl(method="repeatedcv", number=8, repeats=7)
#I used number of K-folds = 14 since I have 14*4 = 56 data points
length(data$dependent_var)/4
# train the model
model <- train(dependent_var ~ 0 + independent_var, data=data, trControl=train_control, method="lm")
#model <- train(dependent_var ~ independent_var, data=data, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

dependent_var_pred <- predict(model, data) # necessary step to get training RMSE
postResample(pred = dependent_var_pred, obs = data$dependent_var) #To get trainign RMSE
#Compare training RMSE to (full) RMSE
# If training RMSE is similare to full RMSE then expect similar predictive results in the future. 

