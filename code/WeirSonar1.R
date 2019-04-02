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
source('code/functions.R')

# data ----
data <- read_csv('H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\data\\chigWeirDidson201718sjp.csv')

data <- data %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y"))

# analysis ----
set.seed(1123)

unique(data$species)

data <- data %>% filter(year == 2017, species == "sockeye") 
data$dependent_var <- data$ten_minute
data$independent_var <- data$sixty_minute


# Linear Regression 
linear_model<- lm(dependent_var ~ independent_var , data=data)

summary(linear_model) # show results
adj_r2 = format(summary(linear_model)$adj.r.squared, digits = 3)
RSS <- c(crossprod(linear_model$residuals))
MSE <- RSS / length(linear_model$residuals)
(RMSE <- sqrt(MSE))

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(linear_model)

#use to plot the new predicted point on the graph
new_data <- data.frame(independent_var= 1700) #put in new data point
newpoint <- broom::augment(linear_model, newdata = new_data)
pred <- predict(linear_model, newdata = new_data, interval = "prediction", level = 0.95)
lwr <- pred[2]
upr <- pred[3]


####Graphing
graph_10vs60(data) 

#Use to make 95% CI and PI 
minindependent_var <- min(data$independent_var, na.rm = TRUE)
maxindependent_var <- max(data$independent_var, na.rm = TRUE)
predx <- data.frame(independent_var = seq(from = minindependent_var, to = maxindependent_var, by = (maxindependent_var-minindependent_var)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
# ... prediction interval
pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))
###########not above section***************



# The font below is sized for power point presentation. Make smaller if in a document.
g.pred <- ggplot(pred.int, aes(x = independent_var, y = fit)) +
  geom_point(data = data, aes(x = independent_var, y = dependent_var)) + #plots all the points
  #geom_text(data = data, aes(x = independent_var, y = dependent_var, label = date), size = 6) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  #geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # adds this years new point
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  xlab("independent_var") +
  ylab("dependent_var") #+
  #ggtitle("dependent_var vs Independent_var")
g.pred  


#Repeated K- fold Cross validation # must make sure there are no NA's

# define training control 
length()
train_control <- trainControl(method="repeatedcv", number=8, repeats=7)
#I used number of K-folds = 14 since I have 14*4 = 56 data points
length(data$dependent_var)/4
# train the model
model <- train(dependent_var ~ independent_var, data=data, trControl=train_control, method="lm")
# summarize results
print(model) #get full model RMSE (Root Mean Square Error)

dependent_var_pred <- predict(model, data) # necessary step to get training RMSE
postResample(pred = dependent_var_pred, obs = data$dependent_var) #To get trainign RMSE
#Compare training RMSE to (full) RMSE
# If training RMSE is similare to full RMSE then expect similar predictive results in the future. 

