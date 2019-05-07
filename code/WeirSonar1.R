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
source('WeirSonar/code/functions.R')

# data ----
data_given <- read_csv('H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\data\\chigWeirDidson201718sjp.csv')

unique(data_given$species)

data_given  <- data_given  %>% 
  #filter(species %in% c("sockeye", "coho")) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y")) 

total_fish  <- data_given  %>% 
  group_by(date) %>%
  summarise(total_fish10 = sum(ten_minute),
            total_fish60 = sum(sixty_minute))
total_fish  <- total_fish  %>% 
  mutate(year = year(date))

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

#Individual regression diagnositics and regression graphs. 

##Sockeye
##weir
#set up particular data
this_species <- "sockeye"
this_method <- "weir"
this_year <- 2017
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

  pt(q = (coef(summary(linear_model))[2,1]-1)/coef(summary(linear_model))[2,2], df = summary(linear_model)$df[2])

  
  #https://stats.stackexchange.com/questions/245350/how-to-compare-my-slope-to-1-rather-than-0-using-regression-analysis-and-t-distr?noredirect=1&lq=1
  #https://stats.stackexchange.com/questions/137118/testing-a-regression-coefficient-against-1-rather-than-0
  
#graph
sock2017weir_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2018weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
sock2018weir_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

##sonar
#set up particular data
this_species <- "sockeye"
this_method <- "didson sonar"
this_year <- 2017
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2017sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
sock2017sonar_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2018sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
sock2018sonar_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

#Coho
#set up particular data
this_species <- "coho"
this_method <- "weir"
this_year <- 2017
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2017weir_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2018weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2018weir_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


##sonar
#set up particular data
this_species <- "coho"
this_method <- "didson sonar"
this_year <- 2017
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2017sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2017sonar_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_given  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2018sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2018sonar_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("WeirSonar/figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

sockeyegraphs <- cowplot::plot_grid(sock2017weir_graph, sock2018weir_graph, sock2017sonar_graph, sock2018sonar_graph, scale = c(1,1,1,1))
cohographs <- cowplot::plot_grid(coho2017weir_graph, coho2018weir_graph, coho2017sonar_graph, coho2018sonar_graph, scale = c(1,1,1,1))


sock2017weir_pvalue
sock2018weir_pvalue
sock2017sonar_pvalue
sock2018sonar_pvalue
coho2017weir_pvalue
coho2018weir_pvalue
coho2017sonar_pvalue
coho2018sonar_pvalue

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

