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
#library(naniar)
#library(stringr)
library(dplyr)
library(PairedData)
source('code/functions.R')
options(scipen=999)
getwd()

# data ----
data_given <- read_csv('H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\data\\chigWeirDidson201618sjp.csv')

#1057/151
#first clean data
data_gathered <- data_given %>%
  dplyr::select(-starts_with("prop")) %>%
  gather(species, abundance, sockeye_10:total_60) %>%
  separate(species, c("species", "period"), sep ="_") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = base::factor(format(date, "%Y")),
         abundance = str_replace(abundance, "-", "0"),
         abundance = as.numeric(abundance),
         period = str_replace(period, "10", "ten_minute"),
         period = str_replace(period, "60", "sixty_minute")) %>% 
  filter(species %in% c("sockeye", "coho", "total"))
#filter(species %in% c("Chinook", "chum", "dolly-varden"))
#filter(species == "pink")

#Find data with NAs
data_NA <- data_gathered %>%
  filter_all(any_vars(is.na(.)))
#All data apears to be present.

data_wide1060 <- data_gathered %>% #suspicious vehicle
  spread(period, abundance)

unique(data_gathered$species)

# analysis ----
set.seed(1123)

# This first section is comparing how a 60 minute per hour count (census) compares with 
# a 10 minute per hour count that is then exapanded by 6. 

regressions <- data_wide1060 %>%
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

#Consider the no intercept regressions. This is liekly not needed since they approximately go through 0 anyway.
regressions <- data_wide1060 %>%
  nest(-species, -year, -method) %>%
  mutate(fit = map(data, ~lm(ten_minute ~ 0+sixty_minute, data = .x )),
         tidied = map(fit, tidy))

regressions%>%
  unnest(tidied)


#Regressions
#Facetted
#https://campus.datacamp.com/courses/data-visualization-with-ggplot2-2/chapter-2-coordinates-and-facets?ex=6
# 95% CIs
#https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
# gridded
#https://i.stack.imgur.com/vtrWf.png

# This graph combines all years together to see if there are trends by species
(fish_grid <- ggplot(data_wide1060 , aes(x = sixty_minute, y = ten_minute)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + #line y = x for reference
  geom_smooth(method=lm, se=TRUE) +
  theme(panel.grid.major = element_line("lightgray",0.5),
        panel.grid.minor = element_line("lightgray",0.25)) + 
  ggtitle("60 min vs 10 min") +
  facet_grid(method ~ species))

#This graph looks at sockeye and displays graphs by year and method (sonar or weir)
sockeyedat <- data_wide1060 %>% filter(species == "sockeye")

(sockeye_grid <- ggplot(sockeyedat, aes(x = sixty_minute, y = ten_minute)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + #line y = x for reference
  geom_smooth(method=lm, se=TRUE) +
  theme(panel.grid.major = element_line("lightgray",0.5),
        panel.grid.minor = element_line("lightgray",0.25)) + 
  ggtitle("Sockeye 60 min vs 10 min") +
  facet_grid(method ~ year))

#This graph looks at coho and displays graphs by year and method (sonar or weir)
cohodat <- data_wide1060 %>% filter(species == "coho")

(coho_grid <- ggplot(cohodat, aes(x = sixty_minute, y = ten_minute)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + #line y = x for reference
  geom_smooth(method=lm, se=TRUE) +
  theme(panel.grid.major = element_line("lightgray",0.5),
        panel.grid.minor = element_line("lightgray",0.25)) + 
  ggtitle("Coho 60 min vs 10 min") +
  facet_grid(method ~ year) )


#This graph looks at totals of all fish passing and displays graphs by year and method (sonar or weir)
# All fish: Chinook, chum, coho, pinks, dolly varden
totaldat <- data_wide1060 %>% filter(species == "total")

(total_grid <- ggplot(totaldat, aes(x = sixty_minute, y = ten_minute)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + #line y = x for reference
  geom_smooth(method=lm, se=TRUE) +
  theme(panel.grid.major = element_line("lightgray",0.5),
        panel.grid.minor = element_line("lightgray",0.25)) + 
  ggtitle("total 60 min vs 10 min") +
  facet_grid(method ~ year))

#Individual regression diagnositics and regression graphs follow.  These also have prediction intervals built in.
#These are preferable for publication purposes

##Sockeye
##weir
#set up particular data
this_species <- "coho"
this_method <- "sonar"
this_year <- 2016
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(sock2017weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(sock2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))
  
#graph
(sock2017weir_graph <- graph_10vs60(chignik, linear_model, this_year, this_method, this_species))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")



  
sockeye16sonar <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "sonar", this_year = 2016)
sockeye17sonar <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "sonar",this_year = 2017)
sockeye18sonar <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "sonar",this_year = 2018)
sockeye16weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir", this_year = 2016)
sockeye17weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir",this_year = 2017)
sockeye18weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir",this_year = 2018)
sockeye_values <- bind_rows(sockeye16sonar$values, sockeye17sonar$values, sockeye18sonar$values,sockeye16weir$values, sockeye17weir$values, sockeye18weir$values)

coho16sonar <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "sonar", this_year = 2016)
coho17sonar <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "sonar",this_year = 2017)
coho18sonar <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "sonar",this_year = 2018)
coho16weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir", this_year = 2016)
coho17weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir",this_year = 2017)
coho18weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir",this_year = 2018)
coho_values <- bind_rows(coho16sonar$values, coho17sonar$values, coho18sonar$values,coho16weir$values, coho17weir$values, coho18weir$values)

total16sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar", this_year = 2016)
total17sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar",this_year = 2017)
total18sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar",this_year = 2018)
total16weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir", this_year = 2016)
total17weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir",this_year = 2017)
total18weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir",this_year = 2018)
total_values <- bind_rows(total16sonar$values, total17sonar$values, total18sonar$values,total16weir$values, total17weir$values, total18weir$values)

values <- bind_rows(sockeye_values, coho_values, total_values)


(sort <- values[order(values$wilcox_pvalue),] )
#Because we are testing 2x3x3 = 18 hypotheses, then for an alpha level of 0.05, the Bonferroni correction is 0.05/18 = 0.002777778
# Since some of the residuals appear to be non i.i.d., we can use non-parametric statistics, using the Wilcoxon rank sum test 
# Ho: 60 minute count and the 10 minute estimate of fish passage are equivalent.
# Ha: 60 minute count and the 10 minute estimate of fish passage are not equivalent.
# In each of the 18 cases we fail to reject the null hypothesis.

(sort <- values[order(values$shapiro_pvalue),] )
#For 12 the 18 cases the data is normally distributed and hypothesis testing on the linear regression is appropriate
# Here the Bonferroni correction is 0.05/12 = 0.004166667
# Testing the slopes against the slope = 1
# Ho: The slope is equivalent to 1 
# Ho: The solope is not equivalent to 1
# In all but one case we fail to reject the null hypothesis. That case is the 2016 sonar estimate for coho. 
#
(sort <- values[order(values$slope_eq1_pvalue),] )

(sort <- values[order(values$species, values$method, values$year),] )

table_values <- sort %>%
  mutate(slope_eq1_pvalue = replace(slope_eq1_pvalue, sort$shapiro_pvalue < 0.05, NA),
         adj_r_squared = replace(adj_r_squared, sort$shapiro_pvalue < 0.05, NA)
         )

print(table_values, digits = 4)
mydata$error[mydata$variable == "cla5C"] = NA 

sockeyegraphs <- cowplot::plot_grid(sockeye16sonar$graph, sockeye17sonar$graph, sockeye18sonar$graph, sockeye16weir$graph, sockeye17weir$graph, sockeye18weir$graph, scale = c(1,1,1,1,1,1))
cohographs <- cowplot::plot_grid(coho16sonar$graph, coho17sonar$graph, coho18sonar$graph, coho16weir$graph, coho17weir$graph, coho18weir$graph, scale = c(1,1,1,1,1,1))
totalgraphs <- cowplot::plot_grid(total16sonar$graph, total17sonar$graph, total18sonar$graph, total16weir$graph, total17weir$graph, total18weir$graph, scale = c(1,1,1,1,1,1))

sonar2016 <- cowplot::plot_grid(sockeye16sonar$graph,coho16sonar$graph, total16sonar$graph)
sonar2017 <- cowplot::plot_grid(sockeye17sonar$graph,coho17sonar$graph, total17sonar$graph)



#####60 minute weir vs 60 minute sonar



##Sockeye
##weir vs sonar
#set up particular data
this_species <- "sockeye"
time_interval <- "weir60sonar60"
this_year <- 2017
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year, species == this_species) %>%
  filter(date != "2017-09-03") %>% # This date had outliers for numbers of fish for the sonar. 
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2017weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
sock2017weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year, species == this_species) %>%
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
sock2018weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
sock2018weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

#coho

this_species <- "coho"
time_interval <- "weir60sonar60"
this_year <- 2017
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year, species == this_species) %>%
  filter(date != "2017-09-03") %>% # This date had outliers for numbers of fish for the sonar. 
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2017weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2017weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year, species == this_species) %>%
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
coho2018weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
coho2018weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")



## total fish
this_species <- "total"
time_interval <- "weir60sonar60"
this_year <- 2017
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year) %>%
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
total2017weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
total2017weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_gathered %>% 
  dplyr::select(-ten_minute) %>% 
  filter(year == this_year) %>%
  spread(method, sixty_minute) %>%
  rename(sonar = "sonar")

# Linear Regression 
linear_model <- lm_weir60vssonar60(chignik)
summary(linear_model)# show results

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
total2018weir60sonar60_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)

#graph
total2018weir60sonar60_graph <- graph_weirvssonar(chignik, linear_model)
ggsave(paste0("figures/", this_year, time_interval, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

#cowplots

sockeyeweir60sonar60graphs <- cowplot::plot_grid(sock2017weir60sonar60_graph, sock2018weir60sonar60_graph, scale = c(1,1))
#sockeyesonar2017graphs <- cowplot::plot_grid(sock2017sonar_graph, sock2017sonar_graph_outlier, scale = c(1,1))
cohoweir60sonar60graphs <- cowplot::plot_grid(coho2017weir60sonar60_graph, coho2018weir60sonar60_graph, scale = c(1,1))
#cohosonar2017graphs <- cowplot::plot_grid(coho2017sonar_graph, coho2017sonar_graph_outlier, scale = c(1,1))
totalweir60sonar60graphs <- cowplot::plot_grid(total2017weir60sonar60_graph, total2018weir60sonar60_graph, scale = c(1,1))
#totalsonar2017graphs <- cowplot::plot_grid(total2017sonar_graph, total2017sonar_graph_outlier, scale = c(1,1))

weir60sonar60graphs <- cowplot::plot_grid(sockeyeweir60sonar60graphs, cohoweir60sonar60graphs, totalweir60sonar60graphs, ncol = 1)
#withandwithout_outliers <- cowplot::plot_grid(sockeyesonar2017graphs, cohosonar2017graphs, totalsonar2017graphs, ncol = 3)

#pvalues

sock2017weir60sonar60_pvalue 
sock2018weir60sonar60_pvalue 
coho2017weir60sonar60_pvalue 
coho2018weir60sonar60_pvalue 
total2017weir60sonar60_pvalue 
total2018weir60sonar60_pvalue 


#####10 minute weir vs 10 minute sonar

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

