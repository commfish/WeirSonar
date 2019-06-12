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
data_NA<- data_gathered %>%
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
this_species <- "sockeye"
this_method <- "weir"
this_year <- 2017
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(sock2017weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(sock2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))
  
#graph
(sock2017weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


pvalues_lm_graph <- function(data = data_gathered, this_species, this_year, this_method){
  #Filter out wanted data
  data_gathered <- data_gathered %>% filter(year == this_year, species == this_species, method == this_method)
  
  #Non- parametric test Ho: period of time counting estimates are the same Ha: estimates are different
  wilcox_out <- wilcox.test(abundance ~ period, data = data_gathered, paired = TRUE, alternative = "two.sided")
  wilcox_pvalue <- wilcox_out$p.value
  
  #prepare data for parametic tests & graphing
  data_wide1060 <- data_gathered %>% 
    spread(period, abundance)
  
  #data_wide1060 <- chignik
  
  #create linear model
  linear_model <- lm_10vs60(data_wide1060)
  summary(linear_model)# show results
  
  #Test for normality of residuals
  shapiro_out <- shapiro.test(linear_model$residuals)
  shapiro_pvalue <- shapiro_out$p.value
  
  #Note the following p_values & R squared are only really valid if shapiro_pvalue > 0.05
  #Test to see if linear regression is statistically significant (in this case aka slope is statistically sig)
  lm_pvalue <- coef(summary(linear_model))[2,4]
  
  #adjusted r squared for those that like it.
  adj_r_squared <- summary(linear_model)$adj.r.squared
  
  #Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
  slope_eq1_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model)
  
  # Graph regression and put in figure file
  (sock2018weir_graph <- graph_10vs60(data_wide1060, linear_model))
  ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
         dpi=600, height=6, width=6, units="in")
  
  # return data frame of pvalues and adj.r.squared
  df <- data.frame(wilcox_pvalue, slope_eq1_pvalue, shapiro_pvalue, lm_pvalue, adj_r_squared, slope_eq1_pvalue)
  return(df)
}
  

pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_year = 2017 , this_method = "sonar")

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(sock2018weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(sock2018weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(sock2018weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

##sonar
#set up particular data
this_species <- "sockeye"
this_method <- "sonar"
this_year <- 2017
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(sock2017sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(sock2017sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(sock2017sonar_graph <- graph_10vs60(chignik, linear_model))  
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(sock2018sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(sock2018sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(sock2018sonar_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

#Coho
#set up particular data
this_species <- "coho"
this_method <- "weir"
this_year <- 2017
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 
wilcox.test(abundance ~ period, data = chignik, paired = TRUE, alternative = "two.sided")
wilcox.test(chignik$sixty_minute, chignik$ten_minute, paired = FALSE, alternative = "two.sided")


# for box plots to visualize paired data: perhaps this is not needed.
# Subset abundance data by period
sixty <- subset(chignik,  period == "sixty_minute", abundance, drop = TRUE)
#Estimates are not stistically significantly different with a p value of 0.3014
ten <- subset(chignik,  period == "ten_minute", abundance, drop = TRUE)

# Plot paired data
pd <- paired(sixty, ten)
plot(pd, type = "profile") + theme_bw()

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(coho2017weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are NOT normally distributed
length(chignik$sixty_minute)
resid(linear_model)
plot(linear_model$fit, resid(linear_model))
abline(0,0)

chignik$log10min <- log(chignik$ten_minute + 1)
chignik$log60min <- log(chignik$sixty_minute + 1)

linear_model <- lm(log10min ~ log60min, data = chignik)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(linear_model)
return(linear_model)

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(coho2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))
 
#graph
(coho2017weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")
minlog60min <- min(chignik$log60min, na.rm = TRUE)
maxlog60min <- max(chignik$log60min, na.rm = TRUE)
predx <- data.frame(log60min = seq(from = minlog60min, to = maxlog60min, by = (maxlog60min-minlog60min)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
# ... prediction interval
pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))

g.pred <- ggplot(pred.int, aes(x = log60min, y = fit)) +
  geom_point(data = chignik, aes(x = log60min, y = ten_minute)) + #plots all the points
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
#graph
# did I cut stuff out here?

data <- chignik
minsqrt_log60min <- min(data$sqrt_log60min, na.rm = TRUE)
maxsqrt_log60min <- max(data$sqrt_log60min, na.rm = TRUE)
predx <- data.frame(sqrt_log60min = seq(from = minsqrt_log60min, to = maxsqrt_log60min, by = (maxsqrt_log60min-minsqrt_log60min)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
# ... prediction interval
pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))

g.pred <- ggplot(pred.int, aes(x = sqrt_log60min, y = fit)) +
  geom_point(data = data, aes(x = sqrt_log60min, y = sqrt_log10min)) + #plots all the points
  #geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # add new point optional must specify newpoint when calling function.
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  geom_abline(intercept = 0, slope = 1) + #line y = x for reference
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  xlab("Squareroot of 60 minute per hour count") +
  ylab("Squareroot of 10 minute per hour estimate") +
  ggtitle(paste0(this_year, " ", this_method, " ", this_species, " squareroot of 10 min. vs squareroot of 60 min."))
g.pred  
#Question is it possible that misassignment from abundant sockeye to less than abundant coho could create this difference?
coho2017weir_graph <- graph_10vs60(chignik, linear_model)
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(coho2018weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are not normally distributed


#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(coho2018weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(coho2018weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


##sonar
#set up particular data
this_species <- "coho"
this_method <- "sonar"
this_year <- 2017
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method)  %>% 
  filter(date != "2017-09-03")

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(coho2017sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are NOT normally distributed
#Residuals are not normally distributed.

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(coho2017sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(coho2017sonar_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")
### coho 2017 sonar with outlier
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method)

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results 
(coho2018sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(coho2018sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(coho2018sonar_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


## For total fish##############
#set up particular data
this_species <- "total"
this_method <- "weir"
this_year <- 2017
chignik <- data_wide1060 %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(total2017weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(total2017weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(total2017weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(total2018weir_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(total2018weir_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(total2018weir_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


##sonar
#set up particular data
this_species <- "total"
this_method <- "sonar"
this_year <- 2017
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(total2017sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(total2017sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(total2017sonar_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")

this_year <- 2018
chignik <- data_wide1060  %>% filter(year == this_year, species == this_species, method == this_method) 

# Linear Regression 
linear_model <- lm_10vs60(chignik)
summary(linear_model)# show results
(total2018sonar_shapiro <- shapiro.test(linear_model$residuals)) # Ho: Residuals are normally distributed

#Test: Ho: The slope of the line is = 1. (AKA methods are equivalent)
(total2018sonar_pvalue <- pvalue_of_t_test_slope_eq_1(linear_model))

#graph
(total2018sonar_graph <- graph_10vs60(chignik, linear_model))
ggsave(paste0("figures/", this_year, this_method, this_species, ".png"),
       dpi=600, height=6, width=6, units="in")


sockeyegraphs <- cowplot::plot_grid(sock2017sonar_graph, sock2018sonar_graph, sock2017weir_graph, sock2018weir_graph, scale = c(1,1,1,1))
cohographs <- cowplot::plot_grid(coho2017sonar_graph, coho2018sonar_graph, coho2017weir_graph, coho2018weir_graph, scale = c(1,1,1,1))
totalgraphs <- cowplot::plot_grid(total2017sonar_graph, total2018sonar_graph, total2017weir_graph, total2018weir_graph, scale = c(1,1,1,1))



sock2017weir_pvalue
sock2018weir_pvalue
sock2017sonar_pvalue
sock2018sonar_pvalue
coho2017weir_pvalue
coho2018weir_pvalue
coho2017sonar_pvalue
coho2018sonar_pvalue
total2017weir_pvalue
total2018weir_pvalue
total2017sonar_pvalue
total2018sonar_pvalue

sock2017weir_shapiro
sock2018weir_shapiro
sock2017sonar_shapiro
sock2018sonar_shapiro
coho2017weir_shapiro
coho2018weir_shapiro
coho2017sonar_shapiro
coho2018sonar_shapiro
total2017weir_shapiro
total2018weir_shapiro
total2017sonar_shapiro
total2018sonar_shapiro

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

