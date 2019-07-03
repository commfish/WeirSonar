# notes ----
# Sarah Power
# sarah.power@alaska.gov
# 2019.06.21
# WeirSonar1

# load ----
source('code/functions.R')
options(scipen=999)
getwd()

# data ----
data_given <- read_csv('data/chigWeirDidson201618sjp.csv')

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
  filter(species %in% c("sockeye", "coho", "total"))  #The values of interest 

#Find data with NAs
data_NA <- data_gathered %>%
  filter_all(any_vars(is.na(.)))
#All data appears to be present.

# data used for comparing estimates based on counting 10 min/hour to a 60 minute 
data_wide1060 <- data_gathered %>% 
  # For sonar since species are apportioned based on seining, the total is only of interest here.
  filter(method == "weir" | (method == "sonar" & species == "total")) %>% 
  spread(period, abundance)

data_wide_weir_sonar <- data_gathered %>% 
  spread(method, abundance)

unique(data_gathered$species)

# analysis ----
set.seed(1123)

# This first section is comparing how a 60 minute per hour count (census) compares with 
# a 10 minute per hour count that is then exapanded by 6. 


#Individual regression diagnositics and regression graphs follow.  These also have prediction intervals built in.
#These are preferable for publication purposes

sockeye16weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir", this_year = 2016)
sockeye17weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir",this_year = 2017)
sockeye18weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir",this_year = 2018)
sockeye_values <- bind_rows(sockeye16weir$values, sockeye17weir$values, sockeye18weir$values)

coho16weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir", this_year = 2016)
coho17weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir",this_year = 2017)
coho18weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir",this_year = 2018)
coho_values <- bind_rows(coho16weir$values, coho17weir$values, coho18weir$values)

total16sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar", this_year = 2016)
total17sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar",this_year = 2017)
total18sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar",this_year = 2018)
total16weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir", this_year = 2016)
total17weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir",this_year = 2017)
total18weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir",this_year = 2018)
total_values <- bind_rows(total16sonar$values, total17sonar$values, total18sonar$values,total16weir$values, total17weir$values, total18weir$values)

values <- bind_rows(sockeye_values, coho_values, total_values)

(sort <- values[order(values$wilcox),] )
#Because we are testing 12 hypotheses, then for an alpha level of 0.05, the Bonferroni correction is 0.05/12 = 0.004166667
# Since some of the residuals appear to be non i.i.d., we can use non-parametric statistics, using the Wilcoxon rank sum test 
# Ho: 60 minute count and the 10 minute estimate of fish passage are equivalent.
# Ha: 60 minute count and the 10 minute estimate of fish passage are not equivalent.
# In each of the 12 cases we fail to reject the null hypothesis.

(sort <- values[order(values$shapiro),] )
# For 8 the 12 cases the data is normally distributed and hypothesis testing on the linear regression is appropriate
# Here the Bonferroni correction is 0.05/8 = 0.00625
# Testing the slopes against the slope = 1
# Ho: The slope is equivalent to 1 
# Ho: The slope is not equivalent to 1
# In all 8 cases we fail to reject the null hypothesis.
#
(sort <- values[order(values$slope_eq1),] )

(sort <- values[order(values$species, values$method, values$year),] )

(table_values_1060 <- sort %>%
  mutate_if(is.numeric, round, digits = 4) %>%
  mutate(slope_eq1 = replace(slope_eq1, sort$shapiro < 0.05, NA), # pvalues on not normally distributed data will be off
         adj_r_squared = replace(adj_r_squared, sort$shapiro < 0.05, NA))) # Same with r^2 value.

save(table_values, file = "output/table_values.Rda")

weirsockeyegraphs <- cowplot::plot_grid(sockeye16weir$graph, sockeye17weir$graph, sockeye18weir$graph,  ncol = 1, scale = c(1,1,1))
ggsave(paste0("figures/weirsockeyegraphs.png"),dpi=600, height=6, width=9, units="in")

weircohographs <- cowplot::plot_grid(coho16weir$graph, coho17weir$graph, coho18weir$graph, ncol = 1, scale = c(1,1,1))
ggsave(paste0("figures/weircohographs.png"), dpi=600, height=6, width=9, units="in")

cowplot::plot_grid(weirsockeyegraphs, weircohographs, ncol = 2)
ggsave(paste0("figures/weirsoockeyecoho1060graphs.png"), dpi=600, height=6, width=9, units="in")

(weirtotalgraphs <- cowplot::plot_grid(total16weir$graph, total17weir$graph, total18weir$graph,  ncol = 1, scale = c(1,1,1)))
ggsave(paste0("figures/weirtotalgraphs.png"),dpi=600, height=6, width=9, units="in")

sonartotalgraphs <- cowplot::plot_grid(total16sonar$graph, total17sonar$graph, total18sonar$graph, ncol = 1, scale = c(1,1,1))
ggsave(paste0("figures/sonartotalgraphs.png"),dpi=600, height=6, width=9, units="in")

cowplot::plot_grid(weirtotalgraphs, sonartotalgraphs, ncol = 2)
ggsave(paste0("figures/weirsonartotal1060graphs.png"), dpi=600, height=6, width=9, units="in")


##Additional code for looking at things.
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

# This graph combines all years together to see if there are trends by species
(fish_grid <- ggplot(data_wide1060 , aes(x = sixty_minute, y = ten_minute)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) + #line y = x for reference
    geom_smooth(method=lm, se=TRUE) +
    theme(panel.grid.major = element_line("lightgray",0.5),
          panel.grid.minor = element_line("lightgray",0.25)) + 
    ggtitle("60 min vs 10 min 2016-2018") +
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








#####60 minute weir vs 60 minute sonar

data_wide_weir_sonar60 <- data_wide_weir_sonar %>%
  filter(period == "sixty_minute")
this_species <- "sockeye"
this_year <- 2016 
this_period <- "sixty_minute"

#preparing data, also using in wilcoxon test
data_wide <- data_wide_weir_sonar60 %>%
  #Filter out wanted data
  filter(species == this_species, period == this_period, year == this_year) %>%
  # remove instances where there are weir counts but no sonar and vise versa
  filter(complete.cases(.))

#used in linear gression & graphing
data_g <- data_wide %>%
  gather(method, abundance, c("sonar", "weir"))

#create linear model
linear_model <- lm_weir60vssonar60(data_wide)
summary(linear_model)# show results



# Graph regression and put in figure file
(graph <- graph_weir_vs_sonar(data_wide, linear_model, this_year, this_period, this_species))
ggsave(paste0("figures/", this_species, this_period, this_year, "weir_sonar.png"),
       dpi=600, height=6, width=6, units="in")

#data <- data_wide_weir_sonar60
#
#this_species <- "sockeye"
#this_period <- "sixty_minute"
#this_year <- 2016
#linear_model <- [get code from pvalues_lm_graph_ws function]
data <- data_wide
#Use to make 95% CI and PI 
minweir <- min(data$weir, na.rm = TRUE)
maxweir <- max(data$weir, na.rm = TRUE)
predx <- data.frame(weir = seq(from = minweir, to = maxweir, by = (maxweir-minweir)/19))

# ... confidence interval
conf.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "confidence", level = 0.95))
# ... prediction interval
pred.int <- cbind(predx, predict(linear_model, newdata = predx, interval = "prediction", level = 0.95))

g.pred <- ggplot(pred.int, aes(x = weir, y = fit)) +
  geom_point(data = data, aes(x = weir, y = sonar)) + #plots all the points
  #geom_point(data = newpoint, aes(y = .fitted), size = 3, color = "red") + # add new point optional must specify newpoint when calling function.
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") + # prediction interval
  geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") + #confidence interval
  geom_smooth(method = "lm", show.legend = TRUE) + #attempt to add dashed line
  geom_abline(intercept = 0, slope = 1, lty = "dashed") + #line y = x for reference
  theme_bw() +
  theme(text = element_text(size=10), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  xlab(" ") +
  ylab(this_year) +
  theme(legend.position = "bottom") #?? trying to get a legend.
  #ggtitle(paste0(this_year, " ", this_species, " weir vs sonar ", this_period, "/hr"))
  g.pred
  legend_b <- get_legend(g.pred + theme(legend.position="bottom"))

sockeye16_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "sockeye", this_period = "sixty_minute", this_year = 2016)
sockeye17_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "sockeye", this_period = "sixty_minute", this_year = 2017)
sockeye18_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "sockeye", this_period = "sixty_minute", this_year = 2018)
sockeye_values <- bind_rows(sockeye16_60$values, sockeye17_60$values, sockeye18_60$values)

coho16_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "coho", this_period = "sixty_minute", this_year = 2016)
coho17_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "coho", this_period = "sixty_minute", this_year = 2017)
coho18_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "coho", this_period = "sixty_minute", this_year = 2018)
coho_values <- bind_rows(coho16_60$values, coho17_60$values, coho18_60$values)

total16_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "total", this_period = "sixty_minute", this_year = 2016)
total17_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "total", this_period = "sixty_minute", this_year = 2017)
total18_60 <- pvalues_lm_graph_ws(data = data_wide_weir_sonar60, this_species = "total", this_period = "sixty_minute", this_year = 2018)
total_values <- bind_rows(total16_60$values, total17_60$values, total18_60$values)

values <- bind_rows(sockeye_values, coho_values, total_values)

(sort <- values[order(values$wilcox),] )
#Because we are testing 9 hypotheses, then for an alpha level of 0.05, the Bonferroni correction is 0.05/9 = 0.005555556
# Since some of the residuals appear to be non i.i.d., we can use non-parametric statistics, using the Wilcoxon rank sum test 
# Ho: 60 minute weir count and the 60 minute sonar count of fish passage are equivalent.
# Ha: 60 minute weir count and the 60 minute sonar count of fish passage are not equivalent.
# In all but 2 cases we fail to reject the null hypothesis.

(sort <- values[order(values$shapiro),] )
# For 3 the 9 cases the data is normally distributed and hypothesis testing on the linear regression is appropriate
# Here the Bonferroni correction is 0.05/3 = 0.01666667
# Testing the slopes against the slope = 1
# Ho: The slope is equivalent to 1 
# Ho: The slope is not equivalent to 1
# In all but 1 cases we fail to reject the null hypothesis.
#
(sort <- values[order(values$slope_eq1),] )

(sort <- values[order(values$species, values$period, values$year),] )

(table_values_ws <- sort %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    mutate(slope_eq1 = replace(slope_eq1, sort$shapiro < 0.05, NA), # pvalues on not normally distributed data will be off
           adj_r_squared = replace(adj_r_squared, sort$shapiro < 0.05, NA))) # Same with r^2 value.

save(table_values_ws, file = "output/table_values_ws.Rda")
#cowplots

s <- sockeyeweir60sonar60graphs <- cowplot::plot_grid(sockeye16_60$graph, sockeye17_60$graph, sockeye18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Sockeye")
#legend <- get_legend(sockeye16_60$graph)
s <- plot_grid(title, s, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

c <- cohoweir60sonar60graphs <- cowplot::plot_grid(coho16_60$graph, coho17_60$graph, coho18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Coho")
c <- plot_grid(title, c, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

t <- totalweir60sonar60graphs <- cowplot::plot_grid(total16_60$graph, total17_60$graph, total18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Total")
t <- plot_grid(title, t, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

#p <- weir60sonar60graphs <- cowplot::plot_grid(sockeyeweir60sonar60graphs, cohoweir60sonar60graphs, totalweir60sonar60graphs, ncol = 3)
p <- weir60sonar60graphs <- cowplot::plot_grid(s, c, t, ncol = 3)

#Add title
title <- ggdraw() + draw_label("Comparision of weir census vs sonar census.")
p <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- add_sub(p, "Weir")
p <- add_sub(p, "___ line linear regression\n--- line y = x with slope = 1.", size = 10)
#p <- p + grid.text("y lable", a = unit(-3, "lines"), rot = 90)
#ggdraw(add_sub(plot, "Label", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

# y label
y.grob <- textGrob("Sonar", 
                   gp=gpar(col="black", fontsize=15), rot=90)

#add y label to plot  
#https://stackoverflow.com/questions/33114380/centered-x-axis-label-for-muliplot-using-cowplot-package
grid.arrange(arrangeGrob(p, left = y.grob))


ggdraw(p)
annotate_figure(p,
                top = text_grob("Visualizing mpg", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                right = "I'm done, thanks :-)!",
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave(paste0("figures/weir60sonar60graphs.png"), dpi=600, height=6, width=9, units="in")



###Extra code....
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

