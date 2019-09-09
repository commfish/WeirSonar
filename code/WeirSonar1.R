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

# data used for comparing estimates based on weir vs sonar
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
sockeye17weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir", this_year = 2017)
sockeye18weir <- pvalues_lm_graph(data = data_gathered, this_species = "sockeye", this_method = "weir", this_year = 2018)
sockeye_values <- bind_rows(sockeye16weir$values, sockeye17weir$values, sockeye18weir$values)

coho16weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir", this_year = 2016)
coho17weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir", this_year = 2017)
coho18weir <- pvalues_lm_graph(data = data_gathered, this_species = "coho", this_method = "weir", this_year = 2018)
coho_values <- bind_rows(coho16weir$values, coho17weir$values, coho18weir$values)

total16sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar", this_year = 2016)
total17sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar", this_year = 2017)
total18sonar <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "sonar", this_year = 2018)
total16weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir", this_year = 2016)
total17weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir", this_year = 2017)
total18weir <- pvalues_lm_graph(data = data_gathered, this_species = "total", this_method = "weir", this_year = 2018)
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
  mutate(wilcox = replace(wilcox, sort$shapiro >= 0.05, NA), # pvalues normally distributed no need to report Wilcoxon p value
         slope_eq1 = replace(slope_eq1, sort$shapiro < 0.05, NA), # pvalues on not normally distributed data will be off
         adj_r_squared = replace(adj_r_squared, sort$shapiro < 0.05, NA))) # Same with r^2 value.

save(table_values_1060, file = "output/table_values_1060.Rda")
write.csv(table_values_1060,"H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\output\\table_values_1060.csv", row.names = FALSE)


weirsockeyegraphs <- cowplot::plot_grid(sockeye16weir$graph, sockeye17weir$graph, sockeye18weir$graph,  ncol = 1) #, scale = c(1,1,1))
title <- ggdraw() + draw_label("Weir Sockeye", fontfamily = 'Times New Roman')
weirsockeyegraphs <- plot_grid(title, weirsockeyegraphs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

weircohographs <- cowplot::plot_grid(coho16weir$graph, coho17weir$graph, coho18weir$graph,  ncol = 1) #, scale = c(1,1,1))
title <- ggdraw() + draw_label("Weir Coho", fontfamily = 'Times New Roman')
weircohographs <- plot_grid(title, weircohographs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

weirsockeyecohographs <- cowplot::plot_grid(weirsockeyegraphs, weircohographs, ncol = 2)
#weirsockeyecohographs <- plot_grid(title, weirsockeyecohographs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

#Add text to graph
#title <- ggdraw() + draw_label("Comparison of 60 min/hr census vs 10 min/hr estimate.")
# p <- plot_grid(title, weirsockeyecohographs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- plot_grid(weirsockeyecohographs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- add_sub(p, "60 minute per hour census", fontfamily = 'Times New Roman')
#p <- add_sub(p, "___ line linear regression\n--- line y = x with slope = 1.", size = 10)
#p <- p + grid.text("y lable", a = unit(-3, "lines"), rot = 90)
#ggdraw(add_sub(plot, "Label", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

# y label
y.grob <- textGrob("10 minute per hour estimate", 
                   gp=gpar(col="black", fontsize=15, fontfamily = 'Times New Roman'), rot = 90)
#add y label to plot  
p <- grid.arrange(arrangeGrob(p, left = y.grob))
ggsave(paste0("figures/weirsockeyecoho1060graphs.png"), plot = p, dpi = 600, height = 10, width = 7, units ="in")

weirtotalgraphs <- cowplot::plot_grid(total16weir$graph, total17weir$graph, total18weir$graph,  ncol = 1, scale = c(1,1,1))
title <- ggdraw() + draw_label("Weir Total", fontfamily = 'Times New Roman')
weirtotalgraphs <- plot_grid(title, weirtotalgraphs, ncol = 1, rel_heights = c(0.1, 1)) 

sonartotalgraphs <- cowplot::plot_grid(total16sonar$graph, total17sonar$graph, total18sonar$graph, ncol = 1, scale = c(1,1,1))
title <- ggdraw() + draw_label("Sonar Total", fontfamily = 'Times New Roman')
sonartotalgraphs <- plot_grid(title, sonartotalgraphs, ncol = 1, rel_heights = c(0.1, 1)) 

weirsonartotalgraphs <- cowplot::plot_grid(weirtotalgraphs, sonartotalgraphs, ncol = 2)
#Add text to graph
#title <- ggdraw() + draw_label("Comparision of 60 min/hr census vs 10 min/hr estimate.")
#p <- plot_grid(title, weirsonartotalgraphs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- plot_grid(weirsonartotalgraphs, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- add_sub(p, "60 minute per hour census", fontfamily = 'Times New Roman')
#p <- add_sub(p, "___ line linear regression\n--- line y = x with slope = 1.", size = 10)
#p <- p + grid.text("y lable", a = unit(-3, "lines"), rot = 90)
#ggdraw(add_sub(plot, "Label", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

# y label
y.grob <- textGrob("10 minute per hour estimate", gp=gpar(col="black", fontsize=15, fontfamily = 'Times New Roman'), rot=90)
#add y label to plot  
p <- grid.arrange(arrangeGrob(p, left = y.grob))
ggsave(paste0("figures/weirsonartotal1060graphs.png"), plot = p, dpi = 600, height = 10, width = 7, units="in")

#Other Regression Graphs for comparison

# This graph combines all years together to see if there are trends by species
# note no comparisons for sonar individual speciecies.
(fish_grid <- ggplot(data_wide1060 , aes(x = sixty_minute, y = ten_minute)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) + #line y = x for reference
    geom_smooth(method=lm, se=TRUE) +
    theme(panel.grid.major = element_line("lightgray",0.5),
          panel.grid.minor = element_line("lightgray",0.25)) + 
    ggtitle("60 min vs 10 min 2016-2018") +
    facet_grid(method ~ species))

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
    mutate(wilcox = replace(wilcox, sort$shapiro >= 0.05, NA), # pvalues normally distributed no need to report Wilcoxon p value
           slope_eq1 = replace(slope_eq1, sort$shapiro < 0.05, NA), # pvalues on not normally distributed data will be off
           adj_r_squared = replace(adj_r_squared, sort$shapiro < 0.05, NA))) # Same with r^2 value.

save(table_values_ws, file = "output/table_values_ws.Rda")
write.csv(table_values_ws,"H:\\sarah\\Projects\\Kodiak_salmon\\Chignik\\chignik_sonar\\WeirSonar\\output\\table_values_ws.csv", row.names = FALSE)
#cowplots

s <- sockeyeweir60sonar60graphs <- cowplot::plot_grid(sockeye16_60$graph, sockeye17_60$graph, sockeye18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Sockeye", fontfamily = 'Times New Roman')
#legend <- get_legend(sockeye16_60$graph)
s <- plot_grid(title, s, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

c <- cohoweir60sonar60graphs <- cowplot::plot_grid(coho16_60$graph, coho17_60$graph, coho18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Coho", fontfamily = 'Times New Roman')
c <- plot_grid(title, c, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

t <- totalweir60sonar60graphs <- cowplot::plot_grid(total16_60$graph, total17_60$graph, total18_60$graph, scale = c(1,1,1), ncol = 1)
title <- ggdraw() + draw_label("Total", fontfamily = 'Times New Roman')
t <- plot_grid(title, t, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins

#p <- weir60sonar60graphs <- cowplot::plot_grid(sockeyeweir60sonar60graphs, cohoweir60sonar60graphs, totalweir60sonar60graphs, ncol = 3)
p <- weir60sonar60graphs <- cowplot::plot_grid(s, c, t, ncol = 3)

#Add title
##title <- ggdraw() + draw_label("Comparison of weir census vs sonar census.")
#p <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- plot_grid(p, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
p <- add_sub(p, "Weir", fontfamily = 'Times New Roman')
#p <- add_sub(p, "___ line linear regression\n--- line y = x with slope = 1.", size = 10)
#p <- p + grid.text("y lable", a = unit(-3, "lines"), rot = 90)
#ggdraw(add_sub(plot, "Label", vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=4.5))

# y label
y.grob <- textGrob("Sonar", gp=gpar(col="black", fontsize=15, fontfamily = 'Times New Roman'), rot=90)
#add y label to plot  
#https://stackoverflow.com/questions/33114380/centered-x-axis-label-for-muliplot-using-cowplot-package
p <- grid.arrange(arrangeGrob(p, left = y.grob))

ggsave(paste0("figures/weir60sonar60graphs.png"), plot = p, dpi= 600, height= 8, width = 7, units="in")




