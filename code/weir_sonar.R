# load ----
library(tidyverse)
library(lubridate)
library(broom)
library(FNGr)
theme_set(theme_sleek())
options(scipen=999)

# data ----
read_csv('data/chigWeirDidson201618sjp.csv') %>% 
  dplyr::select(-starts_with("prop")) %>%
  gather(species, abundance, sockeye_10:total_60) %>%
  separate(species, c("species", "period"), sep ="_") %>%
  mutate(date = mdy(date),
         year = year(date),
         Year = factor(year),
         abundance = as.numeric(str_replace(abundance, "-", "0")),
         period = case_when(period=="10" ~ "ten_minute",
                            TRUE ~ "sixty_minute"),
         method = ifelse(method=="sonar" & species!="total", NA, method)) %>% 
  spread (period, abundance) %>% 
  mutate(ln_sixty = log(sixty_minute + 1), ln_ten = log(ten_minute + 1)) %>% 
  filter(species %in% c("sockeye", "coho", "total")) %>% 
  drop_na() -> data

# functions ----
# slopes = 1? 
slope_eq_1 <- function(lm){
  slope = exp(coef(summary(lm))[2,1])
  se_slope = exp(coef(summary(lm))[2,2])
  df = summary(lm)$df[2]
  
  pt(-abs((slope - 1) / se_slope), df = df) * 2
}

f_fig <- function(data, YEAR){
  
  data %>% 
    group_split(method, species, year) %>%
    map_df(~{fit = lm(ln_sixty ~ ln_ten, data = .)
    
    data.frame(., ci = predict(fit, ., interval = 'confidence'),
               pi = predict(fit, ., interval = 'prediction'))
    }) %>% 
    filter(year==YEAR) %>% 
    # left_join(., data) %>% 
    ggplot(aes(ten_minute, ci.fit)) + 
    geom_point(aes(y = sixty_minute), alpha = 0.5) +
    facet_wrap(species~method, dir = "v", ncol = 3, scales = "free") +
    geom_ribbon(aes(ymin = exp(ci.lwr), ymax = exp(ci.upr)), alpha = 0.4) +
    geom_ribbon(aes(ymin = exp(pi.lwr), ymax = exp(pi.upr)), alpha = 0.2) +
    geom_abline(slope = 1, lty = 3) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    ylab("Model fit") +
    xlab("Ten minute estimate")
}

# fit models ----
data %>% 
  group_by(species, year, method) %>% 
  nest() %>% 
  mutate(fit = purrr::map(data, ~lm(ln_sixty ~ ln_ten, data = .)),
         slopes = purrr::map(fit, ~ slope_eq_1(.), data = .),
         shapiro = purrr::map(fit, ~shapiro.test(.$residuals))) -> models

# model summaries ----
models %>%
  unnest(fit %>% map(glance)) %>% 
  dplyr::select(-data, -fit, -slopes, -shapiro) -> model_out

models %>% 
  unnest(slopes) %>% 
  dplyr::select(species, year, method, slopes) -> slope_out

models %>%
  unnest(shapiro %>% map(glance)) %>% 
  dplyr::select(species, year, method, shapiro = p.value)  -> shapiro_out

# wilcox tests ----
# needs a different data structure from previous analysis

data %>% 
  gather(period, abundance, 
         -c(date, method, species, year, Year, ln_sixty, ln_ten)) %>% 
  group_by(species, method, year) %>% 
  nest() %>% 
  mutate(wilcox = map(data, ~ wilcox.test(abundance ~ period, 
                                          data = .,
                                          paired = TRUE, 
                                          alternative = "two.sided"))) %>% 
  unnest(wilcox %>% map(tidy)) %>% 
  dplyr::select(species, method, year, wilcox = p.value) -> wilcox_out


# figures ---

f_fig(data, 2016)
f_fig(data, 2017)
f_fig(data, 2018) + ggtitle("2018")

# table ----
model_out %>% 
  left_join(slope_out) %>% 
  left_join(shapiro_out) %>% 
  left_join(wilcox_out) %>% 
  dplyr::select(-c(r.squared, sigma, statistic, df, deviance, df.residual))

