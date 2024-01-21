

## ----Load packages--------------------------------------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(lmtest)
library(sandwich)
library(knitr)
library(modelsummary)
library(fixest)
library(dplyr)
library(magrittr)
library(stargazer)
library(stringr)
library(panelr)
library(kableExtra)
library(ggplot2)
library(feasts)
library(weights)


## Read in data--------------------------------------------------------------------------------------------------------------------------------------------------------
# We read in all data from data management
# This will be analysed and condensed to only a few datasets for the Rmd


load("Births_Enrollment_Capacity_Population.RData")
load("final_budget_summary.RData")
load("panel_percapitabudget_province.RData")
load("event_study.RData")
load("Korea_map.RData")



## Panelling GRDP --------------------------------------------------------------------------------------------------------------------------------------------------------
GRDPpanel <- long_panel(GRDPprovince,
                        prefix = "_",
                        begin = 2016,
                        end = 2021,
                        id = "province") %>% 
  rename(year = wave) %>% 
  select(-c(province)) %>% 
  na.omit(GRDPprovince)

GRDPprovince$year %>% as.integer(GRDPprovince$year)

head(GRDPpanel)



## A few last joins --------------------------------------------------------------------------------------------------------------------------------------------------------
# Join by number of newly wed couples

colnames(Population_enrollment_provinces)[1] <- "province"

Population_enrollment_births_provinces <- Population_enrollment_provinces %>% 
  left_join(Births_provinces, by = "province")

joined_enrolled_births_ncouples_provinces <- Population_enrollment_births_provinces %>% 
  left_join(ncouples_province, by = "province")

##Join by budget and other data ####

joined_enrolled_births_ncouples_provinces_exp <- joined_enrolled_births_ncouples_provinces %>% 
  left_join(joined_summary, by = "province")

joinprovince <- joined_enrolled_births_ncouples_provinces_exp %>% 
  left_join(hhinc_province, by = "province")

joined_final <- joinprovince %>% 
  left_join(GRDPprovince, by = "province")

finaljoinprovince2 <- joined_final %>% 
  left_join(cbp_province, by = "province")

finaljoinprovince3 <- finaljoinprovince2 %>% 
  left_join(joined_capacity_provinces, by = "province")

# clean final joined data frame by excluding data from year 2022
# this is because the variables we are trying to control for in our regression does not have data from 2022
final_final_province_join <- finaljoinprovince3 %>% 
  select(-contains(c("2022", "Location", "is_total", "district")))



## ----province-level panel data for all variables---------------------------------------------------------------------------------------------------------


panel_province <- long_panel(final_final_province_join, 
                              prefix = "_", 
                              begin = 2016, 
                              end = 2021,
                              id = "province") %>% 
  rename(year = wave) %>% 
  na.omit(finaljoinprovince3)

panel_province$hhinc <- as.numeric(panel_province$hhinc)
panel_province$total_pub_enrolled <- as.numeric(panel_province$total_pub_enrolled)
panel_province$birthrate <- as.numeric(panel_province$birthrate)
panel_province$nwedcouples <- as.numeric(panel_province$nwedcouples)
panel_province$budget <- as.numeric(panel_province$budget)
panel_province$budget <- panel_province$budget / 10000
panel_province$birthrate <- panel_province$birthrate * 1000

head(panel_province)

## Saving panel data at province level --------------------------------------------------------------------------------------------------------------------------------------------------------


save(panel_province, file = "Panel Data (FINAL).RData")


## Running some plots --------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(panel_province, aes(x = as.integer(str_extract(year, "\\d+")), 
                    y = nwedcouples, color = province)) +
  geom_line() +
  labs(title = "Change in Number of Newly Wed Couples Over Time", 
       x = "Year", y = "Number of Newly Wed Coupples") +
  scale_x_continuous(breaks = 2016:2021)


ggplot(panel_province, aes(x = as.integer(str_extract(year, "\\d+")), 
                    y = hhinc, color = province)) +
  geom_line() +
  labs(title = "Change in Average Household Income Over Time (in 1,000 won)", 
       x = "Year", y = "Average Household Income") +
  scale_x_continuous(breaks = 2016:2021)


## Budget to enrollment (2-3 control variables)--------------------------------------------------------------------------------------------------------------------------------------------------------

reg1 <- lm(total_pub_enrolled ~ budget + nwedcouples + hhinc, data = panel_province)

summary_results1 <- summary(reg1)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC1"))[2,]
r_squared1 <- summary(reg1)$r.squared

knitr::kable(summary_results1$coefficients, caption = "Regression of Budget to Total Number of Children Enrolled")


reg1 <- lm(total_pub_enrolled ~ budget + nwedcouples + hhinc, data = panel_province)

summary_results1 <- summary(reg1)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC1"))[2,]
r_squared1 <- summary(reg1)$r.squared

p_values <- summary_results1$coefficients[,4]
p_values_stars <- ifelse(p_values < 0.001, "***", ifelse(p_values < 0.01, "**", ifelse(p_values < 0.05, "*", "")))
summary_results1$coefficients[,4] <- paste0(format(summary_results1$coefficients[,4], scientific = FALSE), p_values_stars)

knitr::kable(summary_results1$coefficients, caption = "Regression of Budget to Total Number of Children Enrolled")
cat(paste0("R-squared: ", r_squared1))


## Enrollment to fertility rate (two control variables) --------------------------------------------------------------------------------------------------------------------------------------------------------


reg2 <- lm(birthrate ~ total_pub_enrolled + nwedcouples + hhinc, data = panel_province)

summary_results2 <- summary(reg2)
coeftest(reg2, vcov = vcovHC(reg2, type = "HC1"))[2,]
r_squared2 <- summary(reg2)$r.squared

p_values <- summary_results2$coefficients[,4]
p_values_stars <- ifelse(p_values < 0.001, "***", ifelse(p_values < 0.01, "**", ifelse(p_values < 0.05, "*", "")))
summary_results2$coefficients[,4] <- paste0(format(summary_results1$coefficients[,4], scientific = FALSE), p_values_stars)

knitr::kable(summary_results1$coefficients, caption = "Regression of Total Number of Children Enrolled to Fertility Rate", stars=T)
cat("R-squared: ", r_squared2)


model3 <- list(
  "Budget on Enrollment" = feols(total_pub_enrolled ~ budget + nwedcouples + hhinc + GRDP | factor(year), 
                data = panel_province,
                vcov = "hetero"),
  "Enrollment on Birthrate" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc + GRDP | factor(year), 
                data = panel_province,
                vcov = "hetero")
  )

modelsummary(model3,
            coef_omit = "Intercept", stars = T,
             title = 'Regression Table with Year fixed effects')


## FE Model Trials--------------------------------------------------------------------------------------------------------------------------------------------------------
#load panel data

load("Panel Data (FINAL).RData")

#store models for display using modelsummary()
models <- list(
  "FE 1" = feols(total_pub_enrolled ~ budget
                 | province,
                 data = panel_province, weight = panel_province$Population),
  "FE 2" = feols(total_pub_enrolled ~ budget + nwedcouples + hhinc 
                 | province,
                 data = panel_province, weight = panel_province$Population),
  "FE 3" = feols(total_pub_enrolled ~ budget + nwedcouples + hhinc 
                 | year,
                 data = panel_province, weight = panel_province$Population),
  "FE 4" = feols(total_pub_enrolled ~ budget + nwedcouples + hhinc
                 | province + year,
                 data = panel_province, weight = panel_province$Population)
)

#display results results without much customization
modelsummary(models,
             coef_omit = "Intercept",
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik.',
             stars = c('*' = .1, '**' = .05, '***' = .01))

#set up an object to customize your regression table goodness-of-fit statistics
gm <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",               "N",                  0,
    "r.squared",          "R<sup>2</sup>",      2,
    "adj.r.squared",      "Adj. R<sup>2</sup>", 2,
    "FE: province",         "Province FEs",          0,
    "FE: year",    "Year FEs",    0,
    )

#display results with added formatting options    
modelsummary(models,
             coef_omit = 'Intercept',
             coef_rename = c("budget" = "Family Budget", 
                             "nwedcouples" = "Newly Married Couples per 1,000",
                             "hhinc" = "Average Household Income"),
             gof_map = gm,
             title = 'Effect of Family Budget on Public Childcare Facility Enrollment',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = 'Robust standard errors clustered by province are shown in parentheses.
                      Observations are weighted by the population in each province.')

save(models, file = "FE Budget on Public Enrollment.RData")


library(tibble)

gm <- tibble::tribble(
  ~raw,            ~clean,              ~fmt,
  "nobs",           "N",                 0,
  "r.squared",      "R^2",               2,
  "adj.r.squared",  "Adj. R^2",          2,
  "FE: province",   "Province FEs",      0,
  "FE: year",       "Year FEs",          0,
)

# Assuming models is your regression model object
modelsummary(models,
             coef_omit = 'Intercept',
             coef_rename = c("budget" = "Family Budget", 
                              "nwedcouples" = "Newly Married Couples per 1,000",
                              "hhinc" = "Average Household Income"),
             gof_map = gm,
             title = 'Effect of Family Budget on Public Childcare Facility Enrollment',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = 'Robust standard errors clustered by province are shown in parentheses.
                      Observations are weighted by the population in each province.')



models2 <- list(
  "FE 1" = feols(birthrate ~ total_pub_enrolled
                 | province,
                 data = panel_province, weight = panel_province$Population),
  "FE 2" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc 
                 | province,
                 data = panel_province, weight = panel_province$Population),
  "FE 3" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc 
                 | year,
                 data = panel_province, weight = panel_province$Population),
  "FE 4" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc
                 | province + year,
                 data = panel_province, weight = panel_province$Population)
)

#display results results without much customization
modelsummary(models2,
             coef_omit = "Intercept",
             gof_omit = 'DF|Deviance|R2 Pseudo|AIC|BIC|Log.Lik.',
             stars = c('*' = .1, '**' = .05, '***' = .01))

#set up an object to customize your regression table goodness-of-fit statistics
gm2 <- tibble::tribble(
    ~raw,        ~clean,          ~fmt,
    "nobs",               "N",                  0,
    "r.squared",          "R^2",      2,
    "adj.r.squared",      "Adj. R<sup>2</sup>", 2,
    "FE: province",         "Province FEs",          0,
    "FE: year",    "Year FEs",    0,
    )

#display results with added formatting options    
modelsummary(models2,
             coef_omit = 'Intercept',
             coef_rename = c("total_pub_enrolled" = "N of Children Enrolled in Public Chidcare Facilities", 
                             "nwedcouples" = "Newly Married Couples per 1000",
                             "hhinc" = "Average Household Income"),
             gof_map = gm2,
             title = 'Effect of Public Childcare Facility Enrollment on Birth Rate',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = 'Robust standard errors clustered by province are shown in parentheses.
                      Observations are weighted by the population in each province.')

save(models2, file = "FE Public Enrollment on Birth Rate.RData")



## FINAL MODELS #### 

#store models for display using modelsummary()
newmodels <- list(
  "FE 1" = feols(total_pub_capacity ~ budget 
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 2" = feols(total_pub_capacity ~ budget + nwedcouples
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 3" = feols(total_pub_capacity ~ budget + nwedcouples + hhinc 
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 4" = feols(total_pub_capacity ~ budget + nwedcouples + hhinc + GRDP 
                 | year, data = panel_province, weights = panel_province$Population)
)

#set up an object to customize your regression table goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,            ~clean,              ~fmt,
  "nobs",           "N",                 0,
  "r.squared",      "R^2",               2,
  "adj.r.squared",  "Adj. R^2",          2,
  "FE: year",       "Year FEs",          0,
)

#display results with added formatting options    
modelsummary(newmodels,
             coef_omit = 'Intercept',
             coef_rename = c("budget" = "Family Budget per capita (10k KRW)", 
                             "nwedcouples" = "Newly Married Couples per capita",
                             "hhinc" = "Average Household Income per capita (10k KRW)",
                             "GRDP" = "GRDP per capita"
             ),
             gof_map = gm,
             title = 'Year FE Family Budget on Public Childcare Facilities Enrollment Capacity',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = 'Robust standard errors clustered by province are shown in parentheses.
                      Observations are weighted by the population in each province.')

save(newmodels, file = "Year FE Family Budget on Enrollment Capacity.RData")



newmodels2 <- list(
  "FE 1" = feols(birthrate ~ total_pub_enrolled
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 2" = feols(birthrate ~ total_pub_enrolled + nwedcouples
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 3" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc 
                 | year, data = panel_province, weights = panel_province$Population),
  "FE 4" = feols(birthrate ~ total_pub_enrolled + nwedcouples + hhinc + GRDP 
                 | year, data =  panel_province, weights = panel_province$Population)
)

#display results with added formatting options    
modelsummary(newmodels2,
             coef_omit = 'Intercept',
             coef_rename = c("budget" = "Family Budget per capita", 
                             "total_pub_enrolled" = "Number of Children Enrolled in Public Childcare Facilities per capita",
                             "nwedcouples" = "Newly Married Couples per capita",
                             "hhinc" = "Average Household Income per capita",
                             "GRDP" = "GRDP per capita"
             ),
             gof_map = gm,
             title = 'Year FE Enrollment on Birthrate',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             notes = 'Robust standard errors clustered by province are shown in parentheses.
                      Observations are weighted by the population in each province.')


## Saving for final Rmd #### 

save(newmodels2, file = "Year FE Enrollment on Birthrate.RData")
save(newmodels, newmodels2, panel_province, df_long_clustered_bar, 
     panel_birth_enrollment_province, 
     panel_percapita_budget_province,
     capitalTS,
     Population_enrollment_rate_provinces_panel,
     korea_1_df,
     file = "Final Dataset.RData")