#loading the libraries
library(tidyverse)
library(readxl)
library(gapminder)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(stringr)
library(panelr)
getwd()

## 2016 Cleaning ########

#reading each year's budget data from excel (translated from Korean)
budget_2016 <- read_excel('Data/Budget/2016_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2016_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

#checking for NAs
sum(is.na(budget_2016))

#creating a dummy column for province totals so that those rows can be filtered out
budget_2016 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2016$is_total <- ifelse(grepl("Total", budget_2016$Location, ignore.case = TRUE), 1, 0)

#making the budget column numeric from character and correcting the spelling typo for 2 rows
budget_2016$'2016_child_woman_family_budget' <- as.numeric(budget_2016$'2016_child_woman_family_budget')

#creating separate province and district colums for easy grouping and encoding province as a factor
budget_2016 <- budget_2016 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))
#saving as a clean data for future use
budget_2016_clean <- budget_2016 
budget_2016_clean$is_total <- NULL

#saving the summary for the year's budget data
summary_2016 <-budget_2016_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2016_child_woman_family_budget`),
            median_budget = median(`2016_child_woman_family_budget`),
            max_budget = max(`2016_child_woman_family_budget`),
            min_budget = min(`2016_child_woman_family_budget`)) 

#the above steps have been repeated for each year from 2016-2022 below, jump to line 365 for joins
## 2017 Cleaning ########

budget_2017 <- read_excel('Data/Budget/2017_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2017_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2017))

budget_2017 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2017$is_total <- ifelse(grepl("Total", budget_2017$Location, ignore.case = TRUE), 1, 0)

budget_2017$'2017_child_woman_family_budget' <- as.numeric(budget_2017$'2017_child_woman_family_budget')

budget_2017 <- budget_2017 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2017_clean <- budget_2017 
budget_2017_clean$is_total <- NULL

summary_2017 <-budget_2017_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2017_child_woman_family_budget`),
            median_budget = median(`2017_child_woman_family_budget`),
            max_budget = max(`2017_child_woman_family_budget`),
            min_budget = min(`2017_child_woman_family_budget`)) 

## 2018 Cleaning ########

budget_2018 <- read_excel('Data/Budget/2018_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2018_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2018))

budget_2018 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2018$is_total <- ifelse(grepl("Total", budget_2018$Location, ignore.case = TRUE), 1, 0)

budget_2018$'2018_child_woman_family_budget' <- as.numeric(budget_2018$'2018_child_woman_family_budget')

budget_2018 <- budget_2018 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2018_clean <- budget_2018 
budget_2018_clean$is_total <- NULL

summary_2018 <-budget_2018_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2018_child_woman_family_budget`),
            median_budget = median(`2018_child_woman_family_budget`),
            max_budget = max(`2018_child_woman_family_budget`),
            min_budget = min(`2018_child_woman_family_budget`)) 

## 2019 Cleaning ########

budget_2019 <- read_excel('Data/Budget/2019_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2019_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2019))

budget_2019 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2019$is_total <- ifelse(grepl("Total", budget_2019$Location, ignore.case = TRUE), 1, 0)

budget_2019$'2019_child_woman_family_budget' <- as.numeric(budget_2019$'2019_child_woman_family_budget')

budget_2019 <- budget_2019 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2019_clean <- budget_2019 
budget_2019_clean$is_total <- NULL

summary_2019 <-budget_2019_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2019_child_woman_family_budget`),
            median_budget = median(`2019_child_woman_family_budget`),
            max_budget = max(`2019_child_woman_family_budget`),
            min_budget = min(`2019_child_woman_family_budget`)) 

## 2020 Cleaning ########

budget_2020 <- read_excel('Data/Budget/2020_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2020_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2020))

budget_2020 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2020$is_total <- ifelse(grepl("Total", budget_2020$Location, ignore.case = TRUE), 1, 0)

budget_2020$'2020_child_woman_family_budget' <- as.numeric(budget_2020$'2020_child_woman_family_budget')

budget_2020 <- budget_2020 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2020_clean <- budget_2020 
budget_2020_clean$is_total <- NULL

summary_2020 <-budget_2020_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2020_child_woman_family_budget`),
            median_budget = median(`2020_child_woman_family_budget`),
            max_budget = max(`2020_child_woman_family_budget`),
            min_budget = min(`2020_child_woman_family_budget`)) 

## 2021 Cleaning ########

budget_2021 <- read_excel('Data/Budget/2021_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2021_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2021))

budget_2021 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2021$is_total <- ifelse(grepl("Total", budget_2021$Location, ignore.case = TRUE), 1, 0)

budget_2021$'2021_child_woman_family_budget' <- as.numeric(budget_2021$'2021_child_woman_family_budget')

budget_2021 <- budget_2021 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2021_clean <- budget_2021 
budget_2021_clean$is_total <- NULL

summary_2021 <-budget_2021_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2021_child_woman_family_budget`),
            median_budget = median(`2021_child_woman_family_budget`),
            max_budget = max(`2021_child_woman_family_budget`),
            min_budget = min(`2021_child_woman_family_budget`)) 

## 2022 Cleaning ########

budget_2022 <- read_excel('Data/Budget/2022_budget_expenditure_by_sector.xlsx') %>%
  select(Location, `080 Social Welfare`) %>%
  rename(`2022_child_woman_family_budget`= `080 Social Welfare`) %>%
  filter(Location != "Local government", Location != "National Total")

sum(is.na(budget_2022))

budget_2022 %>%
  filter(str_detect(Location, 'Total')) %>%
  summarize(count = n())
budget_2022$is_total <- ifelse(grepl("Total", budget_2022$Location, ignore.case = TRUE), 1, 0)

budget_2022$'2022_child_woman_family_budget' <- as.numeric(budget_2022$'2022_child_woman_family_budget')

budget_2022 <- budget_2022 %>%
  filter(is_total == 0) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
  mutate(district = if_else(str_detect(Location, 'Main'), 
                            "Main Office",
                            word(Location,-1))) %>%
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", "Incheon",
                                                "Gwangju","Daejeon", "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon","North Chungcheong",
                                                "South Chungcheong","North Jeolla","South Jeolla",
                                                "North Gyeongsang","North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla","South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))

budget_2022_clean <- budget_2022 
budget_2022_clean$is_total <- NULL

summary_2022 <-budget_2022_clean %>%
  group_by(province) %>%
  summarise(district_count = n_distinct(district),
            total_budget = sum(`2022_child_woman_family_budget`),
            median_budget = median(`2022_child_woman_family_budget`),
            max_budget = max(`2022_child_woman_family_budget`),
            min_budget = min(`2022_child_woman_family_budget`))


## Joining each budget########

#joining the budget data from each year into a master source
joined_budget <- budget_2016_clean %>%
  left_join(budget_2017_clean, by = "Location") %>%
  left_join(budget_2018_clean, by = "Location") %>%
  left_join(budget_2019_clean, by = "Location") %>%
  left_join(budget_2020_clean, by = "Location") %>%
  left_join(budget_2021_clean, by = "Location") %>%
  left_join(budget_2022_clean, by = "Location") %>%
  select(Location,province.x, district.x,
         `2016_child_woman_family_budget`, `2017_child_woman_family_budget`,
         `2018_child_woman_family_budget`, `2019_child_woman_family_budget`,
         `2020_child_woman_family_budget`, `2021_child_woman_family_budget`,
         `2022_child_woman_family_budget`) %>%
  rename(province = province.x) %>%
  rename(district = district.x)

#joining the summary data from each year into a master source
temp <- list(summary_2016, summary_2017, summary_2018, summary_2019, summary_2020, summary_2021, summary_2022)
joined_summary <- reduce(temp, left_join, by = "province") %>%
  select(province, district_count, starts_with("total")) %>%
  rename(budget_2016 = total_budget.x) %>%
  rename(budget_2017 = total_budget.y) %>%
  rename(budget_2018 = total_budget.x.x) %>%
  rename(budget_2019 = total_budget.y.y) %>%
  rename(budget_2020 = total_budget.x.x.x) %>%
  rename(budget_2021 = total_budget.y.y.y) %>%
  rename(budget_2022 = total_budget) 
rm(temp)

#chart showing change in budget across provinces over time
df_long <- pivot_longer(joined_summary, cols = -province, names_to = "year", values_to = "budget")
ggplot(df_long, aes(x = as.integer(str_extract(year, "\\d+")), y = budget, color = province)) +
  geom_line() +
  labs(title = "Change in Budget Over Time", x = "Year", y = "Budget") +
  scale_x_continuous(breaks = 2016:2022)

## Reading in population ########
# Population was used in both data management files for cleaning and weighting


#loading Population numbers
Population_2016_22 <- read_excel('Data/Population/Korea Population by Locality_2016_2022.xlsx')

#renaming column names
colnames(Population_2016_22)[1] <- "Location"
colnames(Population_2016_22)[2] <- "Population_2016"
colnames(Population_2016_22)[3] <- "Population_2017"
colnames(Population_2016_22)[4] <- "Population_2018"
colnames(Population_2016_22)[5] <- "Population_2019"
colnames(Population_2016_22)[6] <- "Population_2020"
colnames(Population_2016_22)[7] <- "Population_2021"
colnames(Population_2016_22)[8] <- "Population_2022"

#data prepping
Population_2016_22$is_total <- ifelse(grepl
                                      ("Total", 
                                        Population_2016_22$Location, 
                                        ignore.case = TRUE), 1, 0)

Population_2016_22_cleaned <- Population_2016_22 

Population_2016_22_cleaned$`Population_2016` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2016`))

Population_2016_22_cleaned$`Population_2017` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2017`))

Population_2016_22_cleaned$`Population_2018` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2018`))

Population_2016_22_cleaned$`Population_2019` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2019`))

Population_2016_22_cleaned$`Population_2020` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2020`))

Population_2016_22_cleaned$`Population_2021` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2021`))

Population_2016_22_cleaned$`Population_2022` <- as.numeric(
  gsub(",","",Population_2016_22_cleaned$`Population_2022`))

Population_2016_22_cleaned_provinces <- Population_2016_22_cleaned %>%
  filter(is_total == 1) %>%
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))

Population_budget_province <- joined_summary %>% 
  left_join(Population_2016_22_cleaned_provinces, by = "province") %>% 
  select (-is_total) %>%
  select (-district_count) %>%
  select (-Location)

Percapita_budget_province <- Population_budget_province %>%
  mutate(percapita_budget_2016 = budget_2016*1000/(0.13*Population_2016)) %>%
  mutate(percapita_budget_2017 = budget_2017*1000/(0.13*Population_2017)) %>%
  mutate(percapita_budget_2018 = budget_2018*1000/(0.13*Population_2018)) %>%
  mutate(percapita_budget_2019 = budget_2019*1000/(0.13*Population_2019)) %>%
  mutate(percapita_budget_2020 = budget_2020*1000/(0.13*Population_2020)) %>%
  mutate(percapita_budget_2021 = budget_2021*1000/(0.13*Population_2021)) %>%
  mutate(percapita_budget_2022 = budget_2022*1000/(0.13*Population_2022)) %>%
  select(province, budget_2016, percapita_budget_2016, budget_2017, percapita_budget_2017, 
         budget_2018, percapita_budget_2018, budget_2019, percapita_budget_2019, budget_2020,
         percapita_budget_2020, budget_2021, percapita_budget_2021, budget_2022, 
         percapita_budget_2022)

panel_percapita_budget_province <- long_panel(Percapita_budget_province, 
                                              prefix = "_", 
                                              begin = 2016, 
                                              end = 2022,
                                              id = "province") %>% 
  rename(year = wave)

panel_population_budget_province <- long_panel(Population_budget_province, 
                                               prefix = "_", 
                                               begin = 2016, 
                                               end = 2022,
                                               id = "province") %>% 
  rename(year = wave)

## Saving budget data ########
save(joined_budget, file = "budget_data.RData")
save(joined_summary, file = "budget_summary.RData")
save(panel_percapita_budget_province, file = "panel_percapitabudget_province.RData")
save(panel_population_budget_province, file = "panel_popbudget_province.RData")


## Summary stats and clean ########

#summary statistics yearly
summary(joined_budget[, c("2016_child_woman_family_budget", "2017_child_woman_family_budget", 
                          "2018_child_woman_family_budget", "2019_child_woman_family_budget", 
                          "2020_child_woman_family_budget", "2021_child_woman_family_budget", 
                          "2022_child_woman_family_budget")])

#cross table district province mapping
table(joined_budget$district, joined_budget$province)

#yearly totals
colSums(joined_budget[, c("2016_child_woman_family_budget", "2017_child_woman_family_budget", 
                          "2018_child_woman_family_budget", "2019_child_woman_family_budget", 
                          "2020_child_woman_family_budget", "2021_child_woman_family_budget", 
                          "2022_child_woman_family_budget")], na.rm = TRUE)

#create a dummy variable for capital/non-capital region and rename year-budget
joined_budget <- joined_budget %>%
  mutate(capital_region = if_else(province == "Seoul"|province == "Incheon"|province == "Gyeonggi",
                                  "Capital",
                                  "Non-Capital")) %>%
  mutate(capital_region = factor(capital_region, levels = c("Capital","Non-Capital"), 
                                 labels = c("Yes","No"))) %>%
  rename(`budget_2016`= `2016_child_woman_family_budget`) %>%
  rename(`budget_2017`= `2017_child_woman_family_budget`) %>%
  rename(`budget_2018`= `2018_child_woman_family_budget`) %>%
  rename(`budget_2019`= `2019_child_woman_family_budget`) %>%
  rename(`budget_2020`= `2020_child_woman_family_budget`) %>%
  rename(`budget_2021`= `2021_child_woman_family_budget`) %>%
  rename(`budget_2022`= `2022_child_woman_family_budget`) 

long_joined_budget <- long_panel(joined_budget, 
                                 prefix = "_", 
                                 begin = 2016, 
                                 end = 2022,
                                 id = "Location") %>% 
  rename(year = wave) 

#summary stats by capital region classification
summary_stats <- long_joined_budget %>%
  group_by(province, year) %>%
  summarise(
    mean_budget = mean(budget),
    total_budget = sum(budget),
    district_count = n_distinct(district)
  )
print(summary_stats)

## Plotting budgets ########

# Stacked bar chart for total budget by province
ggplot(summary_stats, aes(x = year, y = total_budget, fill = province)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Budget Trend by Province",
       x = "Year",
       y = "Total Budget")

#per capita
ggplot(panel_percapita_budget_province, aes(x = year, y = percapita_budget, fill = province)) +
  geom_bar(stat = "identity") +
  labs(title = " Budget Trend by Province",
       x = "Year",
       y = "Budget per 1000 child bearing population")

# Stacked area chart for total budget over time
ggplot(long_joined_budget, aes(x = year, y = budget, fill = province)) +
  geom_area() +
  labs(title = "Total Budget Over Time",
       x = "Year",
       y = "Total Budget")

# Boxplot for budget distribution by province
ggplot(long_joined_budget, aes(x = province, y = budget, fill = capital_region)) +
  geom_boxplot() +
  labs(title = "Budget Distribution by Province",
       x = "Province",
       y = "Budget") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#generating % change in budgets for capital/non-capital regions

## Capital and non-capital data ########

# Step 1: Subset the Data
capital_data <- long_joined_budget[long_joined_budget$capital_region == "Yes", ]
non_capital_data <- long_joined_budget[long_joined_budget$capital_region == "No", ]

# Step 2: Aggregate the Data
capital_sum <- aggregate(budget ~ year, data = capital_data, sum)
non_capital_sum <- aggregate(budget ~ year, data = non_capital_data, sum)

# Step 3: Calculate Percentage Change
capital_sum$percentage_change <- c(0, diff(capital_sum$budget) / head(capital_sum$budget, -1) * 100)
non_capital_sum$percentage_change <- c(0, diff(non_capital_sum$budget) / head(non_capital_sum$budget, -1) * 100)

# Step 4: Create Matrix
percentage_change_matrix <- matrix(
  c(capital_sum$percentage_change, non_capital_sum$percentage_change),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Capital", "Non-Capital"), as.character(capital_sum$year)))

title <- "Percentage Change in Budget for Capital vs Non-Capital Regions (2016-2022)"
cat(paste("\n", title, "\n"), sep = "")
print(percentage_change_matrix, quote = FALSE, digits = 2)

#repeating steps for finding cumulative change
#Calculate Total Percentage Change
total_percentage_change_capital <- (capital_sum$budget[7] - capital_sum$budget[1]) / capital_sum$budget[1] * 100
total_percentage_change_non_capital <- (non_capital_sum$budget[7] - non_capital_sum$budget[1]) / non_capital_sum$budget[1] * 100

#Create Matrix
total_percentage_change_matrix <- matrix(
  c(total_percentage_change_capital, total_percentage_change_non_capital),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("Capital", "Non-Capital"), c("2016-2022")))

title <- "Total Percentage Change in Budget for Capital vs Non-Capital Regions (2016-2022)"
cat(paste("\n", title, "\n"), sep = "")
print(total_percentage_change_matrix, quote = FALSE, digits = 2)

#province level totals for top 5 provinces by budget
top_n_provinces <- 5
excluded_column <- 'district_count'

province_totals <- joined_summary %>%
  group_by(province) %>%
  summarize(total_budget = sum(across(starts_with("budget_")), na.rm = TRUE)) %>%
  arrange(desc(total_budget)) %>%
  head(top_n_provinces)

filtered_data <- joined_summary %>%
  filter(province %in% province_totals$province) %>%
  select(-matches(excluded_column))

melted_data <- melt(filtered_data, id.vars = "province")
ggplot(melted_data, aes(x = variable, y = value, fill = province)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Province Budget Totals (Top 5 Provinces)",
       x = "Year",
       y = "Total Budget") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#same can be done for bottom 5 provinces
bottom_n_provinces <- 5
excluded_column <- 'district_count'

province_totals <- joined_summary %>%
  group_by(province) %>%
  summarize(total_budget = sum(across(starts_with("budget_")), na.rm = TRUE)) %>%
  arrange(total_budget) %>%
  head(bottom_n_provinces)

filtered_data <- joined_summary %>%
  filter(province %in% province_totals$province) %>%
  select(-matches(excluded_column))

melted_data <- melt(filtered_data, id.vars = "province")
ggplot(melted_data, aes(x = variable, y = value, fill = province)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Province Budget Totals (Bottom 5 Provinces)",
       x = "Year",
       y = "Total Budget") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#overall heatmap? not sure how we want to use this though
heatmap(table(joined_budget$district, joined_budget$province))

#selected province-year heatmap, can play around to see how it changes over years/provinces
selected_province <- 'Busan'
province_data_2018 <- joined_budget %>%
  filter(province == selected_province) %>%
  select(district, province, `budget_2018`)
province_data_2018 <- province_data_2018 %>%
  mutate(budget_share = `budget_2018` / sum(`budget_2018`) * 100)

ggplot(province_data_2018, aes(x = district, y = province, fill = budget_share)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = paste("Budget Share Heatmap for", selected_province, "in 2018"),
       x = "District",
       y = "Province",
       fill = "Budget Share (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#adding capital/non flag to population panel data

panel_population_budget_province <- panel_population_budget_province %>%
  mutate(capital_region = if_else(province == "Seoul"|province == "Incheon"|province == "Gyeonggi",
                                  "Capital",
                                  "Non-Capital")) %>%
  mutate(capital_region = factor(capital_region, levels = c("Capital","Non-Capital"), 
                                 labels = c("Yes","No"))) 


#year since law was passed chart from Medicaid code (district level code)
test <- long_joined_budget %>%
  mutate(t_law = year - 2018)

aggTS <- test %>%
  group_by(year) %>%
  summarize(budget = mean(
    budget, na.rm = T), .groups = 'drop') %>%
  mutate(grp = "All Districts")

capitalTS <- test %>%
  group_by(year, capital_region) %>%
  summarize(budget = mean(
    budget, na.rm = T), .groups = 'drop') %>%
  mutate(grp = ifelse(
    capital_region == "Yes", "Capital Region", "Non-Capital Region")) %>%
  bind_rows(aggTS)

ggplot(capitalTS, aes(x = year, y = budget, group = grp)) +
  geom_line(aes(linetype = grp)) +
  geom_point(aes(shape = grp)) +
  geom_vline(aes(xintercept = 2018)) +
  labs(
    x = "Year",
    y = "Avg Budget, District Level") +
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(20, 15, 0)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="black"))


#saving for future use
save(long_joined_budget, file = "budget_data_final.RData")
save(summary_stats, joined_summary, file = "final_budget_summary.RData")


#year since law was passed chart from Medicaid code (province level code)
test <- panel_population_budget_province %>%
  mutate(t_law = year - 2018) %>%
  mutate(percapita = budget*1000/(0.13*Population))

aggTS <- test %>%
  group_by(year) %>%
  summarize(budget = mean(
    percapita, na.rm = T), .groups = 'drop') %>%
  mutate(grp = "All Provinces")

capitalTS <- test %>%
  group_by(year, capital_region) %>%
  summarize(budget = mean(
    percapita, na.rm = T), .groups = 'drop') %>%
  mutate(grp = ifelse(
    capital_region == "Yes", "Capital Region", "Non-Capital Region")) %>%
  bind_rows(aggTS)

save(capitalTS, file = "event_study.RData")

ggplot(capitalTS, aes(x = year, y = budget, group = grp)) +
  geom_line(aes(linetype = grp)) +
  geom_point(aes(shape = grp)) +
  geom_vline(aes(xintercept = 2018)) +
  labs(
    x = "Year",
    y = "Budget per 1000 child bearing population, Province Level") +
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  scale_shape_manual(values = c(20, 15, 0)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8,0.6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size=.1, color="black"))


