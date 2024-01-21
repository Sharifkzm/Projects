
library(tidyverse)
library(readxl)
library(openxlsx)
library(data.table)
library(gapminder)
library(dplyr)
library(purrr)
library(tidyr)
library(gapminder)
library(panelr)
library(plm)
library(fixest)
library(lmtest) 
library(multiwayvcov)
library(broom)
library(stargazer)
library(modelsummary)
library(sf)
library(ggplot2)
library(tmap)

## ----ENROLLMENT----------

Enrolled_2016 <- read.csv('Data/Enrollment/Childcare dataset_new_2016 Enrolled.csv')
Enrolled_2017 <- read.csv('Data/Enrollment/Childcare dataset_new_2017 Enrolled.csv')
Enrolled_2018 <- read.csv('Data/Enrollment/Childcare dataset_new_2018 Enrolled.csv')
Enrolled_2019 <- read.csv('Data/Enrollment/Childcare dataset_new_2019 Enrolled.csv')
Enrolled_2020 <- read.csv('Data/Enrollment/Childcare dataset_new_2020 Enrolled.csv')
Enrolled_2021 <- read.csv('Data/Enrollment/Childcare dataset_new_2021 Enrolled.csv')
Enrolled_2022 <- read.csv('Data/Enrollment/Childcare dataset_new_2022 Enrolled.csv')

# A lot of cleaning is then done 



## ----cleaning-enrollment-2016-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2016_cleaned <- subset(Enrolled_2016, select = -1) 

# Let's bring in column names from the top row
Enrolled_2016_cleaned <- tail(Enrolled_2016_cleaned, -3)
colnames(Enrolled_2016_cleaned)[1] <- "Location"
Enrolled_2016_cleaned <- Enrolled_2016_cleaned %>% 
    filter(X.1 == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2016_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2016_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2016_cleaned_districts <- Enrolled_2016_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2016_cleaned_districts$`Publicenroll_2016` <-
  as.numeric(gsub(",","",Enrolled_2016_cleaned_districts$`Public.childcare.center`))

Enrolled_2016_cleaned_districts$`2016 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2016_cleaned_districts$`Total`))

Enrolled_2016_cleaned_districts <- Enrolled_2016_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2017-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2017_cleaned <- subset(Enrolled_2017, select = -1) 

# Let's bring in column names from the top row
Enrolled_2017_cleaned <- tail(Enrolled_2017_cleaned, -3)
colnames(Enrolled_2017_cleaned)[1] <- "Location"
Enrolled_2017_cleaned <- Enrolled_2017_cleaned %>% 
    filter(X.1 == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2017_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2017_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2017_cleaned_districts <- Enrolled_2017_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2017_cleaned_districts$`Publicenroll_2017` <-
  as.numeric(gsub(",","",Enrolled_2017_cleaned_districts$`Public.childcare.center`))

Enrolled_2017_cleaned_districts$`2017 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2017_cleaned_districts$`Total`))

Enrolled_2017_cleaned_districts <- Enrolled_2017_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2018-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2018_cleaned <- subset(Enrolled_2018, select = -1) 

# Let's bring in column names from the top row
Enrolled_2018_cleaned <- tail(Enrolled_2018_cleaned, -3)
colnames(Enrolled_2018_cleaned)[1] <- "Location"
Enrolled_2018_cleaned <- Enrolled_2018_cleaned %>% 
    filter(X.1 == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2018_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2018_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2018_cleaned_districts <- Enrolled_2018_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2018_cleaned_districts$`Publicenroll_2018` <-
  as.numeric(gsub(",","",Enrolled_2018_cleaned_districts$`Public.childcare.center`))

Enrolled_2018_cleaned_districts$`2018 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2018_cleaned_districts$`Total`))

Enrolled_2018_cleaned_districts <- Enrolled_2018_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2019-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2019_cleaned <- subset(Enrolled_2019, select = -1) 

# Let's bring in column names from the top row
Enrolled_2019_cleaned <- tail(Enrolled_2019_cleaned, -3)
colnames(Enrolled_2019_cleaned)[1] <- "Location"
Enrolled_2019_cleaned <- Enrolled_2019_cleaned %>% 
    filter(Classification == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2019_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2019_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2019_cleaned_districts <- Enrolled_2019_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2019_cleaned_districts$`Publicenroll_2019` <-
  as.numeric(gsub(",","",Enrolled_2019_cleaned_districts$`Public.childcare.center`))

Enrolled_2019_cleaned_districts$`2019 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2019_cleaned_districts$`Total`))

Enrolled_2019_cleaned_districts <- Enrolled_2019_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2020-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2020_cleaned <- subset(Enrolled_2020, select = -1) 

# Let's bring in column names from the top row
Enrolled_2020_cleaned <- tail(Enrolled_2020_cleaned, -3)
colnames(Enrolled_2020_cleaned)[1] <- "Location"
Enrolled_2020_cleaned <- Enrolled_2020_cleaned %>% 
    filter(Classification == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2020_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2020_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2020_cleaned_districts <- Enrolled_2020_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2020_cleaned_districts$`Publicenroll_2020` <-
  as.numeric(gsub(",","",Enrolled_2020_cleaned_districts$`Public.childcare.center`))

Enrolled_2020_cleaned_districts$`2020 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2020_cleaned_districts$`Total`))

Enrolled_2020_cleaned_districts <- Enrolled_2020_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2021-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2021_cleaned <- subset(Enrolled_2021, select = -1) 

# Let's bring in column names from the top row
Enrolled_2021_cleaned <- tail(Enrolled_2021_cleaned, -3)
colnames(Enrolled_2021_cleaned)[1] <- "Location"
Enrolled_2021_cleaned <- Enrolled_2021_cleaned %>% 
    filter(Classification == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2021_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2021_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2021_cleaned_districts <- Enrolled_2021_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2021_cleaned_districts$`Publicenroll_2021` <-
  as.numeric(gsub(",","",Enrolled_2021_cleaned_districts$`Public.childcare.center`))

Enrolled_2021_cleaned_districts$`2021 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2021_cleaned_districts$`Total`))

Enrolled_2021_cleaned_districts <- Enrolled_2021_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----cleaning-enrollment-2022-----------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Enrolled_2022_cleaned <- subset(Enrolled_2022, select = -1) 

# Let's bring in column names from the top row
Enrolled_2022_cleaned <- tail(Enrolled_2022_cleaned, -3)
colnames(Enrolled_2022_cleaned)[1] <- "Location"
Enrolled_2022_cleaned <- Enrolled_2022_cleaned %>% 
    filter(Classification == 'Total' ) %>% 
  select(Location, Total, `Public.childcare.center`) 

# Isolating just the districts
Enrolled_2022_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Enrolled_2022_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Enrolled_2022_cleaned_districts <- Enrolled_2022_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Enrolled_2022_cleaned_districts$`Publicenroll_2022` <-
  as.numeric(gsub(",","",Enrolled_2022_cleaned_districts$`Public.childcare.center`))

Enrolled_2022_cleaned_districts$`2022 Enrolled Total` <-
  as.numeric(gsub(",","",Enrolled_2022_cleaned_districts$`Total`))

Enrolled_2022_cleaned_districts <- Enrolled_2022_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----joining-enrollment, ------------------------------------------------------------------------------------------------
joined_enrolled_districts <- Enrolled_2016_cleaned_districts %>%
  left_join(Enrolled_2017_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2018_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2019_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2020_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2021_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2022_cleaned_districts, by = "Location") %>%
  select(Location, province,
         `Publicenroll_2016`,
         `Publicenroll_2017`,
         `Publicenroll_2018`,
         `Publicenroll_2019`,
         `Publicenroll_2020`,
         `Publicenroll_2021`,
         `Publicenroll_2022`) 

joined_enrolled_provinces <- joined_enrolled_districts %>% 
  group_by(province) %>%
  summarise(`total_pub_enrolled_2016` = sum(`Publicenroll_2016`),
            `total_pub_enrolled_2017` = sum(`Publicenroll_2017`),
            `total_pub_enrolled_2018` = sum(`Publicenroll_2018`),
            `total_pub_enrolled_2019` = sum(`Publicenroll_2019`),
            `total_pub_enrolled_2020` = sum(`Publicenroll_2020`),
            `total_pub_enrolled_2021` = sum(`Publicenroll_2021`),
            `total_pub_enrolled_2022` = sum(`Publicenroll_2022`))  





## ----forming panel data at provinces for enrolment-------------------------------------------------------------------------------------------------------
df_long_enrolled <- pivot_longer(joined_enrolled_provinces, 
                                   cols = -province, names_to = "year", 
                                  values_to = "enrolled") 
df_long_enrolled <- df_long_enrolled[!is.na(df_long_enrolled$province),]

ggplot(df_long_enrolled, aes(x = as.integer(str_extract(year, "\\d+")), 
                    y = enrolled, color = province)) +
  geom_line() +
  labs(title = "Change in public childcare enrollment over time", 
       x = "Year", y = "Enrolled") +
  scale_x_continuous(breaks = 2016:2022)


## BIRTHS -------------------------------------------------------------------------------------------------------------------

Births <- read_excel('Data/Births/Copy of Live births by regionalities (2016-2022).xlsx')

Births_crude <- subset(Births, select = c(1, 5, 9, 13, 17, 21, 25, 29))

colnames(Births_crude)[1] <- "Location" 
colnames(Births_crude)[2] <- "birthrate_2016" 
colnames(Births_crude)[3] <- "birthrate_2017" 
colnames(Births_crude)[4] <- "birthrate_2018" 
colnames(Births_crude)[5] <- "birthrate_2019" 
colnames(Births_crude)[6] <- "birthrate_2020" 
colnames(Births_crude)[7] <- "birthrate_2021" 
colnames(Births_crude)[8] <- "birthrate_2022" 

Births_crude$birthrate_2016 <- as.numeric(Births_crude$birthrate_2016)
Births_crude$birthrate_2017 <- as.numeric(Births_crude$birthrate_2017)
Births_crude$birthrate_2018 <- as.numeric(Births_crude$birthrate_2018)
Births_crude$birthrate_2019 <- as.numeric(Births_crude$birthrate_2019)
Births_crude$birthrate_2020 <- as.numeric(Births_crude$birthrate_2020)
Births_crude$birthrate_2021 <- as.numeric(Births_crude$birthrate_2021)
Births_crude$birthrate_2022 <- as.numeric(Births_crude$birthrate_2022)


## births by district ----------------------------------------------------------------------------------------------------------------------------------------
# Isolating just the districts for birth-rate and creating panel data
Births_crude$is_total <- ifelse(grepl
                                 ("Total", 
                                   Births_crude$Location, 
                                   ignore.case = TRUE), 1, 0)

Births_crude_districts <- Births_crude %>%
  filter(is_total == 0)

Births_enrollment <- Births_crude_districts %>% 
  left_join(Enrolled_2016_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2017_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2018_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2019_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2020_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2021_cleaned_districts, by = "Location") %>%
  left_join(Enrolled_2022_cleaned_districts, by = "Location") %>%
  select(Location, province,
         `birthrate_2016`,
         `birthrate_2017`,
         `birthrate_2018`,
         `birthrate_2019`,
         `birthrate_2020`,
         `birthrate_2021`,
         `birthrate_2022`,
         `Publicenroll_2016`,
         `Publicenroll_2017`,
         `Publicenroll_2018`,
         `Publicenroll_2019`,
         `Publicenroll_2020`,
         `Publicenroll_2021`,
         `Publicenroll_2022`) 



panel_birth_enrollment_district <- long_panel(Births_enrollment, 
                            prefix = "_", 
                            begin = 2016, 
                            end = 2022,
                            id = "Location") %>% 
  rename(year = wave) %>% 
  na.omit(birthrate)
head(panel_birth_enrollment_district)
  


## ----births by provinces---------------------------------------------------------------------------------------------------------------------------------
Births_provinces <-  tail(Births_crude, -2) %>% 
  filter(is_total == 1) %>% 
    mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))


joined_enrolled_births_provinces <- joined_enrolled_provinces %>% 
  left_join(Births_provinces, by = "province")




## ----province-level panel data for births and enrolment--------------------------------------------------------------------------------------------------

panel_birth_enrollment_province <- long_panel(joined_enrolled_births_provinces, 
                            prefix = "_", 
                            begin = 2016, 
                            end = 2022,
                            id = "province") %>% 
  rename(year = wave) %>% 
  na.omit(birthrate)
head(panel_birth_enrollment_province)




## ----birth rate graph------------------------------------------------------------------------------------------------------------------------------------
province_to_remove <- "Sejong"
# Removing Sejong because it obscures the difference between other
# provinces due to its anamolous position

panel_birth_enrollment_province_minussejong <- panel_birth_enrollment_province %>% 
  filter(province != province_to_remove)

ggplot(panel_birth_enrollment_province_minussejong, aes(x = as.integer(str_extract(year, "\\d+")), 
                                                        y = birthrate, color = province)) +
  geom_line(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, size = 1.5, linetype = "solid", color = "black") +
  labs(title = "Births per 1,000 people, per province per year", 
       x = "Year", y = "Birth-rate", color = "Province") +
  scale_x_continuous(breaks = 2016:2022) +
  theme(legend.text = element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        panel.background = element_rect(fill = "transparent"))


## ----running basic regression----------------------------------------------------------------------------------------------------------------------------
lsdv1 <- lm(birthrate ~ Publicenroll, data = panel_birth_enrollment_district)
coeftest(lsdv1, vcov = vcovHC(lsdv1, type = "HC1"))[2,]
summary(lsdv1)$r.squared


## ----running dif in dif 2--------------------------------------------------------------------------------------------------------------------------------
feols_robust <- feols(birthrate ~ Publicenroll | factor(Location) + factor(year), 
                data = panel_birth_enrollment_district,
                vcov = "hetero")
summary(feols_robust) 


## ----POPULATION----------------------------------------------------------------------------------------------------------------


Population_2016_22 <- read.csv('Data/Population/Korea Population by Locality_2016_2022.csv')

# A lot of cleaning is then done which is not shown in the knit



## ----renaming population columns--------------------------------------------------------------------------------------------------------
colnames(Population_2016_22)[1] <- "Location"
colnames(Population_2016_22)[2] <- "Population_2016"
colnames(Population_2016_22)[3] <- "Population_2017"
colnames(Population_2016_22)[4] <- "Population_2018"
colnames(Population_2016_22)[5] <- "Population_2019"
colnames(Population_2016_22)[6] <- "Population_2020"
colnames(Population_2016_22)[7] <- "Population_2021"
colnames(Population_2016_22)[8] <- "Population_2022"


## Cleaning pop --------------------------------------------------------------------------------------------------------------------------------------------------------
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


Population_2016_22_cleaned_districts <- Population_2016_22_cleaned %>%
  filter(is_total == 0) 


Population_2016_22_cleaned_provinces <- Population_2016_22_cleaned %>%
  filter(is_total == 1) 


## ----creating panel of population 1----------------------------------------------------------------------------------------------------------------------
Population_births_enrollment <- Births_enrollment %>% 
  left_join(Population_2016_22_cleaned_districts, by = "Location") %>% 
  select (-is_total)

panel_population_birth_enrollment_district <- long_panel(Population_births_enrollment, 
                            prefix = "_", 
                            begin = 2016, 
                            end = 2022,
                            id = "Location") %>% 
    rename(year = wave) 
  
panel_population_birth_enrollment_district <- panel_population_birth_enrollment_district %>% 
  mutate(enroll_per_pop = Publicenroll / Population)


## ----Enrollment rate and population chart----------------------------------------------------------------------------------------------------------------

Population_2016_22_cleaned_provinces <- Population_2016_22_cleaned_provinces %>% 
  mutate(Location = str_replace(Location, " Total", ""))

colnames(joined_enrolled_provinces)[1] <- "Location"


Population_enrollment_provinces <- Population_2016_22_cleaned_provinces %>% 
  left_join(joined_enrolled_provinces, by = "Location")

Population_enrollment_rate_provinces <- Population_enrollment_provinces %>% 
    mutate(enroll_per_pop_2016 = total_pub_enrolled_2016 / Population_2016) %>% 
    mutate(enroll_per_pop_2017 = total_pub_enrolled_2017 / Population_2017) %>% 
    mutate(enroll_per_pop_2018 = total_pub_enrolled_2018 / Population_2018) %>% 
    mutate(enroll_per_pop_2019 = total_pub_enrolled_2019 / Population_2019) %>%   
    mutate(enroll_per_pop_2020 = total_pub_enrolled_2020 / Population_2020) %>% 
    mutate(enroll_per_pop_2021 = total_pub_enrolled_2021 / Population_2021) %>% 
    mutate(enroll_per_pop_2022 = total_pub_enrolled_2022 / Population_2022) 




## Pop enroll panel --------------------------------------------------------------------------------------------------------------------------------------------------------

Population_enrollment_rate_provinces_panel <- long_panel(Population_enrollment_rate_provinces,
                            prefix = "_", 
                            begin = 2016, 
                            end = 2022,
                            id = "Location") %>% 
      rename(year = wave) 

 


## ----enrollment rate chart-------------------------------------------------------------------------------------------------------------------------------
ggplot(Population_enrollment_rate_provinces_panel, aes(x = as.integer(str_extract(year, "\\d+")), 
                    y = enroll_per_pop, color = Location)) +
  geom_line(alpha = 0.5) +
geom_smooth(aes(group = 1), method = "lm", se = FALSE, size = 1.5, linetype = "solid", color = "black") +
  labs(title = "Public facility enrollments per capita, per province per year", 
       x = "Year", y = "Enrollment-rate", color = "Province") +
  scale_x_continuous(breaks = 2016:2022) +
  theme(legend.text = element_text(size=8),
        legend.key.size = unit(0.5, "cm"),
        panel.background = element_rect(fill = "transparent"))


## Childbearing age ####

cbp <- read_excel("Data/Population/Childbearing population.xlsx")

cbp <- cbp[, c(-2, -9)]

colnames(cbp)[1] <- "Location"

cbp

cbp_province <- cbp %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))

colnames(cbp_province)[1] <- "Location" 
colnames(cbp_province)[2] <- "cbp_2016" 
colnames(cbp_province)[3] <- "cbp_2017" 
colnames(cbp_province)[4] <- "cbp_2018" 
colnames(cbp_province)[5] <- "cbp_2019" 
colnames(cbp_province)[6] <- "cbp_2020" 
colnames(cbp_province)[7] <- "cbp_2021" 

cbp_province



## CAPACITY --------------------------------------------------------------------------------------------------------------------------------------------------------
getwd()
Capacity_2016 <- read.csv('Data/Capacity/Childcare dataset_new_2016 Capacity.csv')
Capacity_2017 <- read.csv('Data/Capacity/Childcare dataset_new_2017 Capacity.csv')
Capacity_2018 <- read.csv('Data/Capacity/Childcare dataset_new_2018 Capacity.csv')
Capacity_2019 <- read.csv('Data/Capacity/Childcare dataset_new_2019 Capacity.csv')
Capacity_2020 <- read.csv('Data/Capacity/Childcare dataset_new_2020 Capacity.csv')
Capacity_2021 <- read.csv('Data/Capacity/Childcare dataset_new_2021 Capacity.csv')
Capacity_2022 <- read.csv('Data/Capacity/Childcare dataset_new_2022 Capacity.csv')




## ----Capacity 2016-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2016_cleaned <- subset(Capacity_2016, select = -1) 

# Let's bring in column names from the top row
Capacity_2016_cleaned <- tail(Capacity_2016_cleaned, -1)
colnames(Capacity_2016_cleaned)[1] <- "Location"
Capacity_2016_cleaned <- Capacity_2016_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

Capacity_2016_cleaned <- Capacity_2016_cleaned %>% 
  filter(row_number() <= 248)

# Isolating just the districts
Capacity_2016_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2016_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2016_cleaned_districts <- Capacity_2016_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2016_cleaned_districts$`2016 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2016_cleaned_districts$`Public.childcare.center`))

Capacity_2016_cleaned_districts$`2016 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2016_cleaned_districts$`Total`))

Capacity_2016_cleaned_districts <- Capacity_2016_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2017-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2017_cleaned <- subset(Capacity_2017, select = -1) 

# Let's bring in column names from the top row
Capacity_2017_cleaned <- tail(Capacity_2017_cleaned, -1)
colnames(Capacity_2017_cleaned)[1] <- "Location"
Capacity_2017_cleaned <- Capacity_2017_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2017_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2017_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2017_cleaned_districts <- Capacity_2017_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2017_cleaned_districts$`2017 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2017_cleaned_districts$`Public.childcare.center`))

Capacity_2017_cleaned_districts$`2017 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2017_cleaned_districts$`Total`))

Capacity_2017_cleaned_districts <- Capacity_2017_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2018-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2018_cleaned <- subset(Capacity_2018, select = -1) 

# Let's bring in column names from the top row
Capacity_2018_cleaned <- tail(Capacity_2018_cleaned, -1)
colnames(Capacity_2018_cleaned)[1] <- "Location"
Capacity_2018_cleaned <- Capacity_2018_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2018_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2018_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2018_cleaned_districts <- Capacity_2018_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2018_cleaned_districts$`2018 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2018_cleaned_districts$`Public.childcare.center`))

Capacity_2018_cleaned_districts$`2018 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2018_cleaned_districts$`Total`))

Capacity_2018_cleaned_districts <- Capacity_2018_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2019-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2019_cleaned <- subset(Capacity_2019, select = -1) 

# Let's bring in column names from the top row
Capacity_2019_cleaned <- tail(Capacity_2019_cleaned, -1)
colnames(Capacity_2019_cleaned)[1] <- "Location"
Capacity_2019_cleaned <- Capacity_2019_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2019_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2019_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2019_cleaned_districts <- Capacity_2019_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2019_cleaned_districts$`2019 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2019_cleaned_districts$`Public.childcare.center`))

Capacity_2019_cleaned_districts$`2019 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2019_cleaned_districts$`Total`))

Capacity_2019_cleaned_districts <- Capacity_2019_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2020-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2020_cleaned <- subset(Capacity_2020, select = -1) 

# Let's bring in column names from the top row
Capacity_2020_cleaned <- tail(Capacity_2020_cleaned, -1)
colnames(Capacity_2020_cleaned)[1] <- "Location"
Capacity_2020_cleaned <- Capacity_2020_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2020_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2020_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2020_cleaned_districts <- Capacity_2020_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2020_cleaned_districts$`2020 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2020_cleaned_districts$`Public.childcare.center`))

Capacity_2020_cleaned_districts$`2020 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2020_cleaned_districts$`Total`))

Capacity_2020_cleaned_districts <- Capacity_2020_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2021-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2021_cleaned <- subset(Capacity_2021, select = -1) 

# Let's bring in column names from the top row
Capacity_2021_cleaned <- tail(Capacity_2021_cleaned, -1)
colnames(Capacity_2021_cleaned)[1] <- "Location"
Capacity_2021_cleaned <- Capacity_2021_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2021_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2021_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2021_cleaned_districts <- Capacity_2021_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2021_cleaned_districts$`2021 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2021_cleaned_districts$`Public.childcare.center`))

Capacity_2021_cleaned_districts$`2021 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2021_cleaned_districts$`Total`))

Capacity_2021_cleaned_districts <- Capacity_2021_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Capacity 2022-------------------------------------------------------------------------------------------------------------------------------------

# The data is very messy so let's clean it by removing the excess rows and columns
Capacity_2022_cleaned <- subset(Capacity_2022, select = -1) 

# Let's bring in column names from the top row
Capacity_2022_cleaned <- tail(Capacity_2022_cleaned, -1)
colnames(Capacity_2022_cleaned)[1] <- "Location"
Capacity_2022_cleaned <- Capacity_2022_cleaned %>% 
  select(Location, Total, `Public.childcare.center`)

# Isolating just the districts
Capacity_2022_cleaned$is_total <- ifelse(grepl
                                           ("Total", 
                                             Capacity_2022_cleaned$Location, 
                                             ignore.case = TRUE), 1, 0)

Capacity_2022_cleaned_districts <- Capacity_2022_cleaned %>%
  filter(is_total == 0)

# Ensuring all columns are stored as numeric
Capacity_2022_cleaned_districts$`2022 Public childcare center` <-
  as.numeric(gsub(",","",Capacity_2022_cleaned_districts$`Public.childcare.center`))

Capacity_2022_cleaned_districts$`2022 Capacity Total` <-
  as.numeric(gsub(",","",Capacity_2022_cleaned_districts$`Total`))

Capacity_2022_cleaned_districts <- Capacity_2022_cleaned_districts %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1))) %>%
    mutate(district = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,3),
                            word(Location,2))) %>% 
  mutate(province = factor(province, levels = c("Seoul","Busan", "Daegu", 
                                                "Incheon",
                                                "Gwangju","Daejeon", 
                                                "Ulsan","Sejong",
                                                "Gyeonggi","Gangwon",
                                                "North Chungcheong",
                                                "South Chungcheong",
                                                "North Jeolla","South Jeolla",
                                                "North Gyeongsang",
                                                "North Gyeongsang",
                                                "South Gyeongsang","Jeju"), 
                           labels = c("Seoul","Busan", "Daegu", "Incheon",
                                      "Gwangju","Daejeon", "Ulsan","Sejong",
                                      "Gyeonggi","Gangwon","North Chungcheong",
                                      "South Chungcheong","North Jeolla",
                                      "South Jeolla",
                                      "North Gyeongsang","North Gyeongsang",
                                      "South Gyeongsang","Jeju")))



## ----Joining capacity-------------------------------------------------------------------------------------------------------------------------------------
joined_capacity_districts <- Capacity_2016_cleaned_districts %>%
  left_join(Capacity_2017_cleaned_districts, by = "Location") %>%
  left_join(Capacity_2018_cleaned_districts, by = "Location") %>%
  left_join(Capacity_2019_cleaned_districts, by = "Location") %>%
  left_join(Capacity_2020_cleaned_districts, by = "Location") %>%
  left_join(Capacity_2021_cleaned_districts, by = "Location") %>%
  left_join(Capacity_2022_cleaned_districts, by = "Location") %>%
  select(Location, province,
         `2016 Public childcare center`, `2017 Public childcare center`,
         `2018 Public childcare center`, `2019 Public childcare center`,
         `2020 Public childcare center`, `2021 Public childcare center`,
         `2022 Public childcare center`) 

colnames(joined_capacity_districts)[3] <- "Capacity_2016"
colnames(joined_capacity_districts)[4] <- "Capacity_2017"
colnames(joined_capacity_districts)[5] <- "Capacity_2018"
colnames(joined_capacity_districts)[6] <- "Capacity_2019"
colnames(joined_capacity_districts)[7] <- "Capacity_2020"
colnames(joined_capacity_districts)[8] <- "Capacity_2021"
colnames(joined_capacity_districts)[9] <- "Capacity_2022"


joined_capacity_provinces <- joined_capacity_districts %>% 
  group_by(province) %>%
  summarise(`total_pub_capacity_2016` = sum(`Capacity_2016`),
            `total_pub_capacity_2017` = sum(`Capacity_2017`),
            `total_pub_capacity_2018` = sum(`Capacity_2018`),
            `total_pub_capacity_2019` = sum(`Capacity_2019`),
            `total_pub_capacity_2020` = sum(`Capacity_2020`),
            `total_pub_capacity_2021` = sum(`Capacity_2021`),
            `total_pub_capacity_2022` = sum(`Capacity_2022`))  






## Plotting Capacity --------------------------------------------------------------------------------------------------------------------------------------------------------
df_long_capacity <- pivot_longer(joined_capacity_provinces, 
                                   cols = -province, names_to = "year", 
                                  values_to = "capacity") 
df_long_capacity <- df_long_capacity[!is.na(df_long_capacity$province),]

ggplot(df_long_capacity, aes(x = as.integer(str_extract(year, "\\d+")), 
                    y = capacity, color = province)) +
  geom_line() +
  labs(title = "Change in public childcare facility capacity over time", 
       x = "Year", y = "Capacity") +
  scale_x_continuous(breaks = 2016:2022)


## ----creating panel of population 2----------------------------------------------------------------------------------------------------------------------
Population_births_enrollment <- Births_enrollment %>% 
  left_join(Population_2016_22_cleaned_districts, by = "Location") %>% 
  select (-is_total)

Population_births_enrollment <- Population_births_enrollment %>% 
  left_join(joined_capacity_districts, by = "Location") %>% 
    select (-province.y)

panel_population_birth_enrollment_district <- long_panel(Population_births_enrollment, 
                            prefix = "_", 
                            begin = 2016, 
                            end = 2022,
                            id = "Location") %>% 
    rename(year = wave) 
  
panel_population_birth_enrollment_district <- panel_population_birth_enrollment_district %>% 
  mutate(enroll_per_pop = Publicenroll / Population) %>%
  mutate(capacity_per_pop = Capacity / Population) 

colnames(panel_population_birth_enrollment_district)[3] <- "province"


## Capacity and enroll--------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's see if capacity and enrollment both went up

joined_enrolled_provinces <- joined_enrolled_districts %>% 
  group_by(province) %>%
  summarise(`total_pub_enrolled_2016` = sum(`Publicenroll_2016`),
            `total_pub_enrolled_2017` = sum(`Publicenroll_2017`),
            `total_pub_enrolled_2018` = sum(`Publicenroll_2018`),
            `total_pub_enrolled_2019` = sum(`Publicenroll_2019`),
            `total_pub_enrolled_2020` = sum(`Publicenroll_2020`),
            `total_pub_enrolled_2021` = sum(`Publicenroll_2021`),
            `total_pub_enrolled_2022` = sum(`Publicenroll_2022`))  



panel_capacity_enrolment <- panel_population_birth_enrollment_district %>% 
    select(year, Publicenroll, Capacity) 
  
panel_capacity_enrolment_summed <- panel_capacity_enrolment %>% 
  group_by(year) %>% 
  summarise(
    total_capacity = sum(Capacity, na.rm = TRUE),
    total_enrollment = sum(Publicenroll, na.rm = TRUE
  ))



# Reshape the data to long format
df_long_clustered_bar <- gather(panel_capacity_enrolment_summed, key = "type", value = "value", -year)

df_long_clustered_bar <- df_long_clustered_bar %>% 
  filter(type != 'Location')

# Convert 'year' to a factor for categorical x-axis
df_long_clustered_bar$year <- as.factor(df_long_clustered_bar$year)

# Create a clustered bar chart
ggplot(df_long_clustered_bar, aes(x = year, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Capacity and Enrollment Over Years, Across South Korea",
       x = "Year",
       y = "Spots / Enrollment") +
  scale_y_continuous(labels = scales::comma) + 
      scale_fill_manual(
    values = c("total_capacity" = "light blue", "total_enrollment" = "blue"),  # Customize colors
    name = "Legend",  # Customize legend title
    labels = c("Capacity", "Enrollment")  # Customize legend labels
  ) 


## NEWLYWEDS #### 

ncouples <- read_excel('Data/Newlyweds/Number of Newly Wed Couples by locality.xlsx')

# Name the first column as "Location"

colnames(ncouples)[1] <- "Location"

ncouples$is_total <- ifelse(grepl("Total", as.character(ncouples$Location), 
                                  ignore.case = TRUE), 1, 0)

ncouples_province <- ncouples %>% 
  filter(is_total == 1) %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))

colnames(ncouples_province)[1] <- "Location" 
colnames(ncouples_province)[2] <- "nwedcouples_2016" 
colnames(ncouples_province)[3] <- "nwedcouples_2017" 
colnames(ncouples_province)[4] <- "nwedcouples_2018" 
colnames(ncouples_province)[5] <- "nwedcouples_2019" 
colnames(ncouples_province)[6] <- "nwedcouples_2020" 
colnames(ncouples_province)[7] <- "nwedcouples_2021" 

## HH INCOME ####

hhinc <- read_excel("Data/Household income/Average Household Income by Province.xlsx")

# Name the first column as "Location"

colnames(hhinc)[1] <- "Location"

hhinc_province <- hhinc %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))

colnames(hhinc_province)[1] <- "Location" 
colnames(hhinc_province)[2] <- "hhinc_2016" 
colnames(hhinc_province)[3] <- "hhinc_2017" 
colnames(hhinc_province)[4] <- "hhinc_2018" 
colnames(hhinc_province)[5] <- "hhinc_2019" 
colnames(hhinc_province)[6] <- "hhinc_2020" 
colnames(hhinc_province)[7] <- "hhinc_2021" 


## GRDP #### 
GRDP <- read_excel("Data/GRDP/GRDP per capita_province_2016_21.xlsx")

colnames(GRDP)[1] <- "Location"

GRDPprovince <- GRDP %>% 
  mutate(province = if_else(word(Location, 1)=="North"| word(Location, 1)=="South", 
                            word(Location,1,2),
                            word(Location,1)))

colnames(GRDPprovince)[1] <- "Location" 
colnames(GRDPprovince)[2] <- "GRDP_2016" 
colnames(GRDPprovince)[3] <- "GRDP_2017" 
colnames(GRDPprovince)[4] <- "GRDP_2018" 
colnames(GRDPprovince)[5] <- "GRDP_2019" 
colnames(GRDPprovince)[6] <- "GRDP_2020" 
colnames(GRDPprovince)[7] <- "GRDP_2021" 


## Saving most data #### 

save.image(file = "Births_Enrollment_Capacity_Population.RData")


## Reading in the shapefile #### 

korea_1 <- st_read(dsn = 'Data/Mapping/gadm41_KOR_1.shp') 


## Cleaning the file for readable province names #### 
korea_1_df <- korea_1
korea_1_df[c('Province_raw', 'Province_hyphen')] <- str_split_fixed(korea_1_df$NAME_1, '-', 2)

lookup_table <- data.frame(
  old_province = c("Chungcheongbuk", "Chungcheongnam","Gyeongsangbuk",
                   "Gyeongsangnam","Jeollabuk","Jeollanam"),
  new_province = c("North Chungcheong", "South Chungcheong",
                   "North Gyeongsang","South Gyeongsang",
                   "North Jeolla","South Jeolla")
)

korea_1_df <- korea_1_df %>% 
  mutate(
    province = case_when(
      Province_raw %in% lookup_table$old_province ~ lookup_table$new_province[match(Province_raw, lookup_table$old_province)],
      TRUE ~ Province_raw
    )
  )

## Designating capital region #### 

capital_provinces <- c("Seoul", "Incheon", "Gyeonggi")

korea_1_df <- korea_1_df %>%
  mutate(Capital = ifelse(province %in% capital_provinces, "Capital", "Non-capital"))


## Testing the plotting and saving #### 

g2 <- ggplot(data = korea_1_df) +
  geom_sf(aes(fill = Capital)) + 
  theme_bw() + 
  ggtitle("Korea Provinces") 
g2

save(korea_1_df, file = "Korea_map.RData")


