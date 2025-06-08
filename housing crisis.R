install.packages("tidyverse")
install.packages("readxl")
install.packages("stringr")
library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(ggrepel)

overburden <- ilc_lvho07a
household <- lfst_hhnhtych
migration <- tps00019
gov_exp <- gov_10a_exp
gdp <- tec00114

#create a dataframe for model1
df <- reduce(list(overburden, household, migration), full_join, by = "country")

df$household_adjusted <- (df$one_person_household_2023 / lfst_hhnhtych_total_households$total_households ) * 100

#model 1 = predicing housing crisis using single-person household and migration
cor(df$overburden_rate_2023, df$household_adjusted)
cor(df$overburden_rate_2023, df$population_change_2023)
model1 <- lm(df$overburden_rate_2023 ~ df$household_adjusted + df$population_change_2023)
summary(model)

df %>%
  ggplot(aes(x = household_adjusted, y = overburden_rate_2023)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = country), size = 3)

#model 2 = predicting housing crisis using single-person household and government expenditure on housing
cor(df$overburden_rate_2023, gov_exp$gov_expenditure_housing_2023)
model2 <- lm(df$overburden_rate_2023 ~ df$household_adjusted + gov_exp$gov_expenditure_housing_2023)
summary(model2)

#model 3 = predicting housing crisis using gdp and housing inflation
  #calculating avarege hicp for each country for year 2023
hicp_avg <- prc_hicp_midx %>%
  rowwise() %>%
  mutate(mean_housing_inflation_2023 = mean(c_across(Jan:Dec), na.rm = TRUE)) %>%
  ungroup()

  #creating a dataframe
clean_hicp <- hicp_avg %>%
  select(country, mean_housing_inflation_2023)

df3 <- reduce(list(overburden, clean_hicp, gdp), full_join, by = "country")

    #gdp logaritmized
df3$log_gdp <- log(gdp$gdp_per_cap)

  #model 3 = predicting housing crisis with gdp and inflation
cor(df$overburden_rate_2023, hicp_avg$mean_housing_inflation_2023)
cor(df$overburden_rate_2023, df3$log_gdp)
model3 <- lm(df$overburden_rate_2023 ~ hicp_avg$mean_housing_inflation_2023 + df3$log_gdp)
summary(model3)

df3 %>% 
  ggplot(aes(x = mean_housing_inflation_2023, y = overburden_rate_2023)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = country), size = 3)

df3 %>% 
  ggplot(aes(x = log_gdp, y = overburden_rate_2023)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = country), size = 3)
