library(tidyverse)
library(fs)
data_dir <- "data"
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

dataset_name <- c("Casualties", "Expenditure", "Transport",
          "Vehicles", "Cards", "Network", "Purposes")
for(i in 1:7){
  assign(dataset_name[i], readr::read_csv(csv_files[i]))
}

Casualties <- Casualties %>% 
  select(-c(Measurement, Units)) %>%
  spread(Outcome, Value) %>%
  filter(Age == "All" & Gender == "All")

Expenditure <- Expenditure %>%
  select(-c(Measurement, Units)) %>%
  rename(Expenditure = Value)

Transport <- Transport %>%
  select(-c(Measurement, Units)) %>%
  filter(`Indicator (public transport)` %in% c("Number Of Passenger Train Stations",
             "Percentage Of Adults Reporting that they are Very or Fairly Satisfied with Public Transport")) %>%
  spread(`Indicator (public transport)`, Value)

Vehicles <- Vehicles %>%
  select(-c(Measurement, Units)) %>%
  spread(`Indicator (road vehicles)`, Value)

Cards <- Cards %>%
  select(-c(Measurement, Units)) %>%
  spread(Age, Value) %>%
  select(-`60 years and over`) %>%
  rename(Cards = All)

Network <- Network %>%
  select(-c(Measurement, Units)) %>%
  spread(`Indicator (road network traffic)`, Value)

Purposes <- Purposes %>%
  select(-c(Measurement, Units)) %>%
  spread(`Indicator (travel to work)`, Value)

Data <- Expenditure %>% 
  left_join(Casualties, 
                          by = c("FeatureCode" = "FeatureCode", 
                                 "DateCode" = "DateCode")) %>%
  left_join(Cards, 
            by = c("FeatureCode" = "FeatureCode", 
                   "DateCode" = "DateCode")) %>%
  left_join(Network, 
            by = c("FeatureCode" = "FeatureCode", 
                   "DateCode" = "DateCode")) %>%
  left_join(Purposes, 
            by = c("FeatureCode" = "FeatureCode", 
                   "DateCode" = "DateCode")) %>%
  left_join(Transport, 
            by = c("FeatureCode" = "FeatureCode", 
                   "DateCode" = "DateCode")) %>%
  left_join(Vehicles, 
            by = c("FeatureCode" = "FeatureCode", 
                   "DateCode" = "DateCode"))

fit <- lm(`Percentage Of Adults Reporting that they are Very or Fairly Satisfied with Public Transport`~., data = Data[,-c(1,4,5)])
summary(fit)

names(Data)
