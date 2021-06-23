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
  filter(Outcome == "Killed Or Seriously Injured") %>%
  spread(Outcome, Value) %>%
  filter(Age == "All" & Gender == "All") %>%
  select(-c(Age, Gender))

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

glimpse(Data)
names(Data) <- c("FeatureCode", "DateCode", "Expenditure", "Killed_Injured",
                 "Cards", "Congestion", "Repair", "Mileage", "Work_Bus",
                 "Business", "School", "Commuting", "Work_Cycling", "Education",
                 "Health", "Shopping", "Work_Train", "Work_Walking", "Train_Stations",
                 "Satisfaction", "One_Car", "More_Car", "Without_Car", "Petrol_Diesel")

Data$DateCode <- as.factor(Data$DateCode)
fit <- lm(Satisfaction~., data = Data[,-1])
summary(fit)

cor(na.omit(Data[,-c(1, 2, 3, 4)]))

library(car)
vif(fit)

corrgram::corrgram(na.omit(Data[,-c(1, 2, 3, 4, 8)]))

fit2 <- lm(Satisfaction~., data = Data[,-c(1, 3, 4, 8)])
summary(fit2)


n <- nrow(Data)[1] # sample size
samp <- c(1:n)

set.seed(21) # for reproducible results
sample(samp, size = n, replace = TRUE) # sample with replacement

coef <- fit2$coefficients

nboot <- 1000 # number of bootstrap samples

bs <- function(formula, data, indices) { 
  boot.samp <- Data[sample(samp, size = n, replace = TRUE),] 
  fit <- lm(formula, data = boot.samp[,-c(1, 3, 4, 8)])
  return(coef(fit)) 
}

library(boot) 
results <- boot(data=Data, statistic=bs, 
                R=1000, formula=Satisfaction~.) 
print(results) 

ci <- matrix(NA, nrow = 25, ncol = 2)
for(i in 1:25){
  ci[i,] <- boot.ci(results, type="bca", index=i)$bca[4:5]
}

# significant at 0.05
coef(fit)[sign(ci)[,1] == sign(ci)[,2]] 


