library(tidyverse)
library(fs)
library(car)
library(boot) 
library(tidymodels)

#### read data ####

# set data path
data_dir <- "data"
# read data document name
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")
# dataset name
dataset_name <- c("Casualties", "Expenditure", "Transport",
          "Vehicles", "Cards", "Network", "Purposes")
# read data and assign names to dataset
for(i in 1:7){
  assign(dataset_name[i], readr::read_csv(csv_files[i]))
}

#### data wrangling ####

# select total number of casualty outcomes
Casualties <- Casualties %>% 
  select(-c(Measurement, Units)) %>%
  filter(Outcome == "Killed Or Seriously Injured") %>%
  spread(Outcome, Value) %>%
  filter(Age == "All" & Gender == "All") %>%
  select(-c(Age, Gender))

# rename variable name
Expenditure <- Expenditure %>%
  select(-c(Measurement, Units)) %>%
  rename(Expenditure = Value)

# remove variables at the whole Scotland level
table(Transport$`Indicator (public transport)`)
Transport <- Transport %>%
  select(-c(Measurement, Units)) %>%
  filter(`Indicator (public transport)` %in% c("Number Of Passenger Train Stations",
             "Percentage Of Adults Reporting that they are Very or Fairly Satisfied with Public Transport")) %>%
  spread(`Indicator (public transport)`, Value)

Vehicles <- Vehicles %>%
  select(-c(Measurement, Units)) %>%
  spread(`Indicator (road vehicles)`, Value)

# retain card numbers of all people
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

# merge all dataset into one complete dataset by 'FeatureCode' and 'DateCode'
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
# rename variables
names(Data) <- c("FeatureCode", "DateCode", "Expenditure", "Killed_Injured",
                 "Cards", "Congestion", "Repair", "Mileage", "Work_Bus",
                 "Business", "School", "Commuting", "Work_Cycling", "Education",
                 "Health", "Shopping", "Work_Train", "Work_Walking", "Train_Stations",
                 "Satisfaction", "One_Car", "More_Car", "Without_Car", "Petrol_Diesel")
glimpse(Data)
# Transform the DateCode as a factor
Data$DateCode <- as.factor(Data$DateCode)

#### data summary ####



cor(na.omit(Data[,-c(1, 2)]))

#### linear regression ####

# full model
fit <- lm(Satisfaction~., data = Data[,-1])
summary(fit)

# Diagnostic chart
par(mfrow=c(2,2)) 
plot(fit)



# model 2 without 
fit2 <- lm(Satisfaction~., data = Data[,-c(1, 3, 4, 8)])
summary(fit2)

#### Model selection ####

#### Bootstrap ####
Data %>%
  specify(formula = Satisfaction~., success = "yes") %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")
# sample size
n <- nrow(Data)[1] 
samp <- c(1:n)

# for reproducible results
set.seed(21) 

# number of bootstrap samples
nboot <- 1000 

# a function used to lm estimation via bootstrap
bs <- function(formula, data, indices) { 
  boot.samp <- Data[sample(samp, size = n, replace = TRUE),] 
  fit <- lm(formula, data = boot.samp[,-c(1, 3, 4, 8)])
  return(coef(fit)) 
}

# apply to the dataset
results <- boot(data=Data, statistic=bs, 
                R=1000, formula=Satisfaction~.) 
print(results) 

# confidence interval of 95%
ci <- matrix(NA, nrow = 25, ncol = 2)
for(i in 1:25){
  ci[i,] <- boot.ci(results, type="bca", index=i)$bca[4:5]
}

# significant at 0.05
coef(fit)[sign(ci)[,1] == sign(ci)[,2]] 

