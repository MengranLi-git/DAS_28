library(tidyverse)
library(fs)
library(tidymodels)
library(GGally)
library(car)
library(gvlma)
#### read data ####

# set data path
data_dir <- "data"
# read data document name
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")
# dataset name
dataset_name <- c(
  "Casualties", "Expenditure", "Transport",
  "Vehicles", "Cards", "Network", "Purposes"
)
# read data and assign names to dataset
for (i in 1:7) {
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
Transport <- Transport %>%
  select(-c(Measurement, Units)) %>%
  filter(`Indicator (public transport)` %in% c(
    "Number Of Passenger Train Stations",
    "Percentage Of Adults Reporting that they are Very or Fairly Satisfied with Public Transport"
  )) %>%
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
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  ) %>%
  left_join(Cards,
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  ) %>%
  left_join(Network,
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  ) %>%
  left_join(Purposes,
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  ) %>%
  left_join(Transport,
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  ) %>%
  left_join(Vehicles,
    by = c(
      "FeatureCode" = "FeatureCode",
      "DateCode" = "DateCode"
    )
  )
# rename variables
names(Data) <- c(
  "FeatureCode", "DateCode", "Expenditure", "Killed_Injured",
  "Cards", "Congestion", "Repair", "Mileage", "Work_Bus",
  "Business", "School", "Commuting", "Work_Cycling", "Education",
  "Health", "Shopping", "Work_Train", "Work_Walking", "Train_Stations",
  "Satisfaction", "One_Car", "More_Car", "Without_Car", "Petrol_Diesel"
)
glimpse(Data)
# Transform the DateCode as a factor
Data$DateCode <- as.factor(Data$DateCode)

#### data summary ####

# summary

summary(Data[, -c(1, 2)])

# density plot
Data[, -c(1, 2)] %>%
  gather(key = "variable", value = "value") %>%
  ggplot() +
  geom_histogram(aes(x=value), fill="#80C687", color="#80C687", alpha=0.8) +
  facet_wrap(~variable, scales = "free")

# scatter plot
Data[, -c(1, 2)] %>%
  gather(key = "variable", value = "value") %>%
  mutate(Satisfaction = (Data[, -c(1, 2)] %>% pull(Satisfaction) %>% rep(22))) %>%
  ggplot() +
  geom_point(aes(x = value, y = Satisfaction), fill="#80C687", color="#80C687", alpha=0.8) +
  facet_wrap(~variable, scales = "free")

# correlation plot
corr <- cor(na.omit(Data[, -c(1, 2)]))

x <- rep(names(Data[, -c(1, 2)]))
y <- rep(names(Data[, -c(1, 2)]))
corrplot <- expand.grid(X = x, Y = y)
corrplot$Z <- c(corr)

ggplot(corrplot, aes(X, Y, fill = Z)) +
  geom_tile(aes(fill = Z, alpha = 0.7), show.legend = FALSE) +
  geom_text(aes(label = round(Z, 2))) +
  scale_fill_gradient2(
    low = muted("#80C687"),
    mid = "white",
    high = muted("#FF4500"),
    midpoint = 0,
    breaks = c(-1, 0, 1), labels = c(-1, 0, 1),
    limits = c(-1, 1)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      vjust = .8,
      hjust = 0.8
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
#### linear regression ####

# full model
fit <- lm(Satisfaction ~ ., data = na.omit(Data[, -1]))
summary(fit)

# Diagnostic chart
par(mfrow = c(2, 2))
plot(fit)

gvmodel <- gvlma(fit) 
summary(gvmodel) 

vif(fit)

stepAIC(fit, na.rm = TRUE)

# 3 4 5 8 24
# cards, killed_injured, expenditure, petrol_diesel, mileage are highly correlated.
# model 2 without
names(Data)[c(3, 4, 5, 8, 24)]

fit2 <- lm(Satisfaction ~ DateCode + Cards + Repair + Work_Bus + 
             School + Health + Work_Train + Train_Stations + Without_Car + 
             Petrol_Diesel, data = na.omit(Data[, -1]))
summary(fit2)


#### Model selection ####

glance(fit)
glance(fit2)

# according to the adjusted R2 and AIC, fit3 is the best

#### Bootstrap ####
boot_models <- bootstraps(Data[, -c(1, 3, 4, 8)], times = 1000, apparent = TRUE) %>%
  mutate(
    model = map(splits, ~ lm(Satisfaction ~ DateCode + Cards + Repair + Work_Bus + 
                               School + Health + Work_Train + Train_Stations + Without_Car + 
                               Petrol_Diesel, data = .)),
    coef_info = map(model, tidy)
  )

boot_coefs <- boot_models %>%
  unnest(coef_info)

ci <- int_pctl(boot_models, coef_info)
# significant at 0.05
sig <- ci[sign(ci[, 2]) == sign(ci[, 4]), ] %>% pull(term)

boot_coefs <-  boot_coefs %>%
  mutate(sig = ifelse(term %in% sig, "TRUE", "FALSE")) %>%
  filter(term %in% c("Health", "Petrol_Diesel","Repair", "School", "Train_Stations", "Without_Car", "Work_Bus", "Work_Train"))

boot_coefs2 <- boot_coefs %>%
  group_by(term) %>%
  summarise(est = median(estimate), lower = quantile(estimate, 0.025), upper = quantile(estimate, 0.975))

boot_coefs %>%
  ggplot() +
  geom_density(aes(estimate, fill = sig), alpha = 0.7,  color = "white") +
  geom_vline(data = boot_coefs2, mapping = aes(xintercept = est))+
  geom_vline(data = boot_coefs2, mapping = aes(xintercept = lower), color="Red", lty = 2)+
  geom_vline(data = boot_coefs2, mapping = aes(xintercept = upper), color="Red", lty = 2)+
  geom_vline(data = boot_coefs2, mapping = aes(xintercept = 0), color="Blue", lty = 2)+
  facet_wrap(~term, scales = "free")







