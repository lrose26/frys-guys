getwd()
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(janitor)
library(ggpubr)

raw_fry_data = read_excel("Edited Food Truck Raw Data.xlsx")
tidy_fry_data = raw_fry_data |> select(!c("London", "Waterloo", "Toronto")) |> janitor::clean_names()

tidy_fry_data = tidy_fry_data |> mutate(
  total_sales = quantity_sold * price
  )

tidy_fry_data = tidy_fry_data |> mutate(
  expenses_per_burger = case_when(city  == "Hamilton" ~ 3.49,
                         city == "London" ~ 2.05,
                         city == "Waterloo" ~ 2.26,
                         city == "Toronto" ~ 4.10))

tidy_fry_data = tidy_fry_data |> mutate(
  food_costs = expenses_per_burger * quantity_sold)

tidy_fry_data = tidy_fry_data |> mutate(
  travel_costs = case_when(city == "Hamilton" ~ 16.04,
                           city == "London" ~ 27.01,
                           city == "Waterloo" ~ 24.96,
                           city == "Toronto" ~ 39.55))

tidy_fry_data = tidy_fry_data |> mutate(
  total_expenses = food_costs + travel_costs)

tidy_fry_data = tidy_fry_data |> mutate(
  revenue = total_sales - total_expenses)

# no correlation 
tidy_fry_data |> ggplot(aes(x = temperature, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm")

# no correlation
tidy_fry_data |> ggplot(aes(x = temperature, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)


tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm")

tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)

# added correlation coefficients to verify
tidy_fry_data |> ggplot(aes(x = temperature, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm") + stat_cor(method = "pearson") +
facet_wrap(~city)

tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = quantity_sold)) + geom_point() +
geom_smooth(method = "lm") + stat_cor(method = "pearson") +
facet_wrap(~city)

# use box plot to ensure no skewness
tidy_fry_data = tidy_fry_data |> 
  mutate(festival = factor(festival, 
                          levels = c(0, 1),
                          labels = c("No Festival", "Festival")))


tidy_fry_data |> 
  ggplot(aes(x = festival, y = quantity_sold, fill = festival)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("coral", "steelblue")) +
  labs(x = "Festival Status", y = "Quantity Sold")

# for weekday
tidy_fry_data = tidy_fry_data |> 
  mutate(weekday = factor(weekday, 
                          levels = c(0, 1),
                          labels = c("Weekend", "Weekday")))


tidy_fry_data |> 
  ggplot(aes(x = weekday, y = quantity_sold, fill = weekday)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("lavender", "aquamarine")) +
  labs(x = "Weekday Status", y = "Quantity Sold")

# plots using revenue:

tidy_fry_data |> ggplot(aes(x = temperature, y = revenue)) + geom_point() +
geom_smooth(method = "lm")

tidy_fry_data |> ggplot(aes(x = temperature, y = revenue)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)


tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = revenue)) + geom_point() +
geom_smooth(method = "lm")

tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = revenue)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)


tidy_fry_data |> 
  ggplot(aes(x = festival, y = revenue, fill = festival)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("khaki1", "dodgerblue4")) +
  labs(x = "Festival Status", y = "Quantity Sold")

tidy_fry_data |> 
  ggplot(aes(x = weekday, y = revenue, fill = weekday)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("hotpink", "honeydew2")) +
  labs(x = "Weekday Status", y = "Quantity Sold")



























