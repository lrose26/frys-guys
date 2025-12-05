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
  profit = total_sales - total_expenses)

tidy_fry_data = tidy_fry_data |> mutate(
  precip_percent = probability_of_precipitation * 100)

# correlation for profit
tidy_fry_data |> ggplot(aes(x = temperature, y = profit)) + geom_point() +
geom_smooth(method = "lm") + stat_cor(method = "pearson") +
facet_wrap(~city)

tidy_fry_data |> ggplot(aes(x = probability_of_precipitation, y = profit)) + geom_point() +
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

# plots using profit:

tidy_fry_data |> ggplot(aes(x = temperature, y = profit)) + geom_point() +
geom_smooth(method = "lm")

tidy_fry_data |> ggplot(aes(x = temperature, y = profit)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)


tidy_fry_data |> ggplot(aes(x = precip_percent, y = profit)) + geom_point() +
geom_smooth(method = "lm") + 
labs(
  title = "How Precipitation Effects Profit",
  subtitle = "As chances of precipitation increases, profit decreases."
  x = "Chance of Precipitation",
  y = "Profit"

tidy_fry_data |> ggplot(aes(x = precip_percent, y = profit)) + geom_point() +
geom_smooth(method = "lm") +
facet_wrap(~city)


tidy_fry_data |> 
  ggplot(aes(x = festival, y = profit, fill = festival)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("khaki1", "dodgerblue4")) +
  labs(x = "Festival Status", y = "Profit")

tidy_fry_data |> 
  ggplot(aes(x = weekday, y = profit, fill = weekday)) +
  geom_boxplot() +
  facet_wrap(~ city) +
  scale_fill_manual(values = c("hotpink", "honeydew2")) +
  labs(x = "Weekday Status", y = "Profit")

# the big picture maybe?

tidy_fry_data |> 
  ggplot(aes(x = city, y = profit, fill = city)) +
  geom_boxplot() +
  scale_fill_manual(values = c("indianred1", "lightblue1", "lightgoldenrod", "darkolivegreen1")) +
  labs(x = "City", y = "Profit")

# big regression models

precip_regression = lm(profit ~ precip_percent, data = tidy_fry_data)

summary(precip_regression) 

temp_regression = lm(profit ~ temperature, data = tidy_fry_data)

summary(temp_regression) 

day_regression = lm(profit ~ weekday, data = tidy_fry_data)

summary(day_regression) 

fest_regression = lm(profit ~ festival, data = tidy_fry_data)

summary(fest_regression) 

city_regression = lm(profit ~ city, data = tidy_fry_data)

summary(city_regression) 

full_regression = lm(profit ~ precip_percent + weekday + festival + city, data = tidy_fry_data)

summary(full_regression)


# testing regression visualization

regression = lm(profit ~ precip_percent + weekday + city, data = tidy_fry_data)

ggplot(regression, aes(x = precip_percent, y = profit, color = weekday)) +
geom_point(alpha = 0.4, size = 2) +
geom_line(aes(y = predict(regression)), linewidth = 1.2) +
facet_wrap(~ city, ncol = 2) +
scale_color_manual(values = c("Weekend" = "darkseagreen", "Weekday" = "darkorchid4")) +
labs(
title = "Precipitation vs Profit by City",
  subtitle = "Comparing weekday sales to weekend sales",
x = "Precipitation (%)",
y = "Profit ($)") 

fest_regression = lm(profit ~ precip_percent + festival + city, data = tidy_fry_data)

ggplot(tidy_fry_data, aes(x = precip_percent, y = profit, color = festival)) +
geom_point(alpha = 0.4, size = 2) +
geom_line(aes(y = predict(fest_regression)), linewidth = 1.2) +
facet_wrap(~ city, ncol = 2) +
scale_color_manual(values = c("Festival" = "firebrick1", "No Festival" = "lavenderblush3")) +
labs(
title = "Precipitation vs Profit by City",
  subtitle = "Comparing sales at festivals to non-festival days",
x = "Precipitation (%)",
y = "Profit ($)")








































