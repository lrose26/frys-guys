getwd()
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(janitor)
library(ggpubr)

raw_fry_data = read_excel("Edited Food Truck Raw Data.xlsx")
tidy_fry_data = raw_fry_data |> select(!c("London", "Waterloo", "Toronto")) |> janitor::clean_names()

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

# using geom_col() because of the 0 and 1 variable
# 0 = no festival, 1 = festival 
tidy_fry_data |> ggplot(aes(x = festival, y = quantity_sold)) + geom_col() +
facet_wrap(~city)

# 0 = weekend, including Friday, 1 = weekday
tidy_fry_data |> ggplot(aes(x = weekday, y = quantity_sold)) + geom_col() +
facet_wrap(~city)

tidy_fry_data |> ggplot(aes(x = weekday, y = quantity_sold)) + geom_boxplot() +
facet_wrap(~city)













