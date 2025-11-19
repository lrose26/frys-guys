getwd()
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(janitor)

raw_fry_data = read_excel("Edited Food Truck Raw Data.xlsx")
tidy_fry_data = raw_fry_data |> select(!c("London", "Waterloo", "Toronto")) |> janitor::clean_names()

tidy_fry_data |> ggplot(aes(x = quantity_sold, y = temperature) + geom_point()








