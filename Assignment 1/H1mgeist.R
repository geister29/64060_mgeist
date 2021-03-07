library(readr)
mlb <- read_csv("School/MIS 64060/Assignment 1/2010-2019mlb.csv")
cols(
  yearID = col_double(),
  lgID = col_character(),
  teamID = col_character(),
  franchID = col_character(),
  divID = col_character(),
  G = col_double(),
  W = col_double(),
  L = col_double(),
  name = col_character()
  )
View(mlb)
mlb
str(mlb)
cols(
  yearID = col_double(),
  lgID = col_character(),
  teamID = col_character(),
  franchID = col_character(),
  divID = col_character(),
  G = col_integer(),
  W = col_double(),
  L = col_double(),
  name = col_character())
plot(mlb$yearID,mlb$W)
my_data <- subset(mlb, teamID == "CLE")
my_data
plot(my_data$yearID,my_data$W)
print("done")

