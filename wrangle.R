library(tidyverse)
library(haven)
library(labelled)

max5 <- read_sav("Data/Maxwell Poll 2005.sav")
  val_labels(max5) <- NULL
write_csv(max5, "Data/maxwell05.csv")

max5 <- read_csv("Data/maxwell05.csv")

##pick variables to keep
##count missingness
##match recertification based on state and program
