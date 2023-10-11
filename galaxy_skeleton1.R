library(magrittr)
library(stringr)
library(tidyverse)
# Skeleton file 1 for Assignment 1 in BAN400. 
# -------------------------------------------

# Comments below describes briefly a set of steps that solves Problem 1.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.
galaxy <- readLines(con = "https://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.
L <- which(startsWith(galaxy, "----"))

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function
cat(galaxy[1:(L-2)], file = "description.txt", sep = "\n")

# Extract the variable names (i.e. line (L-1)), store the names in a vector.
variable_names <- str_replace_all(galaxy[(L-1)], " ", "") 
variable_names <- unlist(strsplit(variable_names, "\\|"))
cat(variable_names, file = "variable_names.csv", sep = ";")

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).
comma_seperated_values <- 
  galaxy[(L+1):length(galaxy)] %>% 
  str_replace_all(., " ", "") %>% 
  str_replace_all(., "\\|", ",")

# adding column names
comma_seperated_values_with_names <- c(paste(variable_names, collapse = ","), 
                                       comma_seperated_values)

# saving the file
cat(comma_seperated_values_with_names, file = "galaxies.csv", sep = "\n")

# Read the finished .csv back into R in the normal way.
galaxies_new <- read.csv("galaxies.csv")


# problem 3
# visualizing size vs distance
galaxies_new %>% 
  ggplot(aes(x = D, y = log_lk)) + 
  geom_point() +
  geom_smooth(aes(x = D, y = log_lk))

# fewer small galaxies close to us


# problem 4
# load data
hubble <- readLines("https://www.sao.ru/lv/lvgdb/article/UCNG_Table4.txt")

# use same techique to locate L
L <- which(startsWith(hubble, "----"))

# extract variable names
variable_names <- str_replace_all(hubble[(L-1)], " ", "") 
variable_names <- unlist(strsplit(variable_names, "\\|"))

# aand finalize the data
comma_seperated_values <- 
  hubble[(L+1):length(hubble)] %>% 
  str_replace_all(., " ", "") %>% 
  str_replace_all(., "\\|", ",")

# adding column names
comma_seperated_values_with_names <- c(paste(variable_names, collapse = ","), 
                                       comma_seperated_values)

# save the file
cat(comma_seperated_values_with_names, file = "hubble.csv", sep = "\n")

# read the file
hubble <- read.csv("hubble.csv") %>% 
  select(name, cz)

# combine the dfs
combined <- galaxies_new %>% 
  left_join(hubble, by = "name")

# plot 
combined %>% 
  ggplot(aes(x = D, y = cz)) +
  geom_point() +
  geom_smooth()

# looks somewhat linear with a few exeptions in the right tale
# some galaxies are moving towards us. This is Andromeda as hinted in the text


# can estimate the constant with linear regression without intercept
lm(cz ~ D - 1, data = combined) %>% summary

# coefficient of 79,073. 
# real is 67,8