
# Skeleton file 1 for Assignment 1 in BAN400. 
# -------------------------------------------

#Problem 2
#-------------------------------------------------------------------------------

# Comments below describes briefly a set of steps that solves Problem 2.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.
library(tidyverse)

data_raw_1 <- readLines(con="suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.

substr(x = data_raw_1, start = 2, stop = 3)
# Line 2

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function

#Find line
L <- 
  (substr(x = data_raw_1, start = 2, stop = 3) == "--") %>% 
  which(arr.ind = TRUE) %>% 
  min()

# Write data without line into csv-file
# Extract the variable names (i.e. line (L-1)), store the names in a vector.

variable_names <- 
  str_split(string = data_raw_1[(L-1)] , pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

comma_separated_values <- 
  data_raw_1[(L+1):length(data_raw_1)]%>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)

#write into csv file
cat(comma_separated_values_with_names, sep = "\n", file = "data_1.csv")

# Read the finished .csv back into R in the normal way.
galaxies_1 <- read_csv("data_1.csv")

#Problem 3
#-------------------------------------------------------------------------------
# Histogram
library(ggplot2)

ggplot(galaxies_1, aes(x = a_26)) +
  geom_histogram(bins = 25, fill ="gray", color="black") +
  labs(title = "Galaxy Size Distribution",
       x = "Linear diameter of the galaxy in kpc (a_26)",
       y = "Frequency") +
  theme_minimal()

# Observe lower frequency of lower size than larger size
# Assuming an more equal distribution between smaller and larger sizes, there 
# should not be a large peak

# A plausible explanation could be that the sample is affected by selection bias.
# Smaller ones could be more difficult to detect the further away from us they 
# are which would exclude them from the sample of detected ones.

#Problem 4
#-------------------------------------------------------------------------------
#P1 - Redo operations from P2 with new data
data_raw_2 <- readLines(con="UCNG_Table4.txt")

substr(x = data_raw_2, start = 2, stop = 3)

L <- 
  (substr(x = data_raw_2, start = 2, stop = 3) == "--") %>% 
  which(arr.ind = TRUE) %>% 
  min()

variable_names <- 
  str_split(string = data_raw_2[(L-1)] , pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

comma_separated_values <- 
  data_raw_2[(L+1):length(data_raw_2)]%>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)

#write into csv file
cat(comma_separated_values_with_names, sep = "\n", file = "data_2.csv")

# Read the finished .csv back into R in the normal way.
galaxies_2 <- read_csv("data_2.csv")

# Select and merge relevant data from the two datasets
galaxies_1 <- galaxies_1 |> select(name, a_26, D, md)
galaxies_2 <- galaxies_2 |> select(name, cz)

galaxies_1 <- galaxies_1 |> full_join(galaxies_2)

# Create plot
ggplot(galaxies_1) +
  geom_point(aes(x = D, y = cz)) +  # Create a scatterplot
  labs(title = "Galaxies: Distance vs. Velocity",
       x = "Distance",
       y = "Velocity") +
  geom_smooth(aes(x = D, y = cz),method = "lm", se = FALSE, color = "red",linetype=2) +
  theme_minimal()

#We observe a positive correlation between distance and velocity indicating
#what Hubble in accordance with what Hubble found

#P2
#Convert km/h to km/s
galaxies_1 <- galaxies_1 |> mutate(cz1 = cz/3600)

#Calculate hubbles constant estimate
hubbles_constant <- 
  mean(galaxies_1$cz1, na.rm = T) / mean(galaxies_1$D, na.rm = T)
#Hubbles constant is estimated to be 0.02km/s/Mpc

hubbles_constant
#Approximatly the same as other estimates