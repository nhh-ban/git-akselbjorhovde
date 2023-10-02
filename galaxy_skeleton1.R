
# Skeleton file 1 for Assignment 1 in BAN400. 
# -------------------------------------------

#Problem 2
#-------------------------------------------------------------------------------

# Comments below describes briefly a set of steps that solves Problem 2.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.
library(tidyverse)

data_raw <- readLines(con="UCNG_Table4.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.

substr(x = data_raw, start = 2, stop = 3)
# Line 2

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function

#Find line
L <- 
  (substr(x = data_raw, start = 2, stop = 3) == "--") %>% 
  which(arr.ind = TRUE) %>% 
  min()

# Write data without line into csv-file
# Extract the variable names (i.e. line (L-1)), store the names in a vector.

variable_names <- 
  str_split(string = data_raw[(L-1)] , pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

comma_separated_values <- 
  data_raw[(L+1):length(data_raw)]%>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values)

#write into csv file
cat(comma_separated_values_with_names, sep = "\n", file = "data_1.csv")

# Read the finished .csv back into R in the normal way.
galaxies <- read_csv("data_1.csv")

#Problem 3
#-------------------------------------------------------------------------------
# Histogram
library(ggplot2)

ggplot(galaxies, aes(x = cz)) +
  geom_histogram(bins = 15, fill ="gray", color="black") +
  labs(title = "Galaxy Size Distribution",
       x = "Size",
       y = "Frequency") +
  theme_minimal()

# Observe lower frequency of lower size than larger size
# Assuming an more equal distribution between smaler and larger sizes, there 
# should not be a large peak

# A plausible explanation could be that the sample is affected by selection bias.
# Smaller ones could be more difficult to detect the further away from us they are
# which would exclude them from the sample of detected ones.

#Problem 4
#-------------------------------------------------------------------------------
