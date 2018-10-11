# Compute metrics of interest on survey data

# Set up
library(tidyr)
library(dplyr)
library(stringr)

# Load data
results <- read.csv("results.csv", stringsAsFactors = FALSE)

# Reshape the results so that the question columns become a row (gather)
results_long <- gather(results, question, score, -Timestamp)

# Manipulatie the results by doing the following:
#   - Add a column that is (just) the course number
#   - Create a column for "measure" (i.e., value being assessed)
results_long <- results_long %>%
  mutate(course.number = str_extract(question, "(\\d)+")) %>%
  separate(question, c("stub", "measure"), "\\.\\.\\.\\.") %>%
  select(-stub)

# Add another identifier (because timestamp isn't sufficient)
results_long <- results_long %>%
  group_by(Timestamp, measure) %>%
  mutate(id = paste0(Timestamp, row_number())) %>%
  ungroup()

# Reshape the data to have a separate column for each measure being assessed (spread)
# The rows should now represent a response for a given course 
# (you'll have courses * responses rows)
results_wide <- results_long %>%
  select(-Timestamp) %>%
  spread(measure, score)

# Group your data by course.number and compute the mean of each measure (across respondents)
# The rows should now represent a course, and the columns are the (average) measures of interest
mean_results <- results_wide %>%
  group_by(course.number) %>%
  select(-id) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

# Compute your own metrics of interest below!
