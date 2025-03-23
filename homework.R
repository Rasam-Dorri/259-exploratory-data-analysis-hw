# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

read_weather <- function(station_abbr) {
  read_csv(
    file = file.path("us-weather-history", paste0(station_abbr, ".csv"))
  ) %>%
    mutate(
      date    = ymd(date),          # Convert date string to Date
      station = station_abbr        # Add station abbreviation column
    )
}

# Quick check on one station:
test_station <- read_weather("KCLT")
glimpse(test_station)


# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

ds <- map(stations, ~ read_weather(.x)) %>%
  bind_rows()

glimpse(ds)  # Check the combined dataset


# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

ds <- ds %>%
  mutate(
    city = factor(
      station,
      levels = stations,  # the order (levels) of station abbreviations
      labels = cities     # the labels to use for each level
    )
  )

# Check with fct_count:
fct_count(ds$city)



# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

# 1. Define a helper function
f2c <- function(f) {
  (f - 32) * (5/9)
}

# 2. Convert all temperature columns that contain the string "temp"
ds <- ds %>%
  mutate(
    across(contains("temp"), ~ round(f2c(.x), 1))
  )

glimpse(ds)


### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

ds <- read_csv("data-clean/compiled_data.csv") 
   %>% mutate(date = ymd(date))  # if necessary


# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

count_extreme_days <- function(.df) {
  .df %>%
    filter(
      actual_min_temp == record_min_temp |
        actual_max_temp == record_max_temp
    ) %>%
    nrow()
}

ds %>%
  group_by(city) %>%
  summarise(extreme_days = count_extreme_days(cur_data())) %>%
  arrange(desc(extreme_days))

#Mcomment: Check out the alternative code from the key
extreme_days <- . %>% 
  mutate(is_extreme_day = actual_min_temp == record_min_temp | actual_max_temp == record_max_temp,
         is_extreme_day = as.numeric(is_extreme_day)) %>% 
  summarize(n_extreme = sum(is_extreme_day))

ds %>% group_by(city) %>% extreme_days %>% arrange(-n_extreme)

# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 


ds <- ds %>%
  mutate(
    month = factor(month(date), 
                   levels = 1:12, 
                   labels = month.name)
  )

#Mcomment: if you use month( ,label = T) you don't need the factor() function
ds$month <- month(ds$date, label = T) #makes new variable into an ordered factor

# Split into a list of 12 tibbles, one per month
ds_by_month <- split(ds, ds$month)

# or use group_split:
# ds_by_month <- ds %>% group_by(month) %>% group_split()



# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

unique_months <- levels(ds$month)

for (m in unique_months) {
  # Subset data for this month
  temp_ds <- ds %>% filter(month == m)
  
  # Compute correlations
  cor_precip <- cor(temp_ds$actual_precipitation, temp_ds$average_precipitation)
  cor_min    <- cor(temp_ds$actual_min_temp, temp_ds$average_min_temp)
  cor_max    <- cor(temp_ds$actual_max_temp, temp_ds$average_max_temp)
  
  # Print results
  cat(
    "Month:", m, "\n",
    "   - Actual vs Avg Precip:", round(cor_precip, 3), "\n",
    "   - Actual vs Avg Min Temp:", round(cor_min, 3), "\n",
    "   - Actual vs Avg Max Temp:", round(cor_max, 3), "\n\n"
  )
}



# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this


# Boxplots by city
plot_boxplot(ds, by = "city")

# Boxplots by month
plot_boxplot(ds, by = "month")

# Correlation among numeric columns only
plot_correlation(ds) 




# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month

ggplot(ds, aes(x = date, y = actual_mean_temp, color = month)) +
  geom_point() +
  facet_wrap(~ city, ncol = 3) +
  theme_minimal() +
  labs(
    title = "Actual Mean Temperature Over Time by City",
    x = "Date",
    y = "Temperature (°C)"
  )



# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month



# 1. Define your plotting function
plot_month_data <- function(.data, month_abb) {
  
  # If your dataset has a full month factor, you can convert or match it. 
  # For example, if you store abbreviated month in a separate column,
  # you might do something like this:
  .data <- .data %>%
    mutate(month_abb = month(date, label = TRUE, abbr = TRUE))
  
  # Filter for the chosen abbreviated month
  sub_df <- .data %>%
    filter(month_abb == month_abb)
  
  # Create the plot
  p <- ggplot(sub_df, aes(x = date, y = actual_mean_temp)) +
    geom_point() +
    geom_line() +
    ggtitle(paste("Month:", month_abb)) +
    theme_minimal() +
    labs(y = "Actual Mean Temp (°C)")
  
  # Save the plot
  ggsave(
    filename = file.path("eda", paste0(month_abb, ".png")),
    plot = p,
    width = 6, height = 4
  )
}

# 2. Loop (or map) over the 3-letter abbreviations: Jan, Feb, ...
all_month_abbs <- month.abb  # c("Jan", "Feb", ..., "Dec")
walk(all_month_abbs, ~ plot_month_data(ds, .x))



