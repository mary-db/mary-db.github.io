rm(list = ls())
library("tidyverse")
library("tidytuesdayR")
library(here)
library(usethis)
library("lubridate")
## Start version controlling it
writeLines("", here::here(".nojekyll"))

## Share it via GitHub with the world
usethis::use_git()
system("git checkout projects")

# tests if a directory named "data" exists locally
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

if (!file.exists(here("data", "tuesdata_rainfall.RDS"))) {
  tuesdata <- tidytuesdayR::tt_load("2020-01-07")
  rainfall <- tuesdata$rainfall
  temperature <- tuesdata$temperature

  # save the files to RDS objects
  saveRDS(tuesdata$rainfall, file = here("data", "tuesdata_rainfall.RDS"))
  saveRDS(tuesdata$temperature, file = here("data", "tuesdata_temperature.RDS"))
}


Exp <- function(x, k) {
  if (k <1 || k >100 || k %% 1 != 0) {
    cat("value of k must be an integere >=1 and <=100")
    return(NULL)
    }
    answer <- 1
    for(i in 1:k){
      answer <- answer + (x^i)/factorial(i)
    }
    return(answer)
  }


Exp(2,3)
Exp(2, 1.5)
Exp(2, 0)
Exp(2, 101)
Exp(2, 90)
Exp(70,70)

sample_mean <- function (x) {
  answer <-sum(x)/length(x)
  return(answer)
}

x<-1:100
sample_mean(x)

sample_sd <- function (x) {
  xbar <- sum(x)/length(x)
  diff <- sum((x - xbar)^2)
  inside <- diff/(length(x)-1)
  stdev <- sqrt(inside)
  return(stdev)
  }

sample_sd(x)

calculate_CI <- function (x, conf = 0.95) {
  alpha <- 1 - conf
  degrees_freedom <- length(x) - 1
  t_score <- qt(p = alpha / 2, df = degrees_freedom, lower.tail = FALSE)
  se <- sd(x)/sqrt(length(x))
  lower_bound <- mean(x)-t_score*se
  upper_bound <- mean(x)+t_score*se
  CI <- c(lower_bound = lower_bound, upper_bound=upper_bound)
  return(CI)
}

calculate_CI(x, 0.95)

rainfall <- readRDS(here("data", "tuesdata_rainfall.RDS"))
temperature <- readRDS(here("data", "tuesdata_temperature.RDS"))
glimpse(rainfall)
glimpse(temperature)

rainfall <- rainfall %>% filter(complete.cases(.))
rainfall <- rainfall %>% mutate(date = make_date(year, month, day)) %>%
  select(- month, - day)
class(rainfall$date)

rainfall <- rainfall %>% mutate(city_name =toupper(city_name))
rainfall$city_name

joined_data <- inner_join(rainfall, temperature, by = c("city_name", "date"))


df <- joined_data %>% filter(year >= 2014)
df <- df %>%
  group_by(date, city_name) %>%
  summarize(
    max_temperature = max(temperature),
    min_temperature = min(temperature),
    .groups = "drop"
  )


ggplot(df, aes(x=date)) +
 geom_line(aes(y =max_temperature, color = "Max Temperature")) +
  geom_line(aes(y =min_temperature, color = "Min Temperature")) +
  facet_wrap(~city_name)+
  labs(title = "Max and Min Temperatures Since 2014 in Major Australian Cities",
       subtitle = "Little Change in Overall Max/Min Temperature Variability in the Last Decade",
       caption = "This graph was produced by Mary de Boer",
      x = "Date since 2014",
      y = "Temperature in Celsisus") +
  scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue"))


joined_data %>%
  filter(city_name == "PERTH", year == 2000) %>%
  ggplot(aes(log(rainfall))) +
  geom_histogram()


city_hist <- function (city_name, year) {
  # Convert city_name argument to string if passed as a symbol
  city_name <- as.character(toupper((substitute(city_name))))

  #Make a version for the ggplot
  city_name_formatted <- tools::toTitleCase(tolower(city_name))

  # Check if the combination of city and year exists in the data
  if (!(city_name %in% unique(joined_data$city_name) && year %in% unique(joined_data$year))) {
    stop("This combination of city and year does not exist in the dataset.")
  }


  joined_data %>%
    filter(city_name == !!city_name, year == !!year) %>%
    ggplot(aes(log(rainfall))) +
    geom_histogram(fill = "blue", color = "black") +
    labs(title = paste("Rainfall Histogram for", city_name_formatte, "in", year),
         x = "Log of Rainfall in mm",
         y = "Frequency")
}

city_hist(melbourne, 2022)
city_hist(Brisbane, 2000)
city_hist(BRISBANE, 2007)
as.character(toupper("Perth"))
unique(rainfall$city_name)

#Part 4A: Tasks
#In this part, we will apply the functions we wrote in Part 1 to our rainfall data
#starting with our wrangled df data from Part 2.

#First, filter for only years including 2014 and onwards.
#For a given city and for a given year, calculate the sample mean (using your function
# sample_mean()), the sample standard deviation (using your function sample_sd()),
# and a 95% confidence interval for the average rainfall (using your function calculate_CI()).
# Specifically, you should add two columns in this summarized dataset: a column
# titled lower_bound and a column titled upper_bound containing the lower and upper
# xfbounds for you CI that you calculated (using your function calculate_CI()).
# Call this summarized dataset rain_df.#

rain_df <- joined_data %>%
  filter(year >= 2014) %>%
  group_by(city_name, year) %>%
  summarise(mean_rainfall = sample_mean(rainfall), sd = sample_sd(rainfall),
            CI = list(calculate_CI(rainfall))) %>%
  mutate(
    CI_LB = sapply(CI, function(x) x[["lower_bound"]]),  # Extract lower bounds
    CI_UB = sapply(CI, function(x) x[["upper_bound"]])   # Extract upper bounds
  ) %>%
  select(-CI)

# Using the rain_df, plots the estimates of mean rainfall and the 95% confidence
# intervals on the same plot. There should be a separate faceted plot for each city.
# Think about using ggplot() with both geom_point() (and geom_line() to connect the
# points) for the means and geom_errorbar() for the lower and upper bounds of the
# confidence interval. Check https://r-graphics.org/recipe-annotate-error-bar and
# or the official documentation https://ggplot2.tidyverse.org/reference/geom_linerange.html
# for examples of how to use geom_errorbar().

ggplot(rain_df, aes(x=year)) +
  geom_line(aes(y =mean_rainfall)) +
  geom_errorbar(aes(ymin = CI_LB, ymax = CI_UB), width = .2) +
  facet_wrap(~city_name)+
  labs(title = "Mean Rainfall in Major Australian Cities Since 2014",
       subtitle = "Declining average rainfall is seen in all cities except Canberra",
       caption = "This graph was produced by Mary de Boer",
       x = "Date (years)",
       y = "Rainfall (mm)")
