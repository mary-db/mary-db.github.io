library(usethis)
library(here)
library(tidyverse)
library(palmerpenguins)
install.packages("car")
library(car)
#usethis::create_project("/Users/zukini/Library/Mobile Documents/com~apple~CloudDocs/Documents/School work/Hopkins/R computing/Week 3")

## Then in your new RStudio project, run the following:
## Share it via GitHub with the world
usethis::use_git()
usethis::use_github()
system("git checkout projects")
## Create a .nojekyll file
writeLines("", here::here(".nojekyll"))

qplot(x = displ, y = hwy, data = mpg, color = drv)
qplot(x = flipper_length_mm, y= bill_length_mm, data = penguins,
      color = species, geom = c("point", "smooth"))
qplot(hwy, data = mpg, fill = drv, binwidth = 2)

qplot(drv, hwy, data = mpg, geom = "boxplot")
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)

qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(displ, hwy, data = mpg, facets = . ~ drv) +
  geom_smooth(method = "lm")

qplot(x = flipper_length_mm, y= bill_length_mm, data = penguins,
      geom = c("point"), facets = species ~.)+
  geom_smooth(method = "lm")
qplot(body_mass_g, data = penguins,
      fill = species)
qplot(x = displ, y = hwy, data = mpg, shape = drv)
?mpg
str(mpg)


penguins
penguins[["species"]]
str(penguins)
head(data.matrix(penguins[,c("bill_length_mm", "bill_depth_mm", "flipper_length_mm","body_mass_g")]))
x<- as.logical(penguins$year > 2008)
sum(x)/length(x)
list.files(here::here())

#Let’s use the palmerpenguins dataset. Here are the tasks:

  #Start a for loop
#Iterate over the columns of penguins
#For each column, extract the values of that column (Hint: check out the pull()
#function in dplyr).
#Using a if-else statement, test whether or not the values in the column are
#numeric or not (Hint: remember the is.numeric() function to test if a value is numeric).
#If they are numeric, compute the column mean. Otherwise, report a NA.

for(i in 1:ncol(penguins)) {
  x <- pull(penguins,i)
  if (is.numeric(x) == T) {
    print(mean(x, na.rm = T))
  }
  else {
    print(NA)
  }
}

# Write for loops to compute the mean of every column in mtcars.

for(i in 1:ncol(mtcars)) {
  x <- pull(mtcars, i)
  print(mean(x, na.rm = T))
}

colnames(mtcars)

# Get the list of CSV files
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store the individual data frames
df_list <- list()

# Loop through each file and read it in
for (i in seq_along(files)) {
  df_list[[i]] <- read_csv(files[i])  # Read each CSV file and store it in the list
}

# Combine all the data frames into one using bind_rows from dplyr
combined_df <- bind_rows(df_list)

# View the combined data frame
print(combined_df)

x1 <- c(a = 1, b = 2)         # All elements named
x2 <- c(a = 1, 2)             # Some elements named
x3 <- c(a = 1, a = 2)         # Non-unique names
x4 <- c(1, 2)                 # No names

for (nm in names(x1)) print(nm)   # Prints "a" and "b"
for (nm in names(x2)) print(nm)   # Prints "a" and ""
for (nm in names(x3)) print(nm)   # Prints "a" and "a"
for (nm in names(x4)) print(nm)   # Does not print anything

f <- function() {
  ## This is an empty function
}
## Functions have their own class
class(f)
f()
f <- function() {
  # this is the function body
  hello <- "Hello, world!\n"
  cat(hello)
}
f()
f <- function(num) {
  for (i in seq_len(num)) {
    hello <- "Hello, world!\n"
    cat(hello)
  }
}
f(3)
f <- function(num) {
  hello <- "Hello, world!\n"
  for (i in seq_len(num)) {
    cat(hello)
  }
  chars <- nchar(hello) * num
  chars
}
meaningoflife <- f(3)
print(meaningoflife)

runif(4)
runif(1)
