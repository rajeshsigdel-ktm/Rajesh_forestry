dbh <- 28.4
height <- 20.1
#objects can be overwritten if a measurement is 
ba <- pi * (dbh^2)/40000
ba
# volume <- basal_area * height
###########
dbh_num <- 30.2
tree_count <- 123
special_character <- "katush"
is_tropical_types <- FALSE
# Create a complex number
z <- 3 + 4*dbh_num 
class(special_character)
typeof(special_character)
typeof(is_tropical_types)
class(is_tropical_types)
#########################
is.logical(is_tropical_types) #TRUE
is.integer(is_tropical_types)  #FALSE

#Number Stored as Text (Common Field Sheet Error)
### different types of data can be changed to numeric using as.numeric() function

messy_dbh <- c(12,4234,"28.4", 54252,"30.2", 5235235,"25.1", "32.0")
clean_dbh <- as.numeric(messy_dbh)
pi * (clean_dbh^2) / 40000
class(clean_dbh)

### what if a text string contains units like "35.1 cm"?
bad_entry <- c("35.1 cm", "28.4 cm", "30.2 cm")
as.numeric(bad_entry) # returns NA with warning 

##############
forest_types <- c("tropical", "temperate", "boreal", "tropical", "temperate")
forest_types_factor <- as.factor(forest_types)
class(forest_types_factor)
levels(forest_types_factor)

##Scenario E: the classic factor to numeric problem
# if numeric codes are stored as factors, you need to convert them to character first,
## then numeric:

plot_codes_factor <- factor(c("101", "102", "105"))
as.numeric(plot_codes_factor)                # Returns level indices: 1 2 3
as.numeric(as.character(plot_codes_factor))  # Correct: 101 102 105


# 4. implicit coercion (when R changes automatically converts in a vector)
mixed_vector <- c(28.4, TRUE, "Katush")
mixed_vector   ## "28.4" "TRUE" "Katush" all silently converted to character!
class(mixed_vector)


#Section 3: logical statements and relational operators
#Ask TRUE/FALSE questions about your data tree measurement to audit and filter data.

#1. Relational (Comaprison) Operators: <, >, <=, >=, == (), != (not equal)
dbh > 20
dbh <= 10
special_character 
special_character == "shorea robusta"   #FALSE (Exact match check: note the double equal sign)
special_character != "pinus roxburghii" #TRUE (Not equal to chir pine)


## 2. compound logical operators: & (AND), | (OR), ! (NOT)
# Merchantable timber rule: DBH > 30 cm and height > 20 m
(dbh > 30) & (height > 20)  #FALSE (both conditions must be TRUE)

## pole sized range check: is DBH between 10 and 30 cm?
(dbh >= 10) & (dbh <= 30)  #TRUE (both conditions must be TRUE)

##Flagging rule: Is DBH negative OR absurdly large (> 150 cm)?
(dbh<= 0) | (dbh > 150) #FALSE (TRUE if at least one condition is met

#Negation (! flips TRUE to FALSE and vice versa)
!(dbh < 0) #TRUE (dbh is not negative)

dbh <- 300
# 3. Condtional Decision Making with if/else (for a single value)
## cat and print are used to display output to the console, 
## print garda line number aauccha ## cat ma aaudaina
if (dbh >= 30) {
  cat("Classification: Sawtimber tree\n") ###\n for line break
} else if (dbh >= 10) {
 cat("Classification: Pole-sized tree\n")
} else {
  cat("Classification: Sapling\n")
}

# SECTION 4: Vectors & Missing Values
## combine multiple DBH measurements into a numerica vector using c()
# Vector of DBH measurements (cm)
trees_dbh <- c(28.4, 35.1, 17.8, 42.0, 22.4)

# Vectorised formula: calculates all 5 basal areas simultaneously!
trees_ba <- pi * (trees_dbh^2) / 40000
round(trees_ba, 4)

# Vectorised logical choice with ifelse(test, yes, no) function:
ifelse(trees_dbh >= 30, "Large Stem", "Small/Medium Stem")

# Handling Missing Values (NA) in field data
heights <- c(19.2, 22.4, NA, 25.1, 17.1)

##Default: fails safely so you know data are missing
mean(heights)                # Returns NA (fails safely)

# The fix: tell R to remove NAs before calculating
mean(heights, na.rm = TRUE)  # Removes NA: returns 20.95 m

# How many trees had missing heights measurement?
is.na(heights)          # FALSE FALSE TRUE FALSE FALSE
sum(is.na(heights))     # Returns 1 (number of missing values)


## 1. Test how logical checks behave on a vector with planed errors
test_trees <- c(28.4, -35.1, 999, 42.0, 0, NA)

#which trees have impossible non positive diameters?
test_trees <= 0

#which trees are suspiciously large (> 150 cm) AND not missing?
test_trees > 150 & !is.na(test_trees)

#Do any trees in the vector violate the > 0 rule?
any(test_trees <= 0, na.rm =TRUE) #Returns a single TRUE
##############################################
############################################
#############################################
# 2. Build a defensive basal area function
calculate_basal_area <- function(dbh_cm) {
  # Guard 1: Type check
  if (!is.numeric(dbh_cm)) {
    stop("Error: DBH must be numeric (measured in cm)!")
  }
  # Guard 2: Biologically impossible negative or zero values
  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("Error: DBH must be strictly greater than 0!")
  }
  # Guard 3: Warn on possible unit errors (e.g. mm recorded instead of cm)
  if (any(dbh_cm > 150, na.rm = TRUE)) {
    warning("Warning: DBH > 150 cm detected. Did you record in mm?")
  }
  
  ba_m2 <- pi * (dbh_cm^2) / 40000
  return(ba_m2)
}

# Test valid input:
calculate_basal_area(28.4)

# Test warning guard (run with 999 cm):
calculate_basal_area(c(28.4, 999))

# Coefficient of Variation function (CV%)
calc_cv <- function(x) {
  cv <- (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
  return(round(cv, 2))
}
calc_cv(c(28.4, 35.1, 17.8, 42.0, 22.4, 25.1))


## How to load csv file with relative path (from the current working directory)
treeDatas <- read.csv("./DATA/RawDATA/unit2_starter_trees.csv")

# Inspect the dataset
head(treeDatas)
nrow(treeDatas, 9)       # Total tree stems
ncol(treeDatas)       # Number of attributes
names(treeDatas)      # Column names
str(treeDatas)        # Column types

#access a single column as a vector using the $ operator
treeDatas$dbh_cm    # Access the DBH column as a vector

## Apply our custom functions to the inventory column
calc_cv(treeDatas$dbh_cm)  # Calculate CV for DBH column
treeDatas$ba_m2 <- calculate_basal_area(treeDatas$dbh_cm)  # Add a new column for basal area
head(treeDatas)  # Inspect the first few rows of the updated table

###1. logical subsetting with square brackets [rows, columns]

treeDatas$dbh_cm[treeDatas$dbh_cm > 30]  # Returns only the DBH values greater than 30 cm

##which row numbers has the largest diameter tree in the inventory?
which(treeDatas$dbh_cm), 1]






