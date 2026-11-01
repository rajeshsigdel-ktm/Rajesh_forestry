treeDatas <- read.csv("./DATA/RawDATA/bagmati_raw_inventory.csv")

treeDatas 
# Inspect the dataset
head(treeDatas)
nrow(treeDatas)       # Total tree stems
ncol(treeDatas)       # Number of attributes
names(treeDatas)      # Column names
str(treeDatas)        # Column types

# 2. Build a defensive basal area function
calculate_basal_area <- function(DBH_cm) {
  # Guard 1: Type check
  if (!is.numeric(DBH_cm)) {
    stop("Error: DBH must be numeric (measured in cm)!")
  }
  # Guard 2: Biologically impossible negative values
  if (any(DBH_cm < 0, na.rm = TRUE)) {
    stop("Error: DBH cannot be negative!")
  }
  # Guard 3: Warn on possible unit errors (e.g. mm recorded instead of cm)
  if (any(DBH_cm > 150, na.rm = TRUE)) {
    warning("Warning: DBH > 150 cm detected. Did you record in mm?")
  }
  
  ba_m2 <- pi * (DBH_cm^2) / 40000
  return(ba_m2)
}
valid <- !is.na(treeDatas$DBH_cm) &
  treeDatas$DBH_cm >= 0 &
  treeDatas$DBH_cm <= 150
calculate_basal_area(treeDatas$DBH_cm[valid])
treeDatas <- treeDatas[valid, ]
treeDatas$basal_area <- calculate_basal_area(treeDatas$DBH_cm)
head(treeDatas[c("Tree_ID", "DBH_cm", "basal_area")])
