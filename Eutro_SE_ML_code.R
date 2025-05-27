# Load required libraries
library(readxl)
library(dplyr)
library(ranger)

# ----- Load covariate data from Excel -----
# Reading soil, elevation, and nutrient data (sheet 5)
Soil_data_ele <- read_excel("C:/Users/surya/Documents/EUtro_new_data/SOil_elev_data_lakes.xlsx", sheet = 5)
colnames(Soil_data_ele)
# Columns: "Hylak_id", "Phos", "Nitro", "Temp", "Slope", "Elevation"

# ----- Load erosion and eutrophication area data -----
files12 <- read.csv("C:/Users/surya/Documents/EUtro_new_data/Other_countries/UK_0_02_100m.csv")

# Summarize area and erosion values by lake ID
files23 <- files12 %>%
  group_by(Hylak_id) %>%
  summarise(Area11 = sum(Area11, na.rm = TRUE))

files22 <- files12 %>%
  group_by(Hylak_id) %>%
  summarise(SUM = mean(SUM, na.rm = TRUE))

# Merge summarized data
files12 <- merge(files23, files22, by = "Hylak_id")
colnames(files12)

# Log-transform erosion and eutrophication area
files12$Log_Sum <- log10(files12$SUM)
files12$Log_Eu_Area <- log10(files12$Area11)

# Merge with covariate data
files <- merge(files12, Soil_data_ele, by = "Hylak_id")
files$Log_Sum <- as.numeric(as.character(files$Log_Sum))
files <- files[!is.na(files$Log_Sum),]

# ----- Select variables for modeling -----
I.vars <- make.names(unique(unlist(sapply(
  c("Log_Sum", "Nitro", "Phos", "Temp", "Elevation", "Slope"),
  function(i) { names(files)[grep(i, names(files))] }
))))

t.vars <- c("Log_Eu_Area")
sel.n <- c(t.vars, I.vars)
sel.r <- complete.cases(files[, sel.n])
Lakes_UK <- files[sel.r, sel.n]
Lakes_UK23 <- Lakes_UK[, c("Log_Sum", "Nitro", "Phos", "Temp", "Elevation", "Slope")]

# ----- Run Random Forest model -----
set.seed(2)
fm.lake <- as.formula(paste("Log_Eu_Area ~", paste(names(Lakes_UK23), collapse = " + ")))
rm.lake <- Lakes_UK[complete.cases(Lakes_UK[, all.vars(fm.lake)]), ]

m.lake <- ranger(fm.lake, rm.lake, num.trees = 200, mtry = 2, quantreg = TRUE, importance = "impurity")
print(m.lake)

# ----- Extract and display variable importance -----
xl <- as.list(ranger::importance(m.lake))
vv <- t(data.frame(xl[order(unlist(xl), decreasing = TRUE)[6:1]]))
print(vv)
