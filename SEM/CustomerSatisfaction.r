# Load necessary libraries
library(readr)    # For reading CSV files
library(readxl)   # For reading Excel files
library(lavaan)   # For SEM modeling
library(semPlot)  # For SEM visualization

# Read data from Excel file
data <- read_excel("Customer_Data.xlsx")
head(data)

# Define the SEM model
model <- '
  # Measurement model
  Perceived_Useful =~ Trust + Social_Influence + Innovativeness 
  Perceived_Ease =~ Mobility + Perceived_Enjoyment + Involvement
  Customer_Satisf =~ Perceived_Usefulness + Perceived_Easeofuse
  Loyalt =~ Customer_Satisfaction
'

# Fit the SEM model
fit <- sem(model, data = data)

# Create a color vector for each variable based on their type
var_labels <- c("Perceived_Usefulness", "Perceived_Easeofuse", "Customer_Satisfaction", "Loyalty",
                "Trust", "Social_Influence", "Innovativeness", "Mobility", "Perceived_Enjoyment", "Involvement")

# Visualize the SEM model with colored nodes
semPaths(fit,
         color = "darkblue",
         theme="colorblind",
         whatLabels = "std",
         style = "lisrel",
         sizeLat = 10,
         sizeLat2 = 10,
         sizeMan = 7,
         edge.color = "blue",
         edge.label.cex = 1.5,
         rotation = 2,
         layout = "tree2",
         intercepts = T,
         residuals = T,
         curve = 2,
         title = T,
         title.color = "black",
         cardinal = "lat cov",
         residScale = 15,
         curvePivot = T,
         mar = c(2,5,2,5.5))

# Extract specific fit indices
fit_indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# Print the fit indices
print(fit_indices)

# Extract path coefficients and standardized estimates
path_coeffs <- parameterEstimates(fit, standardized = TRUE)

# Filter to display only structural (regression) paths
structural_paths <- path_coeffs[path_coeffs$op == "~", ]

# Display custom output for path coefficients
print(structural_paths)
