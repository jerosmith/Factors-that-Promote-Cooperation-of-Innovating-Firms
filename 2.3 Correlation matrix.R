# CREATE CORRELATION MATRIX AND STORE IN EXCEL
# Helps to understand the variables
# Multicollinearity
# Instrumental variables
# 2 sec

# Packages
library(openxlsx)
library(sqldf)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/01 Metadata/"
path.database = "../Database/"
path.result = "../Results/"
file.metadata = "Metadata Thesis.xlsx"
file.database = "ENI Chile 2020.xlsx"
file.result = "Correlation Matrix.xlsx"

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 2. Select variables for correlation table
select.cols = df_variables[  !is.na(df_variables$Include.in.DB) 
                           & df_variables$Role.in.Dataset %in% c("Dependent", "Endogenous", "Exogenous") 
                           & df_variables$Data.Type == "Continuous"
                           & !(df_variables$Variable.R %in% c("Cooperation.Firms", "Cooperation.Universities")) # Exclude obviously correlated variables
                           & !grepl(".Scaled", df_variables$Variable.R)
                           , "Variable.R"]

df_data = df_data[, select.cols]

# 3. Create correlation table
df_corr = as.data.frame(cor(df_data, use = "complete.obs"))
df_corr = abs(df_corr)
df_corr = round(df_corr, 2)
n = nrow(df_corr)
for (i in 1:n){
  df_corr[i, i] = NA
}
names(df_corr) = gsub(".", " ", names(df_corr), fixed = T)
row.names(df_corr) = gsub(".", " ", row.names(df_corr), fixed = T)

# Save
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_corr, file = paste0(path.result, file.result), firstRow = T, rowNames = T, headerStyle = hs, colWidths = 9)

# Show time taken
print(Sys.time() - t0)

