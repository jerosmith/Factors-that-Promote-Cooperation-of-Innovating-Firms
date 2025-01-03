# GENERATE DATABASE
# 12 sec

# Packages
library(openxlsx)
library(sqldf)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/01 Metadata/"
path.source = "../Data/02 Source/"
path.master = "../Data/03 Master Tables/"
path.database = "../Database/"
file.metadata = "Metadata Thesis.xlsx"
file.master = "Master Tables.xlsx"
file.source = "2019-2020-ENI-base-de-datos-sector-economico.xlsx"
file.innovating.firms = "ENI Chile 2020.xlsx"

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
df_industry = read.xlsx(paste0(path.master, file.master), sheet = "Industry")[, c("Industry.ID", "Short.Name")]
columns = df_variables[df_variables$Source=="Data", "Variable.ID"]
df_data = read.xlsx(paste0(path.source, file.source))[, columns]
df_data = as.data.frame(df_data)

# 2. Convert all numeric columns to numeric
columns.numeric = df_variables[df_variables$Data.Type!="Categorical" & df_variables$Source=="Data", "Variable.ID"]
for (c in columns.numeric){
  if (class(df_data[, c])=="character"){
    print(c)
    df_data[, c] = gsub(",", ".", df_data[, c])
    df_data[, c] = as.numeric(df_data[, c])
  }
}

# 3. For all columns with Replace.NA not null, replace NAs with Replace.NA
print("For variables with Replace.NA not null replace NAs with Replace.NA...")
columns.replace.NA = df_variables[df_variables$Source=="Data" & !is.na(df_variables$Replace.NA), "Variable.ID"]
for (c in columns.replace.NA){
  print(c)
  value = df_variables[!is.na(df_variables$Variable.ID) & df_variables$Variable.ID==c, "Replace.NA"]
  df_data[, c] = replace(df_data[, c], list = is.na(df_data[, c]), values = value)
}

# 4. Create Formula 1 calculated variables
print("Creating level 1 calculated variables...")
df_formula = df_variables[df_variables$Source=="Formula 1", c("Variable.R", "Formula")]
nf = nrow(df_formula)
for (i in 1:nf){
  print(paste0(round(i/nf*100,0), "%"))
  Rtxt = paste0("df_data$", df_formula[i, "Variable.R"], " = ", df_formula[i, "Formula"])
  eval(parse(text = Rtxt))
}

# 5. Create Formula 2 calculated variables
print("Creating level 2 calculated variables...")
df_formula = df_variables[df_variables$Source=="Formula 2", c("Variable.R", "Formula")]
nf = nrow(df_formula)
for (i in 1:nf){
  print(paste0(round(i/nf*100,0), "%"))
  Rtxt = paste0("df_data$", df_formula[i, "Variable.R"], " = ", df_formula[i, "Formula"])
  eval(parse(text = Rtxt))
}

# 6. Keep innovative firms only
# df_data = df_data[df_data$Innovation > 0, ]

# 7. Add industry-level aggregated variables (Formula 3)
agg.variables = df_variables[!is.na(df_variables$Group.By), "Variable.R"]
agg.formulas = df_variables[!is.na(df_variables$Group.By), "Formula"]
sqltxt = paste0(agg.formulas, " as [", agg.variables, "]", collapse = ", ")
sqltxt = paste("select [COD_ACTIVIDAD],", sqltxt, "from df_data group by [COD_ACTIVIDAD]")
df_aggregated = sqldf(sqltxt)
df_data = merge(df_data, df_aggregated, all.x = T)

# 8. Create industry dummy variables
df_data = merge(df_data, df_industry, by.x = "COD_ACTIVIDAD", by.y = "Industry.ID", all.x = T)
df_data$Short.Name = gsub(" ", ".", df_data$Short.Name, fixed = T)
dummies = unique(df_data$Short.Name)
for (d in dummies){
  print(d)
  Rtxt = paste0("df_data$", d, " = ifelse(df_data$Short.Name == '", d, "', 1, 0)")
  eval(parse(text = Rtxt))
}

# 9. Keep only columns that have been defined to include in DB
column.id = df_variables[!is.na(df_variables$Include.in.DB), "Variable.R"]
df_data = df_data[, column.id]

# 10. Format and save to database
names(df_data) = gsub(".", " ", names(df_data), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_data, file = paste0(path.database, file.innovating.firms), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)
