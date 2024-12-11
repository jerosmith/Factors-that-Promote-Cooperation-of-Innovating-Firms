# QUALITY CONTROL CHECK OF SOURCE DATA

# Clear memory
rm(list = objects())

# Parameters
path.source = "../Data/03 csv from Source/"
csv.source = "ENI 10 sector.csv"

# Load data
df_data = read.csv2(paste0(path.source, csv.source))

# Check SECTOR_ACTIVIDAD
unique(df_data$SECTOR_ACTIVIDAD)
# Just codes. OK for dummy variables but not for descriptive graphs.

# 2. Check consistency of P3162 (Cooperates yes/no)
sum(is.na(df_data$P3162)) # 4547
sum(df_data$P3162, na.rm = T) # 323
df_data[is.na(df_data$P3162), "P3162"] = 0
sum(df_data$P3162) # 323
dependent.columns = c("P3164", "P3166", "P3168", "P3170", "P3172", "P3174", "P3176", "P3178", "P3180", "P3182", "P3184", "P3186", "P3188", "P3190")
for (c in dependent.columns){
  df_data[, c] = ifelse(is.na(df_data[, c]), 0, df_data[, c])
}
df_data$P3162.Check = apply(df_data[, dependent.columns], MARGIN = 1, FUN = sum)
sum(df_data$P3162.Check)
# Conclusion: P3162 is inconsistent with its dependent columns. DO NOT USE IT.

# 3. Check completeness of variables for Incoming Spillovers: P3139 and similar
# 3.1 Count NAs of main variables
sum(is.na(df_data$P3139)) # 5232
sum(is.na(df_data$P3142)) # 5290
sum(is.na(df_data$P3145)) # ...etc. all big
sum(is.na(df_data$P3281))
sum(is.na(df_data$P3154))
sum(is.na(df_data$P3157))
sum(is.na(df_data$P3332))
sum(is.na(df_data$P3333))
sum(is.na(df_data$P3334))
sum(is.na(df_data$P3335))
# 3.2 See if NAs can be replaced by 1 from associated binary variables
df_data[is.na(df_data$P3139) & !is.na(df_data$P4147) & df_data$P4147==1, c("P3139", "P4147")] # 0 missing values in P3139 can be recovered by value 1 in P4147.
df_data[is.na(df_data$P3142) & !is.na(df_data$P4148) & df_data$P4148==1, c("P3142", "P4148")] # 0 missing values in P3139 can be recovered by value 1 in P4147.
df_data[is.na(df_data$P3145) & !is.na(df_data$P4149) & df_data$P4149==1, c("P3145", "P4149")] # 0 missing values in P3145 can be recovered by value 1 in P4149.
df_data[is.na(df_data$P3281) & !is.na(df_data$P4150) & df_data$P4150==1, c("P3281", "P4150")] # 0 missing values in P3281 can be recovered by value 1 in P4150.
df_data[is.na(df_data$P3154) & !is.na(df_data$P4151) & df_data$P4151==1, c("P3154", "P4151")] # 0 missing values in P3154 can be recovered by value 1 in P4151.
df_data[is.na(df_data$P3157) & !is.na(df_data$P4152) & df_data$P4152==1, c("P3157", "P4152")] # 0 missing values in P3157 can be recovered by value 1 in P4152.
df_data[is.na(df_data$P3332) & !is.na(df_data$P4153) & df_data$P4153==1, c("P3332", "P4153")] # 0 missing values in P3332 can be recovered by value 1 in P4153.
df_data[is.na(df_data$P3333) & !is.na(df_data$P4154) & df_data$P4154==1, c("P3333", "P4154")] # 0 missing values in P3333 can be recovered by value 1 in P4154.
df_data[is.na(df_data$P3334) & !is.na(df_data$P4155) & df_data$P4155==1, c("P3334", "P4155")] # 0 missing values in P3334 can be recovered by value 1 in P4155.
df_data[is.na(df_data$P3335) & !is.na(df_data$P4156) & df_data$P4156==1, c("P3335", "P4156")] # 0 missing values in P3335 can be recovered by value 1 in P4156.
# Conclusion: No valuation of incoming spillover variables can have their NAs saved by their preceding binaries, 
# so there is no point in including the latter.

# 4. Check completeness of variables for Appropriation
# 4.1 Count NAs of main variables
sum(is.na(df_data$P4059))
sum(is.na(df_data$P4163))
sum(is.na(df_data$P4062))
sum(is.na(df_data$P4164))
sum(is.na(df_data$P4065))
sum(is.na(df_data$P4165))
sum(is.na(df_data$P4068))
sum(is.na(df_data$P4166))
sum(is.na(df_data$P4071))
sum(is.na(df_data$P4167))
sum(is.na(df_data$P4074))
sum(is.na(df_data$P4168))
sum(is.na(df_data$P4157)) # 0
sum(is.na(df_data$P4158)) # 0
sum(is.na(df_data$P4159)) # 0
sum(is.na(df_data$P4160)) # 0
sum(is.na(df_data$P4161)) # 0
sum(is.na(df_data$P4162)) # 0
sum(is.na(df_data$P4113)) # 5665,,,etc.
sum(is.na(df_data$P4169))
sum(is.na(df_data$P4170))
sum(is.na(df_data$P4171))
sum(is.na(df_data$P4172))
sum(is.na(df_data$P4173))
sum(is.na(df_data$P4174))
# 4.2 Can NAs of main variables be saved by 1s of binary variables? NO
sum(is.na(df_data$P4059) & !is.na(df_data$P4157) & df_data$P4157==1)
sum(is.na(df_data$P4163) & !is.na(df_data$P4157) & df_data$P4157==1)
sum(is.na(df_data$P4062) & !is.na(df_data$P4158) & df_data$P4158==1)
sum(is.na(df_data$P4164) & !is.na(df_data$P4158) & df_data$P4158==1)
sum(is.na(df_data$P4065) & !is.na(df_data$P4159) & df_data$P4159==1)
sum(is.na(df_data$P4165) & !is.na(df_data$P4159) & df_data$P4159==1)
sum(is.na(df_data$P4068) & !is.na(df_data$P4160) & df_data$P4160==1)
sum(is.na(df_data$P4166) & !is.na(df_data$P4160) & df_data$P4160==1)
sum(is.na(df_data$P4071) & !is.na(df_data$P4161) & df_data$P4161==1)
sum(is.na(df_data$P4167) & !is.na(df_data$P4161) & df_data$P4161==1)
sum(is.na(df_data$P4074) & !is.na(df_data$P4162) & df_data$P4162==1)
sum(is.na(df_data$P4168) & !is.na(df_data$P4162) & df_data$P4162==1)
# 4.3 Mean of main variables
mean(df_data$P4059, na.rm = T)
mean(df_data$P4163, na.rm = T)
mean(df_data$P4062, na.rm = T)
mean(df_data$P4164, na.rm = T)
mean(df_data$P4065, na.rm = T)
mean(df_data$P4165, na.rm = T)
mean(df_data$P4068, na.rm = T)
mean(df_data$P4166, na.rm = T)
mean(df_data$P4071, na.rm = T)
mean(df_data$P4167, na.rm = T)
mean(df_data$P4074, na.rm = T)
mean(df_data$P4168, na.rm = T)
# 4.4 Inspect data
df_data$P4059[!is.na(df_data$P4059)]
df_data$P4163[!is.na(df_data$P4163)]
df_data$P4062[!is.na(df_data$P4062)]
df_data$P4164[!is.na(df_data$P4164)]
df_data$P4065[!is.na(df_data$P4065)]
df_data$P4165[!is.na(df_data$P4165)]
df_data$P4068[!is.na(df_data$P4068)]
df_data$P4166[!is.na(df_data$P4166)]
df_data$P4071[!is.na(df_data$P4071)]
df_data$P4167[!is.na(df_data$P4167)]
df_data$P4074[!is.na(df_data$P4074)]
df_data$P4168[!is.na(df_data$P4168)]
# 4.5 Mean of non-zero values of main variables
mean(df_data$P4059[!is.na(df_data$P4059) & df_data$P4059 != 0])
mean(df_data$P4163[!is.na(df_data$P4163) & df_data$P4163 != 0])
mean(df_data$P4062[!is.na(df_data$P4062) & df_data$P4062 != 0])
mean(df_data$P4164[!is.na(df_data$P4164) & df_data$P4164 != 0])
mean(df_data$P4065[!is.na(df_data$P4065) & df_data$P4065 != 0])
mean(df_data$P4165[!is.na(df_data$P4165) & df_data$P4165 != 0])
mean(df_data$P4068[!is.na(df_data$P4068) & df_data$P4068 != 0])
mean(df_data$P4166[!is.na(df_data$P4166) & df_data$P4166 != 0])
mean(df_data$P4071[!is.na(df_data$P4071) & df_data$P4071 != 0])
mean(df_data$P4167[!is.na(df_data$P4167) & df_data$P4167 != 0])
mean(df_data$P4074[!is.na(df_data$P4074) & df_data$P4074 != 0])
mean(df_data$P4168[!is.na(df_data$P4168) & df_data$P4168 != 0])

# 5. Check completeness of variables for Social Innovation
# 5.1 Count NAs and values of all variables
sum(is.na(df_data$P4000)) # 0
sum(df_data$P4000) # 135
sum(is.na(df_data$P4002)) # 5741
sum(is.na(df_data$P4003)) # 5741
# 5.2 See if NAs can be replaced by 1 from associated binary variables
df_data[is.na(df_data$P4002) & df_data$P4000==1, c("P4000", "P4002")] # 0 missing values in P4002 can be recovered by value 1 in P000.
df_data[is.na(df_data$P4003) & df_data$P4000==1, c("P4000", "P4003")] # 0 missing values in P4003 can be recovered by value 1 in P000.

