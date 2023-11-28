

# Subset the numeric columns using is.numeric()
numeric_columns <- SIMULATION[, sapply(SIMULATION, is.numeric)]

# Use colSums() to calculate the sum of each numeric column
column_sums <- colSums(numeric_columns)


# Counting industries
length(unique(USE_K_DOM_NETPURCH_0$INDUSTRY_NAME))
length(unique(NACE_INDUSTRIES_0$Est.IS$INDUSTRY_NAME))


length(unique(EST_REV_0$PRODUCT_INDUSTRY_NAME))
length(unique(SIMULATION_0$PRODUCT_INDUSTRY_CODE))

