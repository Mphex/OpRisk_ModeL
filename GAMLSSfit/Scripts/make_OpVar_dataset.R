
rm(list  = ls())
library("dplyr")
file_dir <- "C:/Users/User/Documents/GLMfit/GLMfit/"
opdata <- list.files(path = file_dir, pattern  = "^OPriskDataSet_exposure.xlsx$", full.names = TRUE)
opdata <- openxlsx::read.xlsx(opdata, sheet = "CleanedData")
str(opdata)

opdata$UpdateTime <- as.POSIXct(opdata$UpdateTime*60*60*24, origin = "1899-12-30", tz = "UTC")
opdata$TradeTime <- as.POSIXct(opdata$TradeTime*60*60*24, origin = "1899-12-30", tz = "UTC")
opdata$UpdateDate <- substr(opdata$UpdateTime, 1, 10)


opdata_reshape <- data.frame(UpdateDate = unique(opdata$UpdateDate),
                            BL5.Frequency = NA, BL3.Frequency = NA, BL4.Frequency = NA, 
                            BL2.Frequency = NA, BL6.Frequency = NA, BL1.Frequency = NA, 
                            BL9.Frequency = NA, BL7.Frequency = NA,
                            BL5.Severity = NA, BL3.Severity = NA, BL4.Severity = NA, 
                            BL2.Severity = NA, BL6.Severity = NA, BL1.Severity = NA, 
                            BL9.Severity = NA, BL7.Severity = NA)
opdata_reshape <- opdata_reshape[, union("UpdateDate", sort(names(opdata_reshape)))]

for ( freq in unique(opdata$BusinessLineLevel1)) {
  for (date in opdata_reshape$UpdateDate) {
    opdata_reshape[opdata_reshape$UpdateDate %in% date, paste0(freq, ".Frequency")] <- sum(opdata$LossIndicator[opdata$UpdateDate %in% date & opdata$BusinessLineLevel1 %in% freq], na.rm = TRUE)
    opdata_reshape[opdata_reshape$UpdateDate %in% date, paste0(freq, ".Severity")] <- sum(opdata$Loss[opdata$UpdateDate %in% date & opdata$BusinessLineLevel1 %in% freq], na.rm = TRUE)
  }
}


saveRDS(opdata_reshape, "C:/Users/User/Documents/GLMfit/GLMfit/OpVar_Reshape.RDS")
test <- readRDS( "C:/Users/User/Documents/GLMfit/GLMfit/OpVar_Reshape.RDS")

openxlsx::write.xlsx(opdata_reshape, "C:/Users/User/Documents/GLMfit/GLMfit/OpVar_Reshape.xlsx")





