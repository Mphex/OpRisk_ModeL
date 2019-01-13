
file_loc <- "C:/Users/User/Documents/OpRiskPHDGitHub/Exploratory_Data_Analysis"
setwd(file_loc)
list.files(file_loc)

frequency <- openxlsx::read.xlsx("Raw_Formatted_Data.xlsx", check.names = TRUE, sheet = "Frequency")
severity <- openxlsx::read.xlsx("Raw_Formatted_Data.xlsx", check.names = TRUE, sheet = "Severity")
projdata <- openxlsx::read.xlsx("OPriskDataSet_exposure.xlsx", check.names = TRUE, sheet = "CleanedData")

str(frequency)
str(severity)
str(projdata)

length(unique(frequency$Related.Trade))
length(unique(severity$Trd.Nbr))

length(intersect(frequency$Related.Trade, severity$Trd.Nbr))


names(frequency)
names(severity)
dput(names(projdata))

names(projdata)[names(projdata) %in% "EventTypeCategoryLevel1"] <- "EventTypeCategoryLevel"
names(projdata)[names(projdata) %in% "BusinessLineLevel1"] <- "BusinessLineLevel"
names(projdata) <- sub("\\.", "", names(projdata))
dput(names(projdata))


projdata[] <- lapply(projdata, function(x) if (is.character(x)) toupper(trimws(x)) else x)


# Update Time
summary(projdata$UpdatedTime)
hist(projdata$UpdatedTime, col = "navy", main = NULL, xlab = "Update Time", ylab = "Frequency")
hist(projdata$UpdatedTime[projdata$LossIndicator == 0], col = "navy", main = NULL, xlab = "Update Time", ylab = "Frequency")
hist(projdata$UpdatedTime[projdata$LossIndicator == 1], col = "navy", main = NULL, xlab = "Update Time", ylab = "Frequency")

plot(projdata$UpdatedTime, log(projdata$Loss+0.000000001), ylim = c(0, 20), col = "navy", xlab = "Updated Time", ylab = "Log. Loss")

do.call("rbind", lapply(split(projdata$Loss, projdata$UpdatedTime), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))


# Update Day
table(projdata$UpdatedDay)
hist(projdata$UpdatedDay)
hist(projdata$UpdatedDay[projdata$LossIndicator == 0])
hist(projdata$UpdatedDay[projdata$LossIndicator == 1])

plot(projdata$UpdatedDay, log(projdata$Loss+0.000000001), ylim = c(0, 20), col = "navy", xlab = "Updated Day", ylab = "Log. Loss")

do.call("rbind", lapply(split(projdata$Loss, projdata$UpdatedDay), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))


# Traded Day
data.frame(table(projdata$TradedDay)) 
hist(projdata$TradedDay)
plot(projdata$TradedDay, log(projdata$Loss+0.000000001), ylim = c(0, 20), col = "navy", xlab = "Traded Day", ylab = "Log. Loss")

do.call("rbind", lapply(split(projdata$Loss, projdata$TradedDay), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))


# Desk
table(projdata$Desk, projdata$LossIndicator)
addmargins(table(projdata$Desk, projdata$LossIndicator), 2)
round(addmargins(prop.table(table(projdata$Desk, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$Desk), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))



# Captured By
table(projdata$CapturedBy, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$CapturedBy, projdata$LossIndicator), 1), 2)*100, 1)
plot(table(projdata$LossIndicator, projdata$CapturedBy))

do.call("rbind", lapply(split(projdata$Loss, projdata$CapturedBy), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))



# Trader Status
table(projdata$TradeStatus, projdata$LossIndicator)

do.call("rbind", lapply(split(projdata$Loss, projdata$TradeStatus), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))


# Trader
unique(projdata$TraderId)
table(projdata$TraderId, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$TraderId, projdata$LossIndicator), 1), 2)*100, 1)

tapply(projdata$Loss, INDEX = projdata$TraderId, function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))

hist(projdata$Loss[projdata$TraderId == "ANALYST"])

library("lattice")

xyplot(Loss ~ as.factor(TraderId) , data = projdata)

do.call("rbind", lapply(split(projdata$Loss, projdata$TraderId), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))


# Instrument
unique(projdata$Instrument)

table(projdata$Instrument, projdata$LossIndicator)

plot(table(projdata$Instrument, projdata$LossIndicator))

round(addmargins(prop.table(table(projdata$Instrument, projdata$LossIndicator), 1), 2)*100, 1)


pander::pander(tapply(projdata$Loss, INDEX = projdata$Instrument, function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

tablex <- do.call("rbind", lapply(split(projdata$Loss, projdata$Instrument), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))
tablex <- cbind(Instrument = rownames(tablex), tablex)
tablex <- as.data.frame(tablex)
tablex$Mean <- as.numeric(as.character(tablex$Mean))

tablex <- tablex[order(tablex$Mean), ]

stargazer::stargazer(tablex)

openxlsx::write.xlsx(tablex, "tablex.xlsx", rownames = TRUE)

# Reason
unique(projdata$Reason)

# Loss
sum(projdata$Loss)
sum(projdata$Loss[projdata$LossIndicator == 0])
sum(projdata$Loss[projdata$LossIndicator == 1])

# Event Type
table(projdata$EventTypeCategoryLevel, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$EventTypeCategoryLevel, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$EventTypeCategoryLevel), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

# Business Line Level
table(projdata$BusinessLineLevel, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$BusinessLineLevel, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$BusinessLineLevel), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

is_missing <- is.na(projdata$LastResetRate)
sum(is_missing)
plot(log(projdata$Loss[!is_missing]+0.000001), projdata$LastResetRate[!is_missing])

data.frame(table(projdata$LastResetRate))

# Theta
plot(projdata$UpdatedTime[projdata$LossIndicator == 0], projdata$Theta[projdata$LossIndicator == 0])

plot(projdata$UpdatedTime[projdata$LossIndicator == 1], projdata$Theta[projdata$LossIndicator == 1])

projdata$Theta[projdata$LossIndicator == 0]
projdata$Theta[projdata$LossIndicator == 1]

plot(projdata$UpdatedDay[projdata$LossIndicator == 0], projdata$Theta[projdata$LossIndicator == 0])

plot(projdata$UpdatedDay[projdata$LossIndicator == 1], projdata$Theta[projdata$LossIndicator == 1])





