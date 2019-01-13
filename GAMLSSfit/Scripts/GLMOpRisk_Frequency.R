options(scipen = 999)
# Load packages
library(rattle, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(chron, quietly = TRUE)
library(dplyr, quietly = TRUE)

# Set parameter values
crv$seed <- 42 # set random seed
crv$taining.proportion <- 0.7 # proportion of data used for training
crv$validation.proportion <- 0.15 # proportion of data used for validation

# Load data
d <- read.csv("OPriskDataSet_exposure.csv",
              sep=";",
              dec=",",
              na.strings=c(".", "NA", "", "?"),
              strip.white=TRUE, encoding="UTF-8")

exposure <- d[,ncol(d)] 
class(exposure)
length(exposure)

summary(d)

d1 <- d %>%
  group_by(UpdatedDay,
           UpdatedTime,
           TradedDay,
           TradedTime,
           Desk,
           CapturedBy,
           TradeStatus,
           TraderId,
           Instrument,
           Reason,
           EventTypeCategoryLevel1,
           BusinessLineLevel1) %>%
  transmute(LossesIndicator = LossIndicator,
            Losses = Loss,
            exposure = exposure)

getmode <- function(x){
  u <- unique(x)
  as.integer(u[which.max(tabulate(match(x,u)))])
}

for (i in 5:(ncol(d1) - 3)){
     d1[[i]] <- relevel(d1[[i]], getmode(d1[[i]]))
}

### Let us fit a GLM to our data. This will be our global model. We will use "LossesIndicator" as the dependent
### variable, while the other variables will be predictor variables.

freqfit <- glm(LossesIndicator ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
           + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1,
           data=d1, family=poisson(link = 'log'), offset = log(exposure))
summary(freqfit)

names(freqfit)
names(freqfit$data)

predict(freqfit, d1[1:10, setdiff(names(d1), "LossesIndicator")])
freqfit$fitted.values[1:10]

exp(predict(freqfit, d1[1:10, setdiff(names(d1), "LossesIndicator")]))

### Load "MuMIn" package

require(MuMIn)

### Then, we use "dredge" function to generate models using combinations of the terms in the global model. The
### function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for
### finite sample sizes

options(na.action=na.fail)
freqfits <- dredge(freqfit)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will
### also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the
### function calculate the average model (or mean model) using all models.

summary(model.avg(get.models(freqfits, subset = TRUE)))

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

summary(model.avg(get.models(freqfits, subset=delta<2)))

### That's it! Now we have AICc values for our models and we have the average model (or mean model).
