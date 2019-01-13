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
D <- read.csv("Sev_OPriskDataSet_exposure.csv",
              sep=";",
              dec=",",
              na.strings=c(".", "NA", "", "?"),
              strip.white=TRUE, encoding="UTF-8")

exposure <- D[,ncol(D)] 
class(exposure)
length(exposure)

summary(D)

D1 <- D %>%
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

for (i in 5:(ncol(D1) - 3)){
  D1[[i]] <- relevel(D1[[i]], getmode(D1[[i]]))
}

### Let us fit a GLM to our data. This will be our global model. We will use "LossesIndicator" as the dependent
### variable, while the other variables will be predictor variables.

severfit <- glm(Losses ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1,
               data=D1, family=Gamma(link = 'log'), offset = log(exposure))
summary(severfit)

### Load "MuMIn" package

require(MuMIn)

### Then, we use "dredge" function to generate models using combinations of the terms in the global model. The
### function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for
### finite sample sizes


options(na.action=na.fail)
severfits <- dredge(severfit)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will
### also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the
### function calculate the average model (or mean model) using all models.

# summary(model.avg(get.models(severfits, subset = TRUE)))

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

summary(model.avg(get.models(severfits, subset=delta<2)))

### That's it! Now we have AICc values for our models and we have the average model (or mean model).