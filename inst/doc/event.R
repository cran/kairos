## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = NULL
)

## ----packages-----------------------------------------------------------------
## Install extra packages (if needed):
# install.packages("folio") # datasets

# Load packages
library(kairos)

## ----event-model--------------------------------------------------------------
## Bellanger et al. did not publish the data supporting their demonstration: 
## no replication of their results is possible. 
## Here is an example using the Zuni dataset from Peeples and Schachner 2012
data("zuni", package = "folio")

## Assume that some assemblages are reliably dated (this is NOT a real example)
## The names of the vector entries must match the names of the assemblages
zuni_dates <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

## Model the event and accumulation date for each assemblage
model <- event(zuni, dates = zuni_dates, rank = 10)

## Extract model coefficients
## (convert results to Gregorian years)
coef(model, calendar = CE())

## Extract residual standard deviation
## (convert results to Gregorian years)
sigma(model, calendar = CE())

## Extract model residuals
## (convert results to Gregorian years)
resid(model, calendar = CE())

## Extract model fitted values
## (convert results to Gregorian years)
fitted(model, calendar = CE())

## ----event-predict------------------------------------------------------------
## Estimate event dates
eve <- predict_event(model, margin = 1, level = 0.95)
head(eve)

## Estimate accumulation dates (median)
acc <- predict_accumulation(model, level = 0.95)
head(acc)

## ----event-plot, fig.width=7, fig.height=7------------------------------------
## Activity plot
plot(model, type = "activity", event = TRUE, select = 1:6)
plot(model, type = "activity", event = TRUE, select = "LZ1105")

## Tempo plot
plot(model, type = "tempo", select = "LZ1105")

## ----event-refine, warning=FALSE----------------------------------------------
## Check model variability
## Jackknife fabrics
jack <- jackknife(model)
head(jack)

## Bootstrap of assemblages
boot <- bootstrap(model, n = 30)
head(boot)

