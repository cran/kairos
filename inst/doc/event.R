## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = NULL
)

## ----packages-----------------------------------------------------------------
# Load packages
library(folio) # Datasets
library(kairos)

## ----event-model--------------------------------------------------------------
## Bellanger et al. did not publish the data supporting their demonstration: 
## no replication of their results is possible. 
## Here is a pseudo-reproduction using the zuni dataset

## Assume that some assemblages are reliably dated (this is NOT a real example)
## The names of the vector entries must match the names of the assemblages
zuni_dates <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

## Model the event and accumulation date for each assemblage
model <- event(zuni, dates = zuni_dates, cutoff = 90)
summary(get_model(model))

## Estimate event dates
event <- predict_event(model, margin = 1, level = 0.95)
head(event)

## Estimate accumulation dates
acc <- predict_accumulation(model)
head(acc)

## ----event-plot---------------------------------------------------------------
## Activity plot
plot(model, type = "activity", event = TRUE, select = "LZ1105")

## Tempo plot
plot(model, type = "tempo", select = "LZ1105")

## ----event-refine, warning=FALSE----------------------------------------------
## Check model variability
## Warning: this may take a few seconds

## Jackknife fabrics
jack <- jackknife(model)
head(jack)

## Bootstrap of assemblages
boot <- bootstrap(model, n = 30)
head(boot)

