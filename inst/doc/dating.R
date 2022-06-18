## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = NULL
)

## ----packages-----------------------------------------------------------------
## Load packages
library(kairos)
library(folio) # Datasets

## ----mcd-model, fig.width=7, fig.height=3.5, fig.align="center"---------------
## Coerce the zuni dataset to an abundance (count) matrix
data("zuni", package = "folio")

## Set the start and end dates for each ceramic type
zuni_dates <- list(
  LINO = c(600, 875), KIAT = c(850, 950), RED = c(900, 1050), 
  GALL = c(1025, 1125), ESC = c(1050, 1150), PUBW = c(1050, 1150),
  RES = c(1000, 1200), TULA = c(1175, 1300), PINE = c(1275, 1350),
  PUBR = c(1000, 1200), WING = c(1100, 1200), WIPO = c(1125, 1225),
  SJ = c(1200, 1300), LSJ = c(1250, 1300), SPR = c(1250, 1300),
  PINER = c(1275, 1325), HESH = c(1275, 1450), KWAK = c(1275, 1450)
)

## Calculate date midpoint
zuni_mid <- vapply(X = zuni_dates, FUN = mean, FUN.VALUE = numeric(1))

## Calculate MCD
zuni_mcd <- mcd(zuni, dates = zuni_mid)
head(zuni_mcd)

plot(zuni_mcd, select = 100:125)

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

