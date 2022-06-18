## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
# Load packages
library(khroma) # Colour schemes
library(folio) # Datasets
library(dimensio) # Multivariate analysis
library(kairos)

## ----reciprocal, fig.width=3.5, fig.height=3.5, fig.show='hold'---------------
## Build an incidence matrix with random data
set.seed(12345)
incidence1 <- matrix(sample(0:1, 400, TRUE, c(0.6, 0.4)), nrow = 20)

## Get seriation order on rows and columns
## If no convergence is reached before the maximum number of iterations (100), 
## it stops with a warning.
(indices <- seriate_rank(incidence1, margin = c(1, 2), stop = 100))

## Permute matrix rows and columns
incidence2 <- permute(incidence1, indices)

## Plot matrix
heatmap(incidence1, Rowv = NA, Colv = NA, revC = TRUE, 
        scale = "none", col = c("white", "black"))
heatmap(incidence2, Rowv = NA, Colv = NA, revC = TRUE, 
        scale = "none", col = c("white", "black"))

## ----average, fig.width=7, fig.height=3.5-------------------------------------
## Replicates Desachy 2004 results
data("compiegne", package = "folio")

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(indices <- seriate_rank(compiegne, EPPM = TRUE, margin = 2))

## Permute columns
compiegne_permuted <- permute(compiegne, indices)

## ----ca-ford, fig.width=5, fig.height=5---------------------------------------
## Get data
data("zuni", package = "folio")

heatmap(as.matrix(zuni), Rowv = NA, Colv = NA, revC = TRUE)

## ----ca-seriation, fig.width=3.5, fig.height=3.5, results='hold'--------------
## Get row permutations from CA coordinates
(zun_indices <- seriate_average(zuni, margin = 1))

## Plot CA results
## Rows
dimensio::plot_rows(zun_indices) +
  ggplot2::labs(title = "Rows") +
  ggplot2::theme_bw()

## Columns
dimensio::plot_columns(zun_indices) +
  ggplot2::labs(title = "Columns") +
  ggplot2::theme_bw()

## ----ca-permutation, fig.width=5, fig.height=5--------------------------------
## Permute data matrix
zuni_permuted <- permute(zuni, zun_indices)

heatmap(as.matrix(zuni_permuted), Rowv = NA, Colv = NA, revC = TRUE)

## ----refine, fig.width=5, fig.height=4----------------------------------------
## Replicates Peeples and Schachner 2012 results
## Samples with convex hull maximum dimension length greater than the cutoff
## value will be marked for removal.
## Define cutoff as one standard deviation above the mean
fun <- function(x) { mean(x) + sd(x) }

## Partial bootstrap CA
## Warning: this may take a few seconds!
(zuni_boot <- dimensio::bootstrap(zun_indices, n = 1000))

## Plot bootstrap CA results for the columns
dimensio::plot_columns(zuni_boot) +
  ggplot2::labs(title = "Columns") +
  ggplot2::theme_bw()

(zuni_keep <- refine(zuni_boot, cutoff = fun, margin = 1))

## Plot convex hull
## blue: convex hull for samples; red: convex hull for types
### All bootstrap samples
rchull <- as.data.frame(zuni_keep[["hull"]])
ggplot2::ggplot(data = rchull) +
  ggplot2::aes(x = x, y = y, group = id) +
  ggplot2::geom_polygon(fill = "blue", alpha = 0.1) +
  ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  ggplot2::coord_fixed() + 
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw()

### Only retained samples
rchull_sub <- subset(rchull, id %in% zuni_keep[["keep"]])
ggplot2::ggplot(data = rchull_sub) +
  ggplot2::aes(x = x, y = y, group = id) +
  ggplot2::geom_polygon(fill = "blue", alpha = 0.1) +
  ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  ggplot2::coord_fixed() + 
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw()

## Histogram of convex hull maximum dimension length
hull_length <- data.frame(length = zuni_keep[["length"]])
ggplot2::ggplot(data = hull_length) +
  ggplot2::aes(x = length) +
  ggplot2::geom_histogram(breaks = seq(0, 4.5, by = 0.5), fill = "grey70") +
  ggplot2::geom_vline(xintercept = fun(hull_length$length), colour = "red") +
  ggplot2::labs(
    title = "Convex hull max. dim.", 
    x = "Maximum length", 
    y = "Count"
  ) + 
  ggplot2::theme_bw()

