## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
## Install extra packages (if needed):
## - folio: datasets
## - tabula: visualization
# install.packages(c("folio", "tabula"))

# Load packages
library(kairos)

## ----reciprocal, fig.width=3.5, fig.height=3.5, out.width="50%", fig.show='hold'----
## Build an incidence matrix with random data
set.seed(12345)
bin <- sample(c(TRUE, FALSE), 400, TRUE, c(0.6, 0.4))
incidence1 <- matrix(bin, nrow = 20)

## Get seriation order on rows and columns
## If no convergence is reached before the maximum number of iterations (100), 
## it stops with a warning.
(indices <- seriate_rank(incidence1, margin = c(1, 2), stop = 100))

## Permute matrix rows and columns
incidence2 <- permute(incidence1, indices)

## Plot matrix
tabula::plot_heatmap(incidence1, col = c("white", "black"))
tabula::plot_heatmap(incidence2, col = c("white", "black"))

## ----average, fig.width=7, fig.height=3.5-------------------------------------
## Replicates Desachy 2004
data("compiegne", package = "folio")

## Plot frequencies and EPPM values
tabula::seriograph(compiegne)

## Get seriation order for columns on EPPM using the reciprocal ranking method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(indices <- seriate_rank(compiegne, EPPM = TRUE, margin = 2))

## Permute columns
compiegne_permuted <- permute(compiegne, indices)

## Plot frequencies and EPPM values
tabula::seriograph(compiegne_permuted)

## ----ca-ford, fig.width=7, fig.height=7---------------------------------------
## Data from Peeples and Schachner 2012
data("zuni", package = "folio")

## Ford diagram
par(cex.axis = 0.7)
tabula::plot_ford(zuni)

## ----ca-seriation, fig.width=7, fig.height=7----------------------------------
## Get row permutations from CA coordinates
(zun_indices <- seriate_average(zuni, margin = c(1, 2)))

## Plot CA results
biplot(zun_indices)

## ----ca-permutation, fig.width=7, fig.height=7--------------------------------
## Permute data matrix
zuni_permuted <- permute(zuni, zun_indices)

## Ford diagram
par(cex.axis = 0.7)
tabula::plot_ford(zuni_permuted)

## ----bootstrap, fig.width=7, fig.height=7, out.width='50%', fig.show='hold'----
## Partial bootstrap CA
## Warning: this may take a few seconds!
zuni_boot <- bootstrap(zun_indices, n = 30)

## Bootstrap CA results for the rows
## (add convex hull)
zuni_boot |> 
  viz_rows(col = "lightgrey", pch = 16, legend = NULL) |> 
  viz_hull(col = adjustcolor("#004488", alpha = 0.2))

## Bootstrap CA results for the columns
zuni_boot |> 
  viz_columns(pch = 16, color = NULL, legend = list(x = "topright"))

## ----refine-------------------------------------------------------------------
## Replicates Peeples and Schachner 2012 results
## Define cutoff as one standard deviation above the mean
fun <- function(x) { mean(x) + sd(x) }

## Samples with convex hull maximum dimension length greater than the cutoff
## value will be marked for removal.
zuni_refine <- refine(zuni_boot, cutoff = fun, margin = 1)

## ----refine-histogram, fig.width=5, fig.height=5------------------------------
## Histogram of convex hull maximum dimension length
hist(zuni_refine$length, xlab = "Maximum length", main = NULL)
abline(v = zuni_refine$cutoff, col = "red", lty = 2) # Cutoff value

## ----refine-permutation, fig.width=7, fig.height=7----------------------------
## Compute seriation with supplementary rows
zuni_supp <- seriate_average(zuni, margin = c(1, 2), sup_row = zuni_refine$exclude)

## Plot CA results
## (highlight supplementary rows)
viz_rows(zuni_supp, extra_quali = "observation", symbol = c(16, 15))

## ----refine-ford, fig.width=7, fig.height=7-----------------------------------
## Permute data matrix
zuni_permuted2 <- permute(zuni, zuni_supp)

## Ford diagram
tabula::plot_ford(zuni_permuted2)

