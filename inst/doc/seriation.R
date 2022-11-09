## ----setup, include = FALSE, echo=FALSE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
## Install extra packages:
## - khroma: colour schemes
## - folio: datasets
## - tabula: visualization
# install.packages(c("khroma", "folio", "tabula"))

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
tabula::plot_heatmap(incidence1) +
  khroma::scale_fill_logical()
tabula::plot_heatmap(incidence2) +
  khroma::scale_fill_logical()

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

## ----ca-ford, fig.width=5, fig.height=5---------------------------------------
## Get data
data("zuni", package = "folio")

## Ford diagram
tabula::plot_ford(zuni) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank())

## ----ca-seriation, fig.width=5, fig.height=5----------------------------------
## Get row permutations from CA coordinates
(zun_indices <- seriate_average(zuni, margin = c(1, 2)))

## Plot CA results
dimensio::plot_rows(zun_indices) +
  ggplot2::labs(title = "Rows") +
  ggplot2::theme_bw()

## ----ca-permutation, fig.width=5, fig.height=5--------------------------------
## Permute data matrix
zuni_permuted <- permute(zuni, zun_indices)

## Ford diagram
tabula::plot_ford(zuni_permuted) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank())

## ----bootstrap, warning=FALSE,fig.width=5, fig.height=5, out.width='50%', fig.show='hold'----
## Partial bootstrap CA
## Warning: this may take a few seconds!
zuni_boot <- dimensio::bootstrap(zun_indices, n = 30)

## Plot convex hull
## Bootstrap CA results for the rows
zuni_boot |> 
  dimensio::plot_rows(active = FALSE, colour = "obs", fill = "obs") +
  dimensio::stat_hull(alpha = 0.2) +
  ggplot2::labs(title = "Rows") +
  ggplot2::theme_bw() +
  khroma::scale_colour_highcontrast() +
  khroma::scale_fill_highcontrast()

## Bootstrap CA results for the columns
zuni_boot |> 
  dimensio::plot_columns(colour = "group", fill = "group") +
  dimensio::stat_hull(alpha = 0.5) +
  ggplot2::labs(title = "Columns") +
  ggplot2::theme_bw() +
  khroma::scale_colour_discreterainbow() +
  khroma::scale_fill_discreterainbow()

## ----refine, fig.width=5, fig.height=5----------------------------------------
## Replicates Peeples and Schachner 2012 results
## Samples with convex hull maximum dimension length greater than the cutoff
## value will be marked for removal.
## Define cutoff as one standard deviation above the mean
fun <- function(x) { mean(x) + sd(x) }
(zuni_refine <- seriate_refine(zun_indices, cutoff = fun, margin = 1))

## Plot CA results for the rows
dimensio::plot_rows(zuni_refine, colour = "observation", fill = "observation") +
  ggplot2::labs(title = "Rows") +
  ggplot2::theme_bw() +
  khroma::scale_colour_discreterainbow() +
  khroma::scale_fill_discreterainbow()

## ----refine-histogram, fig.width=5, fig.height=3.5----------------------------
## Histogram of convex hull maximum dimension length
hull_length <- data.frame(length = zuni_refine[["length"]])
ggplot2::ggplot(data = hull_length) +
  ggplot2::aes(x = length) +
  ggplot2::geom_histogram(breaks = seq(0, 4.5, by = 0.5), fill = "grey70") +
  ggplot2::geom_vline(xintercept = zuni_refine[["cutoff"]], colour = "red") +
  ggplot2::labs(
    title = "Convex hull max. dim.", 
    x = "Maximum length", 
    y = "Count"
  ) + 
  ggplot2::theme_bw()

## ----refine-permutation, fig.width=5, fig.height=5----------------------------
## Permute data matrix
zuni_permuted2 <- permute(zuni, zuni_refine)

## Ford diagram
tabula::plot_ford(zuni_permuted2) +
  ggplot2::theme(axis.text.y = ggplot2::element_blank())

