# Adjacency indicator

Identifies adjacent predictors.

## Usage

``` r
.is_adjacent(group, j, p, names)
```

## Arguments

- group:

  group structure (three options):

  - \\p\\-dimensional vector of group indices (in \\\\1, \ldots, q\\\\)
    or labels,

  - list with \\q\\ slots containing the variable indices (in \\\\1,
    \ldots, p\\\\) or labels,

  - \\p \times p\\ matrix, where the entry in the \\j^{\text{th}}\\ row
    and the \\k^{\text{th}}\\ column indicates whether information
    should be transferred from the \\j^{\text{th}}\\ to the
    \\k^{\text{th}}\\ variable

- j:

  index of predictor

- p:

  number of predictors

- names:

  names of predictors

## Value

Returns a logical vector of length \\p\\.

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md).

## Examples

``` r
p <- 5
names <- paste0("x", seq_len(p))
group <- list()
group$index_vector <- setNames(object = c(1, 1, 2, 2, 3), nm = names)
group$label_vector <- setNames(object = LETTERS[group$index_vector],
                                 nm = names(group$index_vector))
group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
                     FUN = function(x) which(group$label_vector == x))
group$label_list <- lapply(group$index_list, names)
group$matrix <- 1 * outer(X = group$index_vector,
                          Y = group$index_vector,
                          FUN = "==")
corila:::.is_adjacent(group = group[[1]], j = 3, p = p, names = names)
#>    x1    x2    x3    x4    x5 
#> FALSE FALSE  TRUE  TRUE FALSE 
```
