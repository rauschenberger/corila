# Adjacency indicator

Identifies adjacent predictors.

## Usage

``` r
.is_adjacent(group, j, p, names)
```

## Arguments

- group:

  group structure (multiple options):

  - \\p\\-dimensional vector of group indices (in \\\\1, \ldots, q\\\\)
    or labels,

  - list with \\q\\ slots containing the variable indices (in \\\\1,
    \ldots, p\\\\) or labels,

  - \\p \times p\\ matrix, where the entry in the \\j^{\text{th}}\\ row
    and the \\k^{\text{th}}\\ column indicates whether information
    should be transferred from the \\j^{\text{th}}\\ to the
    \\k^{\text{th}}\\ variable

- j:

  index of predictor (positive integer between \\1\\ and \\p\\)

- p:

  number of predictors (positive integer)

- names:

  names of predictors (character vector of length \\p\\)

## Value

Returns a logical vector of length \\p\\.

## Details

This function is called by
[`corila()`](https://rauschenberger.github.io/corila/reference/corila.md).
A predictor is adjacent to itself. If argument `group` is a list
(specifying potentially overlapping groups), two predictors are adjacent
if they are one or more common groups.

## Examples

``` r
p <- 5L
names <- paste0("x", seq_len(p))
group <- list()
group$index_vector <- setNames(object = c(1L, 1L, 2L, 2L, 3L), nm = names)
group$label_vector <- setNames(object = LETTERS[group$index_vector],
                                 nm = names(group$index_vector))
group$index_list <- lapply(X = setNames(nm = unique(group$label_vector)),
                     FUN = function(x) which(group$label_vector == x))
group$label_list <- lapply(group$index_list, names)
group$matrix <- 1L * outer(X = group$index_vector,
                          Y = group$index_vector,
                          FUN = "==")
.is_adjacent(group = group[[1L]], j = 3L, p = p, names = names)
#> [1] FALSE FALSE  TRUE  TRUE FALSE
```
