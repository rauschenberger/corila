## code to prepare `example.R` dataset goes here

set.seed(1)

# number of training/test observations, predictors, and predictor groups
n0 <- 100
n1 <- 200
n <- n0 + n1
p <- 50
q <- 10

# group membership
group <- sort(sample(x = seq_len(q), size = p, replace = TRUE))

# primary/auxiliary predictors
primary <- as.logical(stats::rbinom(n = p, size = 1, prob = 0.5))

# predictor matrix
mu <- rep(x = 0, times = p)
sigma <- 0.8*outer(X = group, Y = group, FUN = "==") + diag(rep(0.5, times = p))
x <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
count <- sapply(seq_len(p), function(i) sum(group[1:i]==group[i]))
colnames(x) <- paste0(group, ".", count)
colnames(x)[primary] <- paste0("pri_",colnames(x)[primary])
colnames(x)[!primary] <- paste0("aux_",colnames(x)[!primary])

# effect vector
beta <- stats::rnorm(n = p) * stats::rbinom(n = p, size = 1, prob = 0.2)

# response vector
y <- stats::rnorm(n = n, mean = x %*% beta, sd = 1)

# training/test split
fold <- rep(x = c(0, 1), times = c(n0, n1))
x_train <- x[fold == 0, ]
y_train <- y[fold == 0]
x_test <- x[fold == 1, ]
x_test[, !primary] <- NA
y_test <- y[fold == 1]

example <- list(x_train = x_train,
                y_train = y_train,
                group = group,
                primary = primary,
                x_test = x_test,
                y_test = y_test)

usethis::use_data(example, overwrite = TRUE)
