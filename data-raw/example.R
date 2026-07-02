
# code for preparing example dataset

set.seed(1)

# number of training/test observations, predictors, and predictor groups
n0 <- 100
n1 <- 1000
n <- n0 + n1
p <- 50
q <- 10

# group membership
group <- sort(sample(x = seq_len(q), size = p, replace = TRUE))

# primary/auxiliary predictors
primary <- as.logical(stats::rbinom(n = p, size = 1, prob = 0.5))

# hold-out data
holdout <- rep(x = c(FALSE, TRUE), times = c(n0, n1))

# predictor matrix
mu <- rep(x = 0, times = p)
rho <- 0.8
sigma <- rho * outer(X = group, Y = group, FUN = "==") +
  (1 - rho) * diag(rep(0.5, times = p))
x <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
count <- sapply(seq_len(p), function(i) sum(group[1:i] == group[i]))

# effect vector
beta <- stats::rnorm(n = p) * stats::rbinom(n = p, size = 1, prob = 0.2)

# response vector
y <- stats::rnorm(n = n, mean = x %*% beta, sd = 1)

# names of predictors
colnames <- paste0(group, ".", count)
colnames[primary] <- paste0("pri_", colnames[primary])
colnames[!primary] <- paste0("aux_", colnames[!primary])
colnames(x) <- names(primary) <- names(holdout) <- names(group) <- colnames

# names of observations
rownames <- c(paste0("train_", seq_len(n0)), paste0("test_", seq_len(n1)))
rownames(x) <- names(y) <- rownames

# training/test split
x_train <- x[!holdout, ]
y_train <- y[!holdout]
x_test <- x[holdout, ]
x_test[, !primary] <- NA # privileged information
y_test <- y[holdout]

# dataset
data <- list(x_train = x_train,
             y_train = y_train,
             group = group,
             primary = primary,
             beta = beta,
             x_test = x_test,
             y_test = y_test)

print(object.size(data),units="Mb")

usethis::use_data(data, overwrite = TRUE)
