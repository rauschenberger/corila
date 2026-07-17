
# code for preparing example dataset

set.seed(1L)

# number of training/test observations, predictors, and predictor groups
n0 <- 50L
n1 <- 20L
n <- n0 + n1
p <- 30L
q <- 10L

# group membership
group <- sort(c(seq_len(q),
                sample(x = seq_len(q), size = p - q, replace = TRUE)))

# primary/auxiliary predictors
primary <- as.logical(stats::rbinom(n = p, size = 1L, prob = 0.5))

# hold-out data
holdout <- rep(x = c(FALSE, TRUE), times = c(n0, n1))

# predictor matrix
mu <- rep(x = 0.0, times = p)
rho <- 0.5
sigma <- rho * outer(X = group, Y = group, FUN = "==") +
  (1.0 - rho) * diag(rep(x = 1.0, times = p))
x <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
count <- vapply(
  X = seq_len(p),
  FUN = function(i) sum(group[seq_len(i)] == group[i]),
  FUN.VALUE = numeric(1L)
)

# effect vector
beta_group <- sign(stats::rnorm(n = q)) *
  stats::rbinom(n = q, size = 1L, prob = 0.5)
beta <- rep(x = beta_group, times = table(group)) *
  abs(stats::rnorm(n = p)) *
  stats::rbinom(n = p, size = 1L, prob = 0.8)

# response vector
y <- stats::rnorm(n = n, mean = x %*% beta, sd = 2.0)

# names of predictors
colnames <- paste0(group, ".", count)
colnames[primary] <- paste0("pri_", colnames[primary])
colnames[!primary] <- paste0("aux_", colnames[!primary])
colnames(x) <- names(primary) <- names(group) <- names(beta) <- colnames

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
corila_data <- list(x_train = x_train,
                    y_train = y_train,
                    group = group,
                    primary = primary,
                    beta = beta,
                    x_test = x_test,
                    y_test = y_test)

print(object.size(corila_data), units = "Mb")

usethis::use_data(corila_data, overwrite = TRUE)

# coef <- y_hat <- list()
#
# # standard lasso regression
# object <- glmnet::cv.glmnet(x = data$x_train[, data$primary],
# y = data$y_train)
# temp <- stats::coef(object = object, s = "lambda.min")[-1L]
# coef$glmnet <- c(temp[1L], ifelse(primary, temp[-1L], 0.0))
# y_hat$glmnet <- stats::predict(object = object,
#                                newx = data$x_test[, data$primary],
#                                type = "response",
#                                s = "lambda.min")
#
# # flexible group lasso regression
# object <- cv.corila(x = data$x_train, y = data$y_train,
#                     group = data$group, primary = data$primary)
# coef$corila <- stats::coef(object = object)
# y_hat$corila <- stats::predict(object = object,
#                                newx = data$x_test[, data$primary])
#
# # selection performance
# vapply(X = coef,
#        FUN = function(x) {
#          calc_sign_prec(truth = sign(data$beta),estim = sign(x[-1L]))
#        },
#        FUN.VALUE = numeric(1L))
#
# # predictive performance
# vapply(X = y_hat,
#        FUN = function(x) mean((x - data$y_test)^2.0),
#        FUN.VALUE = numeric(1L))
