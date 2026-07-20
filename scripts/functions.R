
#' @srrstats {G1.5} *code for reproducing results in manuscript*
#' @srrstats {G1.6} *code for comparing with other methods*

#' @title
#' Correlation Plot
#' 
#' @description
#' Visualise correlation among grouped variables
#' 
#' @param x
#' \eqn{p \times p} numerical matrix
#' 
#' @param group
#' \eqn{p}-dimensional character vector
#' 
#' @param exp
#' non-negative scalar
#' 
#' @param min
#' positive integer
#' 
#' @param cex
#' non-negative scalar
#' 
#' @param xline
#' non-negative scalar or `NULL`
#' 
#' @param yline
#' non-negative scalar or `NULL`
#'
#' @examples
#' q <- 10L
#' group <- rep(LETTERS[seq_len(q)], times = stats::rpois(n = q, lambda = 4.0))
#' p <- length(group)
#' rho <- 0.8
#' sigma <- rho * outer(X = group, Y = group, FUN = "==") + (1.0 - rho) * diag(p)
#' x <- MASS::mvrnorm(n = 10L, mu = rep(0.0, times = p), Sigma = sigma)
#' .plot_cor(x = sigma, group = group, min = 1.0)
#' .plot_cor(x = cor(x), group = group, min = 1.0)
#' 
.plot_cor <- function(x, group, exp = 1.0, min = 5L, cex = 0.7,
                      xline = 0.5, yline = 0.5) {
  .assert(x = x, type = "numeric", dim = c(Inf, Inf),
          min = -1.0, max = 1.0)
  p <- ncol(x)
  .assert(x = group, type = "nominal", dim = p)
  .assert(x = exp, type = "numeric", min = 0.0)
  .assert(x = min, type = "integer", min = 1L)
  .assert(x = cex, type = "numeric", min = 0.0)
  .assert(x = xline, type = "numeric", min = 0.0)
  .assert(x = yline, type = "numeric", min = 0.0)
  levels <- names(sort(table(group), decreasing = TRUE))
  index <- sapply(levels, function(x) which(group == x))
  size <- sapply(index, length)
  order <- unlist(index)
  cor_exp <- sign(x) * abs(x) ^ exp
  col <- grDevices::colorRampPalette(c("blue", "white", "red"))(200L)
  graphics::image(x = cor_exp[order, rev(order)],
                  axes = FALSE, col = col, zlim = c(-1.0, 1.0))
  pos <- (c(0.0, cumsum(size)) - 0.5) / (ncol(x) - 1L)
  # add grid
  #lwd <- ifelse(size >= min, 2, ifelse(size > 2, 1, 0.5))
  graphics::abline(v = pos, col = "grey", lty = 1L, lwd = 0.5)
  graphics::abline(h = 1.0 - pos, col = "grey" ,lty = 1L, lwd = 0.5)
  # add ticks
  spaces <- rep(x = "", times = length(levels) + 1L)
  graphics::axis(side = 3L, at = pos, labels = spaces,
                 lwd = 0.0, lwd.ticks = 0.5)
  graphics::axis(side = 2L, at= 1.0 - pos, labels = spaces,
                 lwd = 0.0, lwd.ticks = 0.5)
  # add labels
  label <- which(size >= min)
  pos_centre <- 0.5 * pos[label] + 0.5 * pos[label + 1L]
  if(!is.null(xline)){
    las <- ifelse(any(nchar(levels) >= 10L), 2L, 1L)
    graphics::mtext(text = levels[label], side = 3L, at = pos_centre,
                  las = las, cex = cex, line = xline)
  }
  if(!is.null(yline)){
    graphics::mtext(text = levels[label], side = 2L, at = 1.0 - pos_centre,
                  las = 1L, cex = cex, line = yline)
  }
  invisible(NULL)
}

#' @title
#' Visualise adjacency or correlation matrix
#'
#' @param z
#' \eqn{p \times p} adjacency or correlation matrix
#'
#' @param col
#' colour
#'
#' @examples
#' size <- c(3, 3, 2, 1)
#' group <- rep(x = seq_along(size), times = size)
#' z <- outer(group, group, FUN = "==")
#' .plot_groups(z, col = "black")
#'
.plot_groups <- function(z, col = "black"){
  if(is.logical(z)){
    class(z) <- "numeric"
  }
  .assert(x = z, type = "numeric", dim = c(Inf, Inf))
  .assert(x = col, type = "nominal")
  if(ncol(z) != nrow(z)) {
    stop("Requires p rows and p columns.")
  }
  if(any(abs(diag(z) - 1.0) > 1e-06)){
    stop("Requires unit diagonal.")
  }
  p <- ncol(z)
  xpos <- seq(from = 0.0, to = 1.0, length.out = p)
  ypos <- seq(from = 1.0, to = 0.0, length.out = p)
  lines <- seq(from = - 0.5 / (p - 1L),
               to = 1L + 0.5 / (p - 1L),
               length.out = p + 1L)
  if(all(z %in% c(0, 1))) {
    breaks <- c(0.0, 0.5, 1.0)
    col <- c("white", col)
  } else {
    max <- 1.1*max(abs(z))
    eps <- 1e-06
    breaks <- c(seq(-max, -eps, length.out = 50L),
                seq(eps, max, length.out = 50L))
    col <- grDevices::colorRampPalette(c("blue","white","red"))(99L)
    col <- c(col[1L:49L], "white", col[51L:99L])
  }
  graphics::par(mar=c(0.0, 2.0, 2.0, 0.0))
  graphics::image(t(z[p:1L,]), breaks = breaks, col = col, axes = FALSE)
  graphics::abline(h=lines, col = "white")
  graphics::abline(v=lines, col = "white")
  labels <- parse(text = paste0("x[", seq_len(p), "]"))
  graphics::axis(side = 2L, at = ypos, labels = labels, tick = FALSE,
                 las = 2L, line = -0.5)
  graphics::axis(side = 3L, at = xpos, labels = labels, tick = FALSE,
                 las = 1L, line = -0.5)
}


#' @title
#' Visualise learning with privileged information (LUPI)
#' 
#' @description
#' Visualises the predictor matrix, the coefficient vector,
#' and the response vector.
#' 
#' @param x
#' predictor matrix with \eqn{n} rows and \eqn{p} columns
#' 
#' @param y
#' response vector of length \eqn{n}
#' 
#' @param holdout
#' logical vector of length \eqn{n}
#' 
#' @param group
#' integer vector of length \eqn{p}
#' 
#' @param primary
#' logical vector of length \eqn{p}
#' 
#' @return
#' Renders a plot and returns `NULL` invisibly.
#' 
#' @examples
#' set.seed(1)
#' n <- 10L; p <- 20L; q <- 5L
#' group <- rep(x = seq_len(q), each = p / q)
#' sigma <- 0.9 * outer(group, group, "==") + 0.1 * diag(p)
#' x <- mvtnorm::rmvnorm(n = n, mean = rep(0.0, times = p), sigma = sigma)
#' beta <- stats::rbinom(n = p, size = 1L, prob = 0.25) * stats::rnorm(p)
#' y <- as.numeric(scale(x %*% beta))
#' holdout <- rep(c(FALSE, TRUE), each = n / 2L)
#' primary <- rep(rep(c(TRUE, FALSE), times = c(1L, p / q - 1L)), times = q)
#' .heatmap_lupi(x = x, y = y, holdout = holdout, group = group, primary = primary)
#' 
#' data <- .simulate_lupi_data(mode = "upstream",
#'     p = 30L, q = 5L, n0 = 10L, n1 = 20L)
#' .heatmap_lupi(x = data$x_train, y = data$y_train,
#'    group = data$group, primary = data$primary,
#'    holdout = rep(c(FALSE, TRUE), times = c(5L, 5L)))
#' 
.heatmap_lupi <- function(x, y, holdout = NULL, group = NULL, primary = NULL) {
  .assert(x = x, type = "numeric", dim = c(Inf, Inf), na.rm = TRUE)
  n <- nrow(x)
  p <- ncol(x)
  .assert(x = y, type = "numeric", dim = n, na.rm = TRUE)
  .assert(x = holdout, type = "logical", dim = n, na.rm = TRUE)
  if(is.null(holdout)){
    holdout <- rep(x = FALSE, times = n)
  }
  .assert(x = group, type = "numeric", dim = p)
  if(!is.null(group)) {
    if( any(group != sort(group)) ) {
      stop("Vector group should be sorted.")
    }
  }
  .assert(x = primary, type = "logical", dim = p)
  if(is.null(group)) {
    group <- rep(x = 1L, times = p)
  }
  if(is.null(primary)) {
    primary <- rep(x = TRUE, times = p)
  }
  y[holdout] <- NA
  x[holdout, !primary] <- NA
  graphics::par(mar = c(0.0, 0.0, 0.0, 0.0), oma = c(2.0, 4.0, 4.0, 2.0))
  cols <- list(na = "lightgrey", grid = "grey",sep = "black", box = "grey")
  lwd <- list(grid = 1.0, sep = 3.0, box = 1.0)
  cex <- list(axis = 1.1, cell = 1.2, lab = 0.9)
  col  <- grDevices::colorRampPalette(c("blue","white","red"))(99)
  col <- c(col[1L:49L], "white", col[51L:99L], cols$na)
  max <- 1.01 * max(abs(c(as.vector(x), as.vector(y))), na.rm=TRUE)
  eps <- 1e-06
  breaks <- c(seq(-max, -eps, length.out = 50L),
              seq(eps, max, length.out = 50L), 99e99)
  xpos <- seq(from = 0.0, to = 1.0, length.out = p)
  ypos <- seq(from = 1.0, to = 0.0, length.out = n)
  vlines <- seq(from = - 0.5 / (p - 1L),
                to = 1.0 + 0.5 / (p - 1L),
                length.out = p + 1)
  hlines <- seq(from = - 0.5 / (n - 1L),
                to = 1.0 + 0.5 / (n - 1L),
                length.out = n + 1L)
  thick_vlines <- (which(diff(group) != 0.0) - 0.5) * 1.0 / (p - 1L)
  
  y[is.na(y)] <- 99e99 # for grey colour
  #--- feature matrix ---
  graphics::layout(mat = matrix(data = seq_len(9L), nrow = 3L, ncol = 3L),
                   widths = c(p, 1.0, 1.0), heights = c(n, 1.0, 1.0))
  graphics::image(x = t(x[nrow(x):1L, ]), axes = FALSE,
                  col = col, breaks = breaks)
  if(all(!holdout)){
    graphics::mtext(side = 2L, at = 0.5, text = "observations",
                    line = 2.0, font = 2L, cex = cex$lab)
  } else {
      graphics::mtext(side = 2L,
                      at = c(1.0 - mean(holdout) / 2.0, mean(holdout) / 2.0),
                      text = c("training set", "test set"),
                      line = 2.0, font = 2L, cex = cex$lab)
  }
  graphics::mtext(side = 3L, text = "predictors",
                  line = 2.0, font = 2L, cex = cex$lab)
  graphics::abline(h = hlines, col = cols$grid, lwd = lwd$grid)
  graphics::abline(v = vlines, col = cols$grid, lwd = lwd$grid)
  graphics::abline(v = thick_vlines, lwd = lwd$sep, col = cols$sep)
  is_na <- which(is.na(x) | x == 99e99, arr.ind = TRUE)
  if(nrow(is_na)>0L){
    graphics::text(x = xpos[is_na[, "col"]], y = ypos[is_na[, "row"]],
                   labels = "?", font = 2L, cex = cex$cell)
  }
  graphics::abline(h = mean(holdout), lwd = lwd$sep, col = cols$sep)
  graphics::box(col=cols$box, lwd=lwd$box)
  graphics::axis(side = 2L,
                 at = ypos,
                 labels = seq_len(n), tick = FALSE, line = 0.0, las = 2L,
                 cex.axis=cex$axis)
  graphics::axis(side = 3L,
                 at = xpos,
                 labels = parse(text = paste0("x[", seq_len(p), "]")),
                 tick = FALSE, line = -0.5,
                 cex.axis=cex$axis)
  #--- effect vector ---
  graphics::plot.new()
  graphics::image(x = matrix(ifelse(primary, 1L, NA), ncol = 1L),
                  axes = FALSE, col = cols$na)
  graphics::abline(v = vlines, col = cols$grid, lwd = lwd$grid)
  graphics::mtext(side = 2L, text = "coefs", line = 2.0, font = 2L,
                  cex = cex$lab)
  graphics::box(col=cols$box,lwd=lwd$box)
  graphics::abline(v = thick_vlines, lwd = lwd$sep, col = cols$sep)
  graphics::axis(side = 1L,
                 at = xpos,
                 labels = parse(text = paste0("hat(beta)[", seq_len(p), "]")),
                 tick = FALSE,
                 line = -0.3,
                 cex.axis=cex$axis)
  graphics::text(x = xpos[!primary], y = 0.0, label = "0",
                 font = 2L, cex = cex$cell, col = "black")
  graphics::text(x = xpos[primary], y = 0.0, label = "?",
                 font = 2L, cex = cex$cell)
  graphics::plot.new()
  graphics::plot.new()
  graphics::plot.new()
  #--- target vector ---
  graphics::image(x = matrix(rev(y), nrow = 1L), axes = FALSE,
                  col = col, breaks = breaks)
  graphics::abline(h = hlines, col = cols$grid, lwd = lwd$grid)
  graphics::abline(h = mean(holdout), lwd = lwd$sep, col = cols$sep)
  graphics::mtext(text = "response", side = 3L, line = 2.0,
                  font = 2L, cex = cex$lab)
  graphics::axis(side = 3L, at = 0.0, labels = "y", tick = FALSE,
                 line = -0.5, cex.axis = cex$cell)
  graphics::box(col = cols$box, lwd = lwd$box)
  graphics::text(x = 0.0, y = ypos[holdout], labels = "?",
                 font = 2L, cex = cex$cell)
  graphics::plot.new()
  invisible(NULL)
}


#' @title
#' Simulation with Privileged Information
#'
#' @description
#' Simulates data for learning using privileged information (LUPI).
#'
#' @param mode
#' character string `"upstream"`, `"aggregated"`,
#' `"surrogate"`, `"baseline"`, or `"uninformative"`
#'
#' @param n0
#' number of observations used for fitting the model
#' (size of training set, integer \eqn{>= 2})
#'
#' @param n1
#' number of observations used for making predictions
#' (size of test set, integer \eqn{>=2})
#'
#' @param p
#' number of predictors
#' (integer \eqn{>=2})
#'
#' @param q
#' number of predictor groups
#' (integer \eqn{>=2})
#'
#' @param plot
#' logical
#'
#' @return
#' Returns the \eqn{n0}-dimensional and \eqn{n1}-dimensional
#' outcome vectors `y_train` and `y_test`,
#' the \eqn{n0 \times p} and \eqn{n1 \times p} predictor matrices
#' `x_train` and `x_test`,
#' the \eqn{p}-dimensional integer vector `group`
#' grouping the predictors,
#' the \eqn{p}-dimensional logical vector `primary`
#' indicating primary predictors,
#' and the \eqn{p}-dimensional effect vector `beta`.
#'
#' @keywords internal
#'
#' @examples
#' data <- .simulate_lupi_data(mode = "upstream")
#' sapply(data, length)
#' 
#' data <- .simulate_lupi_data(mode = "baseline")
#' sapply(data, length)
#' # Also return ordered group object (sort X, beta, group, binary, causal).
#'
.simulate_lupi_data <- function(mode, n0 = 100L, n1 = 10000L, p = 200L, q = 4L,
                                plot = FALSE) {
  .assert(x = n0, type = "integer", min = 2L)
  .assert(x = n1, type = "integer", min = 2L)
  .assert(x = p, type = "integer", min = 5L)
  .assert(x = q, type = "integer", min = 2L, max = p)
  .assert(x = mode, type = "nominal",
          support = c("upstream", "aggregated", "surrogate", "baseline", "uninformative"))
  .assert(x = plot, type = "logical")
  fold <- rep(x = c(0L, 1L), times = c(n0, n1))
  n <- n0 + n1
  if (p %% q != 0L) {
    stop("This function simulates equally sized groups.",
         "So `p` must be a multiple of `q`.")
  }
  if (mode == "upstream") {
    #--- upstream and downstream predictors ---
    group <- rep(x = seq_len(p / q),
                 each = q)
    primary <- rep(x = rep(x = c(TRUE, FALSE), times = c(1L, q - 1L)),
                   times = p / q)
    causal <- rep(x = sample(rep(x = c(TRUE, FALSE), times = c(5L, p / q - 5L))),
                  each = q)
    x <- matrix(data = NA, nrow = n, ncol = p)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & primary
      sel_aux <- group == j & !primary
      x[, sel_pry] <- stats::rnorm(n = n)
      #w <- c(0.2,0.5,0.8) # original
      w <- stats::runif(q - 1) # trial
      x[, sel_aux] <- x[, sel_pry] %*% t(sqrt(w)) + t(t(matrix(
        stats::rnorm(n * sum(sel_aux)),
        nrow = n,
        ncol = sum(sel_aux)
      )) * sqrt(1.0 - w))
    }
    beta <- (!primary) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "aggregated") {
    #--- fine-grained and aggregated predictors ---
    group <- rep(x = seq_len(p / q), each = q)
    primary <- rep(x = rep(x = c(TRUE, FALSE), times = c(1L, q - 1L)),
                   times = p / q)
    causal <- rep(x = sample(rep(
      x = c(TRUE, FALSE), times = c(5L, p / q - 5L)
    )), each = q)
    x <- matrix(data = NA,
                nrow = n,
                ncol = p)
    #w <- 0.5
    #w <- c(1/3,1/3,1/3)
    #w <- stats::rgamma(n=3,shape=1,rate=1)
    #w <- w/sum(w)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & primary
      sel_aux <- group == j & !primary
      w <- stats::runif(n = 1L)
      #x[,sel_aux] <- sqrt(w)*stats::rnorm(n=n)+
      #sqrt(1-w)*stats::rnorm(n=n*sum(sel_aux))
      #x[,sel_pry] <- rowSums(x[,sel_aux])
      x[, sel_aux] <- stats::rnorm(n = n * sum(sel_aux))
      w <- stats::runif(n = q)
      #w <- c(1/3,1/3,1/3)
      #w <- stats::rgamma(n=4,shape=1,rate=1)
      w <- w / sum(w)
      #x[,sel_pry] <- sqrt(v)*(x[,sel_aux] %*% sqrt(w))+
      #sqrt(1-v)*stats::rnorm(n=n) # originally with v=0.7
      #w <- c(1/4,1/4,1/4); x[,sel_pry] <-
      #sqrt(1)*(x[,sel_aux] %*% sqrt(w))+sqrt(1/4)*stats::rnorm(n=n)
      # original # TRIAL (was above)
      #w <- stats::runif(n=4)
      #
      #w[4] <- 0
      #w <- c(0.1,0.2,0.4,0.3)
      #w <- w/sum(w)
      x[, sel_pry] <- cbind(x[, sel_aux], stats::rnorm(n = n)) %*% sqrt(w)
      #x[,sel_pry] <- x[,sel_aux] %*% sqrt(w)
    }
    beta <- (!primary) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "surrogate") {
    #--- canonical and surrogate predictors ---
    group <- rep(x = seq_len(p / q), each = q)
    primary <- rep(x = rep(x = c(FALSE, TRUE), times = c(1L, q - 1L)),
                   times = p / q)
    causal <- rep(x = sample(rep(
      x = c(TRUE, FALSE), times = c(5L, p / q - 5L)
    )), each = q)
    x <- matrix(data = NA,
                nrow = n,
                ncol = p)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & primary
      sel_aux <- group == j & !primary
      x[, sel_aux] <- stats::rnorm(n = n)
      # w <- c(0.2,0.5,0.8) # TRIAL
      # was c(0.7,0.5,0.3)#  stats::runif(n=q-1) # rep(x=0.9,times=q-1)
      w <- stats::runif(n = q - 1L)
      x[, sel_pry] <- x[, sel_aux] %*% t(sqrt(w)) + t(t(matrix(
        stats::rnorm(n * sum(sel_pry)),
        nrow = n,
        ncol = sum(sel_pry)
      )) * sqrt(1 - w))
    }
    beta <- (!primary) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "baseline") {
    #--- baseline and follow-up predictors ---
    w <- c(NA, rep(x = 0.9, times = q - 1L))
    #w <- c(NA,runif(3)) # TRIAl
    list <- list()
    list[[1]] <- matrix(
      data = stats::rnorm(n = n * p / q),
      nrow = n,
      ncol = p / q
    )
    for (j in seq(from = 2L, to = q)) {
      list[[j]] <- sqrt(w[j]) * list[[j - 1L]] +
        sqrt(1 - w[j]) * stats::rnorm(n = n * p / q)
    }
    x <- do.call(what = "cbind", args = list)
    group <- rep(x = seq_len(p / q), times = q)
    primary <- rep(x = c(TRUE, FALSE), times = c(p / q, p / q * (q - 1L)))
    beta <- sample(rep(x = c(0.0, 1.0), times = c(p / q - 5L, 5L))) * 
      abs(stats::rnorm(n = p / q))
    causal <- NULL
    eta <- list[[length(list)]] %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * sd(eta))
  } else if (mode == "uninformative") {
    group <- rep(x = seq_len(p / q), each = q)
    primary <- rep(x = rep(x = c(TRUE, FALSE), times = c(1L, q - 1L)),
                   times = p / q)
    causal <- rep(x = sample(rep(x = c(TRUE, FALSE), times = c(5L, p / q - 5L))),
                  each = q)
    x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
    beta <- primary * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  }
  # else if(mode=="adversarial"){
  #   group <- rep(x=seq_len(p/q),each=q)
  #   primary <- rep(x=rep(x=c(TRUE,FALSE),times=c(1,q-1)),times=p/q)
  #   causal <- rep(x=sample(rep(x=c(TRUE,FALSE),times=c(5,p/q-5))),each=q)
  #   x <- matrix(data=NA,nrow=n,ncol=p)
  #   for(j in seq_len(p/q)){
  #     sel.pry <- group==j & primary
  #     sel.aux <- group==j & !primary
  #     x[,sel.pry] <- stats::rnorm(n=n)
  #     w <- stats::runif(q - 1)
  #     x[,sel.aux] <- x[,sel.pry] %*% t(sqrt(w)) +
  # t(t(matrix(stats::rnorm(n*sum(sel.aux)),nrow=n,ncol=sum(sel.aux))) *
  # sqrt(1-w))
  #   }
  #   beta <- ifelse(primary,1,-1/3) * causal * abs(stats::rnorm(n=p))
  #   eta <- x %*% beta
  #   y <- eta + stats::rnorm(n=n,sd=0.5*stats::sd(eta))
  # }
  #else if(mode=="multiview"){
  #   #--- multi-view blocks ---
  #   mean <- rep(x=0,times=p/q)
  #   sigma <- matrix(data=NA,nrow=p/q,ncol=p/q)
  #   sigma <- 0^abs(col(sigma)-row(sigma))
  #   z <- mvtnorm::rmvnorm(n=n,mean=mean,sigma=sigma)
  #   list <- list()
  #   w <- 0.7
  #   for(j in seq_len(q)){
  #     list[[j]] <- sqrt(w)*z + sqrt(1-w)*stats::rnorm(n=n*p/q)
  #   }
  #   x <- do.call(what="cbind",args=list)
  #   group <- rep(x=seq_len(p/q),times=q)
  #   primary <- rep(x=c(TRUE,FALSE),times=c(p/q,p/q*(q-1)))
  #   beta <- sample(rep(x=c(0,1),times=c(p/q-5,5)))*abs(stats::rnorm(n=p/q))
  #   eta <- z %*% beta
  #   y <- eta + stats::rnorm(n=n,sd=0.5*sd(eta))
  # }
  sd <- apply(X = x, MARGIN = 2L, FUN = function(x) stats::sd(x))
  if (any(sd <= 0.95) || any(sd >= 1.05)) {
    warning("no unit variance")
  }
  if (plot) {
    graphics::par(mfrow = c(1L, 2L))
    graphics::plot(beta, col = group)
    graphics::image(t(stats::cor(x)[p:1L, ]))
  }
  list(y_train = y[fold == 0L],
       x_train = x[fold == 0L, ],
       y_test = y[fold == 1L],
       x_test = x[fold == 1L, ],
       group = group,
       primary = primary,
       causal = causal,
       beta = beta)
}


#' @title
#' Visualise Simulation Settings
#'
#' @examples
#' graphics::par(mar=c(0,0,1.5,0))
#' .flowchart_lupi(mode = "baseline")
#'
.flowchart_lupi <- function(mode, lwd = 1.5, length_arrow = 0.06,
                            mar = 0.3, xlim = c(1.0, 5.0),
                            ylim = c(11.0, 0.0),
                            cex = 0.9) {
  .assert(x = mode, type = "nominal",
          support = c("upstream", "aggregated", "surrogate", "baseline"))
  .assert(x = lwd, type = "numeric", min = 0.0)
  .assert(x = length_arrow, type = "numeric", min = 0.0)
  .assert(x = mar, type = "numeric", min = 0.0)
  .assert(x = xlim, type = "numeric", dim = 2L)
  .assert(x = ylim, type = "numeric", dim = 2L)
  .assert(x = cex, type = "numeric", min = 0.0)
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim)
  if (identical(mode, "upstream")) {
    graphics::mtext(
      at = c(NA, 3.0),
      adj = c(0.0, NA),
      text = c("upstream", "downstream"),
      col = c("blue", "red"),
      side = 3L,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1.0,
      y = c(1.0, 5.0, 10.0),
      label = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 1.0 + mar,
      y0 = c(1.0, 5.0, 10.0),
      x1 = 2.0,
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 2.0,
      y0 = rep(c(1.0, 5.0, 10.0), each = 3L),
      x1 = 3.0 - mar,
      y1 = c(0:2, 4:6, 9:11),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 3,
      y = c(0:2, 4:6, 9:11),
      label = expression(x["1,1"], x["1,2"], x["1,3"],
                         x["2,1"], x["2,2"], x["2,3"],
                         x["50,1"], x["50,2"], x["50,3"]),
      col = "red",
      cex = cex
    )
    knot <- c(2.5, 5.0, 8.0) # c(2,5,9)
    graphics::segments(
      x0 = 3 + mar,
      y0 = c(0:2, 4:6, 9:11),
      x1 = 4,
      y = rep(knot, each = 3L),
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 4.0,
      y0 = knot,
      x1 = 5.0 - mar,
      y1 = 5.0 + seq(
        from = -1,
        to = +1,
        length.out = 3L
      ),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 5,
      y = 5,
      label = "y",
      col = "blue",
      cex = cex
    )
    graphics::text(
      x = c(1.0, 3.0),
      y = 7.5,
      label = "...",
      srt = 90.0,
      font = 2L,
      col = c("blue", "red"),
      cex = cex
    )
  } else if (identical(mode, "aggregated")) {
    graphics::mtext(
      at = c(NA, 3.0),
      adj = c(0, NA),
      text = c("aggregated", "fine-grained"),
      col = c("blue", "red"),
      side = 3L,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1.0,
      y = c(1, 5, 10),
      label = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 3.0 - mar,
      y0 = c(0:2, 4:6, 9:11),
      x1 = 2.0,
      y = rep(c(1, 5, 10), each = 3L),
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 2,
      y0 = c(1, 5, 10),
      x1 = 1 + mar,
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 3,
      y = c(0:2, 4:6, 9:11),
      label = expression(x["1,1"], x["1,2"], x["1,3"],
                         x["2,1"], x["2,2"], x["2,3"],
                         x["50,1"], x["50,2"], x["50,3"]),
      col = "red",
      cex = cex
    )
    knot <- c(2.5, 5, 8) # c(2,5,9)
    graphics::segments(
      x0 = 3 + mar,
      y0 = c(0:2, 4:6, 9:11),
      x1 = 4,
      y = rep(knot, each = 3L),
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 4,
      y0 = knot,
      x1 = 5 - mar,
      y1 = 5 + seq(
        from = -1,
        to = +1,
        length.out = 3L
      ),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 5,
      y = 5,
      label = "y",
      col = "blue",
      cex = cex
    )
    graphics::text(
      x = c(1, 3),
      y = 7.5,
      label = "...",
      srt = 90,
      font = 2L,
      col = c("blue", "red"),
      cex = cex
    )
  } else if (identical(mode, "surrogate")) {
    graphics::mtext(
      at = c(NA, 3),
      adj = c(0, NA),
      text = c("surrogate", "canonical"),
      col = c("blue", "red"),
      side = 3L,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1.0,
      y = c(0:2, 4:6, 9:11),
      label = expression(x["1,1"], x["1,2"], x["1,3"],
                         x["2,1"], x["2,2"], x["2,3"],
                         x["50,1"], x["50,2"], x["50,3"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 3.0 - mar,
      y0 = c(1, 5, 10),
      x1 = 2.0,
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 2.0,
      y0 = rep(c(1, 5, 10), each = 3L),
      x1 = 1 + mar,
      y1 = c(0:2, 4:6, 9:11),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 3,
      y = c(1, 5, 10),
      label = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "red",
      cex = cex
    )
    graphics::arrows(
      x0 = 3 + mar,
      y0 = c(1, 5, 10),
      x1 = 5 - mar,
      y1 = 5 + seq(
        from = -1,
        to = +1,
        length.out = 3L
      ),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 5,
      y = 5,
      label = "y",
      col = "blue",
      cex = cex
    )
    graphics::text(
      x = c(1, 3),
      y = 7.5,
      label = "...",
      srt = 90,
      font = 2,
      col = c("blue", "red"),
      cex = cex
    )
  } else if (identical(mode, "baseline")) {
    graphics::mtext(
      at = c(NA, 3),
      adj = c(0, NA),
      text = c("baseline", "follow-up"),
      col = c("blue", "red"),
      side = 3L,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1,
      y = c(1, 5, 10),
      labels = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "blue",
      cex = cex
    )
    graphics::arrows(
      x0 = 1:3 + mar,
      y0 = rep(c(1, 5, 10), each = 3),
      x1 = 2:4 - mar,
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = rep(x = c(2, 3, 4), times = 3),
      y = rep(c(1, 5, 10), each = 3),
      labels = expression(x["1,1"], x["1,2"], x["1,3"],
                          x["2,1"], x["2,2"], x["2,3"],
                          x["50,1"], x["50,2"], x["50,3"]),
      col = "red",
      cex = cex
    )
    graphics::arrows(
      x0 = 4 + mar,
      y0 = c(1, 5, 10),
      x1 = 5 - mar,
      y1 = 5 + seq(
        from = -1,
        to = +1,
        length.out = 3
      ),
      length = length_arrow,
      col = "grey",
      lwd = lwd
    )
    graphics::text(
      x = 5,
      y = 5,
      label = "y",
      col = "blue",
      cex = cex
    )
    graphics::text(
      x = c(1, 2, 3, 4),
      y = 7.5,
      label = "...",
      srt = 90.0,
      font = 2L,
      col = rep(x = c("blue", "red"), times = c(1L, 3L)),
      cex = cex
    )
  }
  invisible(NULL)
}


#' @title
#' Plot Change
#'
#' @description
#' Visualises change from start to end value
#' in different repetitions of different settings.
#' 
#' @param x
#' list of matrices of equal dimensions
#' 
.plot_change <- function(x, ylab = "", main = names(x) , alternative = "two.sided",
                         lwd = 1.0, cex = 1.0, cex.axis = 1.0, cex.lab = 1.0){
  if(!is.list(x)){stop("Expect list.")}
  nslot <- length(x)
  for (i in seq_len(nslot)) {
    .assert(x = x[[i]], type = "numeric", dim = c(Inf, Inf))
  }
  .assert(x = main, type = "nominal", dim = nslot)
  .assert(x = alternative, type = "nominal",
          support = c("two.sided", "greater", "less"))
  ylim <- range(x)
  if(graphics::par()$mfrow[2L] != nslot){
    warning("Set graphics::par(mfrow=c(...,length(x)))." )
  }
  if(!is.na(graphics::par()$xpd)){
    warning("Set graphics::par(xpd=NA).")
  }
  for (i in seq_len(nslot)) {
    ncol <- ncol(x[[i]])
    nrow <- nrow(x[[i]])
    graphics::plot.new()
    graphics::plot.window(xlim = c(0.5, ncol + 0.5), ylim = ylim)
    usr <- graphics::par("usr")
    if (i == 1) {
      graphics::axis(side = 2L, cex.axis = cex.axis)
      graphics::segments(x0 = usr[1L], y0 = usr[3L], y1 = usr[4L])
      graphics::segments(x0 = usr[1L], x1 = 99, y0 = usr[3L])
      graphics::title(ylab = ylab , cex.lab = cex.lab)
    }
    graphics::title(main = main[i], line = 0.5, cex.main = cex.lab)
    for (k in seq_len(nrow)) {
      graphics::lines(x = seq_len(ncol), y = x[[i]][k, ],
        col = "grey", lwd = lwd)
    }
    col <- matrix(data = "grey", nrow = nrow , ncol = ncol)
    col[, 1] <- "blue"
    col[, ncol] <- "red"
    graphics::points(x = col(x[[i]]), y = x[[i]],
                     col = col, pch = 16L, cex = cex)
    pvalue <- stats::t.test(x = x[[i]][, 1],
                            y = x[[i]][, ncol],
                            paired = TRUE,
                            alternative = alternative)$p.value
    text <- paste0("p=", format(x = signif(pvalue, digits = 2L),
                                scientific = TRUE))
    #graphics::mtext(text = text, side = 1, cex = cex.axis, line = 0.2)
    graphics::axis(side = 1L, at = ncol/2L + 0.5, labels = text,
                   cex.axis = cex.axis, tick = FALSE, line = - 0.5)
  }
  invisible(NULL)
}

# dependencies: imports: , mvtnorm, pROC, survival
# suggests: CBPE, MLGL, Matrix, SGL, ecpc, gglasso,
# grpreg, grpregOverlap, multiview, pcLasso,
# scoop, sparsegl, squeezy, graper

simulate_overlap <- function() {
  n0 <- 100L
  n1 <- 10000L
  n <- n0 + n1
  p <- 100L
  n_group <- 20L
  size_group <- rep(x = 5, times = n_group)
  # sample(x = 2:10, size = n_group, replace = TRUE)
  group <- lapply(X = seq_len(n_group),
                  FUN = function(i) {
                    sort(sample(x = seq_len(p), size = size_group[i]))
                  })
  # Inside corila, put each feature that is in no group in a separate group?
  mean <- rep(x = 0, times = p)
  sigma <- matrix(data = NA, nrow = p, ncol = p)
  #sigma <- 0.95^(abs(col(sigma)-row(sigma)))
  # alternative:
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      group_i <- vapply(group, function(x) i %in% x, logical(1))
      group_j <- vapply(group, function(x) j %in% x, logical(1))
      sigma[i, j] <- 0.5^(i != j) * 0.25^(!any(group_i & group_j) & (i != j))
    }
  }
  sigma <- as.matrix(Matrix::nearPD(x = sigma)$mat)
  x <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma)
  sel_group <- sample(x = seq_len(n_group), size = 3)
  beta <- 1 * (seq_len(p) %in% unlist(group[sel_group]))
  # NB: multiply by abs(stats::rnorm(p))?
  eta <- x %*% beta
  y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
  fold <- rep(x = c(0, 1), times = c(n0, n1))
  x_train <- x[fold == 0, ]
  y_train <- y[fold == 0]
  x_test <- x[fold == 1, ]
  y_test <- y[fold == 1]
  info <- data.frame(n0 = n0, n1 = n1, p = p, n_group = n_group)
  list(x_train = x_train,
       group = group,
       y_train = y_train,
       x_test = x_test,
       y_test = y_test,
       beta = beta,
       info = info)
}

#----- code for assessing selection and predictive performance -----

#' coef <- y_hat <- list()
#'
#' # standard lasso regression
#' object <- glmnet::cv.glmnet(x = data$x_train[, data$primary],
#' y = data$y_train)
#' temp <- stats::coef(object = object, s = "lambda.min")[-1L]
#' coef$glmnet <- c(temp[1L], ifelse(data$primary, temp[-1L], 0.0))
#' y_hat$glmnet <- stats::predict(object = object,
#'                                newx = data$x_test[, data$primary],
#'                                type = "response",
#'                                s = "lambda.min")
#'
#' # flexible group lasso regression
#' object <- cv.corila(x = data$x_train, y = data$y_train,
#'                     group = data$group, primary = data$primary)
#' coef$corila <- stats::coef(object = object)
#' y_hat$corila <- stats::predict(object = object,
#'                                newx = data$x_test[, data$primary])
#'
#' # selection performance (precision: higher = better)
#' vapply(X = coef,
#'        FUN = function(x) {
#'          calc_sign_prec(truth = sign(data$beta), estim = sign(x[-1L]))
#'        },
#'        FUN.VALUE = numeric(1L))
#'
#' # predictive performance (mean squared error: lower = better)
#' vapply(X = y_hat,
#'        FUN = function(x) mean((x - data$y_test)^2.0),
#'        FUN.VALUE = numeric(1L))
#' }

#' \donttest{
#' # simulation
#' set.seed(1L)
#' n0 <- 100
#' n1 <- 10000
#' n <- n0 + n1
#' p <- c(100, 50)
#' z <- rep(x = seq_along(p), times = p)
#' x <- sapply(X = z, FUN = function(x) stats::rnorm(n = n, sd = x))
#' beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'         stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#' eta <- x %*% beta
#' family <- "gaussian"
#' if (identical(family, "gaussian")) {
#'   y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'   y <- survival::Surv(time = time, event = status)
#' }
#' cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#' y_hat <- coef <- list()
#'
#' # standard lasso regression
#' object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                             family = family, alpha = 1)
#' coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#' y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                                type = "response", s = "lambda.min")
#'
#' # flexible group lasso regression
#' object <- cv.corila(x = x[cond, ], y = y[cond], group = z, family = family)
#' coef$corila <- stats::coef(object = object)
#' y_hat$corila <- stats::predict(object = object, newx = x[!cond, ])
#'
#' # selection performance
#' sapply(coef, function(x) mean(sign(x[-1]) == sign(beta)))
#' sapply(coef, function(x) {
#'   sum(sign(x[-1]) != 0 & sign(x[-1]) == sign(beta)) / sum(x[-1] != 0.0)
#' })
#'
#' # predictive performance
#' if (identical(family, "gaussian")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     mean((x-y[!cond])^2))
#' } else if (identical(family, "binomial")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     pROC::auc(response = y[!cond],
#'               predictor = as.vector(x),
#'               levels = c(0L, 1L),
#'               direction = "<"))
#' } else if (identical(family, "cox")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     survival::concordance(y[!cond]~I(-x))$concordance)
#' }
#' metric
#'
#' # privileged information
#' #primary <- stats::rbinom(n = sum(p), size = 1L, prob = 0.5) == 1L
#' #object <- cv.corila(x = x[cond, ], y = y[cond], group = z,
#' #                     primary = primary, family = family)
#' }


#' \donttest{
#' # simulation
#' set.seed(1)
#' n0 <- 100
#' n1 <- 10000
#' n <- n0 + n1
#' p <- c(100, 50)
#' group <- rep(x = seq_along(p), times = p)
#' x <- sapply(X = group, FUN = function(x) stats::rnorm(n = n, sd = x))
#' beta <- stats::rnorm(n = sum(p), mean = 1, sd = 0) *
#'         stats::rbinom(n = sum(p), size = 1, prob = 0.2)
#' eta <- x %*% beta
#' family <- "gaussian"
#' if (identical(family, "gaussian")) {
#'   y <- eta + 0.5 * stats::rnorm(n = n, sd = stats::sd(eta))
#' } else if (identical(family, "binomial")) {
#'   y <- stats::rbinom(n = n, size = 1, prob = 1 / (1 + exp(-eta)))
#' } else if (identical(family, "cox")) {
#'   time <- stats::rexp(n = n, rate = exp(eta))
#'   status <- stats::rbinom(n = n, prob = 0.5, size = 1)
#'   y <- survival::Surv(time = time, event = status)
#' }
#' cond <- rep(x = c(TRUE, FALSE), times = c(n0, n1))
#'
#' y_hat <- coef <- list()
#'
#' # standard ridge regression
#' object <- glmnet::cv.glmnet(x = x[cond, ], y = y[cond],
#'                            family = family, alpha = 0)
#' coef$glmnet <- stats::coef(object = object, s = "lambda.min")
#' y_hat$glmnet <- stats::predict(object = object, newx = x[!cond, ],
#'                               type = "response", s = "lambda.min")
#'
#' # multi-penalty ridge regression
#' object <- multiridge(x = x[cond, ], y = y[cond],
#'                      group = group, family = family)
#' coef$multiridge <- stats::coef(object = object)
#' y_hat$multiridge <- stats::predict(object = object, newx = x[!cond, ])
#'
#' # estimation performance
#' sapply(coef, function(x) stats::cor(beta, x[-1]))
#' sapply(coef, function(x) mean((beta-x[-1])^2))
#'
#' # predictive performance
#' if (identical(family, "gaussian")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     mean((x-y[!cond])^2))
#' } else if (identical(family, "binomial")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     pROC::auc(response = y[!cond],
#'               predictor = as.vector(x),
#'               levels = c(0, 1),
#'               direction = "<"))
#' } else if (identical(family, "cox")) {
#'   metric <- sapply(X = y_hat, FUN = function(x)
#'     survival::concordance(y[!cond]~I(-x))$concordance)
#' }
#' metric
#' }


#----- simulation (used for general comparison in manuscript) -----

#' @title
#' Data simulation
#'
#' @description
#' Simulates data with grouped predictor variables.
#'
#' @param family
#' character `"gaussian"`, `"binomial"`, `"poisson"`, or `"cox"`
#'
#' @param n0
#' number of training observations
#' (positive integer)
#'
#' @param n1
#' number of testing observations
#' (positive integer)
#'
#' @param n_group
#' number of variable groups
#' (positive integer)
#'
#' @param n_type
#' number of variable types
#' (positive integer)
#'
#' @param size_group
#' size of variable groups (per variable type):
#' integer vector of length `n_type`
#'
#' @param effect_size
#' effect sizes (per variable type):
#' numeric vector of length `n_type`
#'
#' @param corfac_feature
#' decrease of correlation if different variable:
#' scalar in unit interval
#'
#' @param corfac_type
#' decrease of correlation if different type:
#' scalar in unit interval
#'
#' @param corfac_group
#' decrease of correlation if different group:
#' scalar in unit interval
#'
#' @param n_group_causal
#' number of causal groups:
#' integer
#'
#' @param prop_causal
#' proportion of causal features within causal groups:
#' scalar in unit interval
#'
#' @param noise_factor
#' noise factor:
#' numeric scalar
#'
#' @param plot
#' Attempt to visualise effects of and correlation between variables?
#' (`TRUE` or `FALSE`)
#'
#' @param trial
#' logical (groups of negatively correlated subgroups)
#'
#' @return
#' Returns a list with the following slots:
#' - \eqn{n_0 \times p} matrix `x_train`
#' - \eqn{p}-dimensional vector `type`
#' - \eqn{p}-dimensional vector `group`
#' - \eqn{n_0}-dimensional vector `y_train`
#' - \eqn{n_1 \times p} matrix `x_test`
#' - \eqn{n_1}-dimensional vector `y_test`
#' - \eqn{p}-dimensional vector `beta`
#' - data frame `info` with entries
#' \eqn{n_0}, \eqn{n_1}, \eqn{p}, `n_type`,
#' `n_group`, and `family`
#'
#' @examples
#' data <- corila:::simulate()
#' dims <- function(x) {
#'    if (is.matrix(x)||is.data.frame(x)) {
#'      paste(base::dim(x), collapse = " x ")
#'    } else {
#'      paste0(base::length(x))
#'    }
#' }
#' sapply(X = data, FUN = dims)
#'
#' @keywords internal
#'
simulate <- function(family = "gaussian", n0 = 100L, n1 = 10000L, n_group = 20L,
                     n_type = 2L, size_group = c(5L, 3L),
                     effect_size = c(1.0, 1.0),
                     corfac_feature = 0.5, corfac_type = 0.5,
                     corfac_group = 0.25, n_group_causal = 2L,
                     prop_causal = 0.5, noise_factor = 1.0,
                     plot = FALSE, trial = FALSE) {
  # --- check arguments ---
  .assert(x = family, type = "nominal",
          support = c("gaussian", "binomial", "poisson", "cox"))
  .assert(x = n0, type = "integer", min = 2L)
  n0 <- as.integer(n0)
  .assert(x = n1, type = "integer", min = 2L)
  n1 <- as.integer(n1)
  .assert(x = n_group, type = "integer", min = 2L)
  n_group <- as.integer(n_group)
  .assert(x = n_type, type = "integer", min = 2L)
  n_type <- as.integer(n_type)
  .assert(x = size_group, type = "integer", dim = n_type, min = 1L)
  size_group <- as.integer(size_group)
  .assert(x = effect_size, type = "numeric", dim = n_type, min = 0.0)
  .assert(x = corfac_feature, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = corfac_type, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = corfac_group, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = n_group_causal, type = "integer", min = 0.0, max = n_group)
  n_group_causal <- as.integer(n_group_causal)
  .assert(x = prop_causal, type = "numeric", min = 0.0, max = 1.0)
  .assert(x = noise_factor, type = "numeric", min = 0.0)
  .assert(x = plot, type = "logical")
  .assert(x = trial, type = "logical")
  # family = "gaussian";n0 = 100;n1 = 10000;n_group = 20;n_type = 2;
  # size_group = c(5, 3);effect_size = c(1, 1);corfac_feature = 0.5;
  # corfac_type = 0.5;corfac_group = 0.25;n_group_causal = 2;
  # prop_causal = 0.5; noise_factor = 1; plot = TRUE
  n <- n0 + n1
  #if (n_type != length(size_group)) {
  #  stop("Wrong length.")
  #}
  #- - - feature modalities and groups - - -
  p <- sum(n_group * size_group)
  if (!trial) {
    type <- rep(x = seq_len(n_type),
                times = n_group * size_group) # original
    group <- unlist(
      lapply(
        X = size_group,
        FUN = function(x) rep(x = seq_len(n_group), each = x)
      )
    ) # original
  } else {
    group <- rep(x = seq_len(n_group),
                 each = sum(size_group)) # trial 2025-09-22
    type <- rep(x = rep(x = seq_len(n_type), times = size_group),
                times = n_group) # trial 2025-09-22
  }
  #- - - effect vector - - -
  beta <- rep(x = 0.0, times = p)
  index_common <- sample(x = seq_len(n_group), size = n_group_causal)
  cond <- group %in% index_common
  var_binom <- stats::rbinom(n = sum(cond), size = 1L, prob = prop_causal)
  var_norm <- abs(stats::rnorm(n = sum(cond)))
  beta[cond] <- var_binom * var_norm
  if (!trial) {
    beta <- beta * rep(x = effect_size, times = table(type))
    # NB: original, added on 2025-06-20
  } else {
    for (i in seq_along(unique(type))) { # trial 2025-09-22
      beta[type == i] <- beta[type == i] * effect_size[i] # trial 2025-09-22
    } # trial 2025-09-22
  }
  if (plot) {
    tryCatch(expr = graphics::plot(x = beta, col = group, pch = type),
             error = function(x) NULL)
  }
  #- - - feature matrix - - -
  mean <- rep(x = 0.0, times = p)
  sigma <- matrix(data = NA, nrow = p, ncol = p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (!trial) {
        sigma[i, j] <- corfac_feature^(i != j) *
          corfac_type^(type[i] != type[j]) *
          corfac_group^(group[i] != group[j]) # original
      } else {
        sigma[i, j] <- ifelse(i == j, 1.0, ifelse(group[i] == group[j] & type[i] == type[j], 0.5, ifelse(group[i] == group[j], -0.25, ifelse(type[i] == type[j], 0.125, -0.125)))) # Consider not only + but also - (but then use + and - for effect sizes), was -0.0625 MAKE THIS LINE SHORTER USING IF ELSE STATEMENTS # nolint: line_length_linter.
      }
    }
  }
  if (any(diag(sigma) != 1.0)) {
    stop("diagonal != 1")
  }
  if (plot) {
    tryCatch(graphics::image(x = sigma[, rev(seq_len(p))]),
             error = function(x) NULL)
  }
  x <- mvtnorm::rmvnorm(n = n, mean = mean, sigma = sigma)
  #- - - target vector - - -
  eta <- scale(x %*% as.vector(beta)) # was without scale
  if (identical(family, "gaussian")) {
    y <- eta + noise_factor * stats::rnorm(n = n, sd = stats::sd(eta))
    # NB: decrease/increase noise?
    if (stats::sd(y) == 0.0) {
      warning("Replacing constant y by random noise.")
      y <- stats::rnorm(n = n)
    }
  } else if (identical(family, "binomial")) {
    y <- stats::rbinom(n = n, size = 1L, prob = 1.0 / (1.0 + exp(-2.0 * eta)))
    # NB: was without 2*
  } else if (identical(family, "cox")) {
    time <- stats::rexp(n = n, rate = exp(eta))
    status <- stats::rbinom(n = n, size = 1L, prob = 0.5)
    #y <- cbind(time = time, status = status)
    y <- survival::Surv(time = time, event = status)
  } else if (identical(family, "poisson")) {
    y <- stats::rpois(n = n, lambda = exp(eta))
  }
  #- - - outputs - - -
  fold <- rep(x = c(0L, 1L), times = c(n0, n1))
  x_train <- x[fold == 0L, ]
  y_train <- y[fold == 0L]
  x_test <- x[fold == 1L, ]
  y_test <- y[fold == 1L]
  info <- data.frame(n0 = n0,
                     n1 = n1,
                     p = p,
                     n_type = n_type,
                     n_group = n_group,
                     family = family)
  list(x_train = x_train,
       type = type,
       group = group,
       y_train = y_train,
       x_test = x_test,
       y_test = y_test,
       beta = beta,
       info = info)
}

#----- comparison -----


#' @title
#' Comparison with hold-out
#'
#' @description
#' Compares methods using hold-out method
#'
#' @inheritParams cv.corila
#'
#' @param x_train
#' \eqn{n_0 \times p} matrix
#'
#' @param y_train
#' \eqn{n_0}-dimensional vector
#'
#' @param x_test
#' \eqn{n_1 \times p} matrix
#'
#' @param y_test
#' \eqn{n_1}-dimensional vector
#'
#' @param method
#' character vector listing all methods to be compared
#'
#' @param nfolds
#' number of internal cross-validation folds (integer scalar)
#'
#' @param foldid
#' internal cross-validation fold identifiers
#' (\eqn{n_0}-dimensional integer vector)
#'
#' @param seed
#' random seed (integer scalar) for reproducibility, or \code{NULL}
#'
#' @return
#' Returns a list with the following slots:
#' \itemize{
#' \item \eqn{n_1}-dimensional vector \code{y_hat}
#' containing predicted values
#' \item \eqn{p}-dimensional vector \code{coef}
#' containing estimated coefficients
#' \item numerical vector \code{difftime}
#' indicating the computation time of each \code{method}
#' }
#'
#' @examples
#' \donttest{
#' data <- simulate()
#' results <- holdout(x_train = data$x_train,
#'                    y_train = data$y_train,
#'                    group = data$group,
#'                    primary = rep(c(TRUE, FALSE), each = 80),
#'                    x_test = data$x_test,
#'                    y_test = data$y_test,
#'                    family = data$info$family,
#'                    method = c("mean", "ridge", "lasso", "corila"))
#' # Why does holdout require y_test? Try to remove this
#' }
#'
#' @keywords iteration
#'
#' @export
holdout <- function(x_train, y_train, group, primary, family,
                    alpha_init = 0, alpha_final = 1,
                    x_test = NULL, y_test = NULL,
                    nfolds = 10, foldid = NULL, method = NULL,
                    seed = NULL, tune = "both") {
  # nfolds <- 10; foldid <- NULL; seed <- NULL
  
  if (!is.null(primary) && any(primary == 0) && !is.numeric(group)) {
    stop(paste0("Function holdout is not fully implemented",
                "for privileged learning with overlapping groups."))
  }
  
  p <- ncol(x_train)
  #n0 <- nrow(x_train)
  n1 <- nrow(x_test)
  
  if (is.null(primary)) {
    primary <- rep(x = TRUE, times = p)
  }
  
  if (is.null(x_test) != is.null(y_test)) {
    stop("Provide either both or none of x_test and y_test.")
  }
  
  if (is.null(foldid)) {
    #foldid <- sample(rep(x = seq_len(nfolds), length.out = n0))
    # balanced/stratified folds:
    foldid <- .folds(y = y_train, family = family, nfolds = nfolds)
  }
  
  if (is.null(method)) {
    if (is.numeric(group)) {
      method <- c("mean", "ridge", "multiridge", "lasso", "gglasso", "grpreg",
                  "sparsegl", "SGL", "graper", "grpregOverlap", "scoop",
                  "ecpc", "squeezy", "MLGL", "pcLasso", "corila")
      # multiview is not for groups (only modalities)
    } else if (is.list(group)) {
      method <- c("mean", "ridge", "lasso", "grpregOverlap",
                  "ecpc", "squeezy", "corila")
      # overlapping groups (multiridge could also be adapted)
    }
    warning("omitting slow methods ...")
    method <- method[!method %in% c("SGL", "ecpc", "squeezy", "scoop")]
    #method <- method[method != "pcLasso"] # bug in application (singletons?)
  }
  
  if (!is.null(x_test)) {
    y_hat <- lapply(X = method, FUN = function(x) rep(x = NA, times = n1))
    names(y_hat) <- method
  } else {
    y_hat <- NULL
  }
  coef <- lapply(X = method, FUN = function(x) rep(x = NA, times = p + 1))
  names(coef) <- method
  #y_hat <- coef <- list()
  
  difftime <- numeric()
  
  for (i in method) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    start <- Sys.time()
    if (i == "mean") {
      #--- prediction by the mean ---
      if (!is.null(x_test)) {
        y_hat$mean <- rep(x = mean(y_train), times = n1)
      }
      if (family == "cox") {
        warning("Implement intercept-only model for Cox regression.")
      }
      coef$mean <- c(ifelse(test = family == "binomial",
                            yes = log(mean(y_train) / (1 - mean(y_train))),
                            no = ifelse(test = family == "poisson",
                                        yes = log(mean(y_train)),
                                        no = mean(y_train))),
                     rep(x = 0, times = sum(primary)))
    } else if (i == "ridge") {
      #--- ridge ---
      object <- glmnet::cv.glmnet(x = x_train[, primary],
                                  y = y_train,
                                  family = family,
                                  alpha = 0,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$ridge <- stats::predict(object = object,
                                      newx = x_test[, primary],
                                      s = "lambda.min",
                                      type = "response")
      }
      coef$ridge <- c(NA[family == "cox"],
                      as.numeric(stats::coef(object = object,
                                             s = "lambda.min")))
    } else if (i == "multiridge") {
      if (family == "poisson") {
        next
      }
      #--- multiridge ---
      object <- multiridge(x = x_train[, primary],
                           y = y_train,
                           z = group[primary],
                           family = family)
      if (!is.null(x_test)) {
        y_hat$multiridge <- stats::predict(object = object,
                                           newx = x_test[, primary])
      }
      coef$multiridge <- stats::coef(object = object)
    } else if (i == "lasso") {
      #--- lasso ---
      object <- glmnet::cv.glmnet(x = x_train[, primary],
                                  y = y_train,
                                  family = family,
                                  alpha = 1,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$lasso <- stats::predict(object = object,
                                      newx = x_test[, primary],
                                      s = "lambda.min",
                                      type = "response")
      }
      coef$lasso <- c(NA[family == "cox"],
                      as.numeric(stats::coef(object = object,
                                             s = "lambda.min")))
    } else if (i == "gglasso") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      #--- group lasso (gglasso) ---
      if (family == "binomial") {
        temp_y_train <- 2 * y_train - 1
        temp_loss <- "logit"
      } else {
        temp_y_train <- y_train
        temp_loss <- "ls"
      }
      object <- gglasso::cv.gglasso(x = x_train[, primary],
                                    y = temp_y_train,
                                    loss = temp_loss,
                                    group = group[primary],
                                    foldid = foldid)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, primary],
                               s = "lambda.min",
                               type = "link")
        if (family == "binomial") {
          y_hat$gglasso <- 1 / (1 + exp(-temp))
          #} else if (family == "poisson") {
          #  y_hat$gglasso <- exp(temp)
        } else {
          y_hat$gglasso <- temp
        }
      }
      coef$gglasso <- stats::coef(object, s = "lambda.min")
    } else if (i == "grpreg") {
      #if (family == "cox") {next}
      #--- group lasso (grpreg) ---
      if (family == "cox") {
        object <- grpreg::cv.grpsurv(X = x_train[, primary],
                                     y = y_train,
                                     group = group[primary],
                                     fold = foldid)
      } else {
        object <- grpreg::cv.grpreg(X = x_train[, primary],
                                    y = y_train,
                                    family = family,
                                    group = group[primary],
                                    fold = foldid)
      }
      if (!is.null(x_test)) {
        y_hat$grpreg <- stats::predict(object = object,
                                       X = x_test[, primary],
                                       type = "response",
                                       lambda = object$lambda.min)
      }
      coef$grpreg <- c(NA[family == "cox"],
                       as.numeric(stats::coef(object = object,
                                              lambda = object$lambda.min)))
    } else if (i == "grplasso") {
      #--- group lasso (grplasso) ---
      ## This package requires the user to implement hyperparameter tuning.
      # if (family == "cox") {next}
      # if (family == "gaussian") {
      #   model <- grplasso::LinReg()
      # } else if (family == "binomial") {
      #   model <- grplasso::LogReg()
      # } else if (family == "poisson") {
      #   model <- grplasso::PoissReg()
      # }
      # lambda <- grplasso::lambdamax(x = cbind(1, x_train[, primary]),
      # y = y_train, index = c(NA, group[primary]),
      # penscale = base::sqrt, model = model) * 0.9^(0:100)
      # object <- grplasso::grplasso(x = cbind(1, x_train[, primary]),
      #                              y = y_train,
      #                              index = c(NA, group[primary]),
      #                              model = model,
      #                              lambda = lambda,
      # control = grplasso::grpl.control(update.hess = "lambda", trace = 0))
      # if (!is.null(x_test)) {
      #   y_hat$grplasso <- stats::predict(object = object,
      # newdata = cbind(1, x_test[, primary]), type = "response")
      # }
      # coef$grplasso <- object$coefficients[, 1]
    } else if (i == "sparsegl") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      #--- sparse group lasso (sparsegl) ---
      object <- sparsegl::cv.sparsegl(x = x_train[, primary],
                                      y = y_train,
                                      group = group[primary],
                                      family = family,
                                      foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$sparsegl <- stats::predict(object = object,
                                         newx = x_test[, primary],
                                         type = "response",
                                         s = "lambda.min")
      }
      coef$sparsegl <- stats::coef(object, s = "lambda.min")
    } else if (i == "SGL") {
      if (family == "poisson") {
        next
      }
      #--- sparse group lasso (SGL) ---
      family_temp <- ifelse(test = family == "gaussian",
                            yes = "linear",
                            no = ifelse(test = family == "binomial",
                                        yes = "logit",
                                        no = family))
      if (family == "cox") {
        data_temp <- list(x = x_train[, primary],
                          time = as.matrix(y_train)[, "time"],
                          status = as.matrix(y_train)[, "status"])
      } else {
        data_temp <- list(x = x_train[, primary], y = y_train)
      }
      cv_object <- SGL::cvSGL(data = data_temp,
                              index = group[primary],
                              type = family_temp,
                              foldid = foldid)
      object <- SGL::SGL(data = data_temp,
                         index = group[primary],
                         type = family_temp,
                         lambdas = cv_object$lambdas)
      if (!is.null(x_test)) {
        y_hat$SGL <- SGL::predictSGL(x = object,
                                     newX = x_test[, primary],
                                     lam = which.min(cv_object$lldiff))
      }
      if (family == "gaussian") {
        coef$SGL <- c(object$intercept,
                      object$beta[, which.min(cv_object$lldiff)])
      } else {
        coef$SGL <- c(object$intercept[which.min(cv_object$lldiff)],
                      object$beta[, which.min(cv_object$lldiff)])
      }
    } else if (i == "graper") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      invisible(utils::capture.output(
        object <- suppressMessages(graper::graper(
          X = x_train[, primary],
          y = y_train,
          annot = as.factor(group[primary]),
          family = family
        ))
      ))
      if (!is.null(x_test)) {
        y_hat$graper <- stats::predict(object = object,
                                       newX = x_test[, primary],
                                       type = "response")
      }
      coef$graper <- stats::coef(object = object)
    } else if (i == "grpregOverlap") {
      #--- grpregOverlap (only on GitHub) ---
      func <- grpregOverlap::expandX
      body(func)[[3]] <- quote(
        over_mat <- Matrix(incidence.mat %*% t(incidence.mat), sparse = TRUE)
      )
      utils::assignInNamespace(x = "expandX",
                               value = func,
                               ns = "grpregOverlap")
      if (is.numeric(group)) {
        list <- c(lapply(X = unique(group[primary]),
                         FUN = function(z) which(group[primary] == z)))
        #lapply(X = unique(type[primary]),
        # FUN = function(z) which(type[primary]  == z))
      } else {
        list <- group
      }
      if (family == "cox") {
        object <- grpregOverlap::cv.grpsurvOverlap(X = x_train[, primary],
                                                   y = y_train,
                                                   group = list)
      } else {
        object <- grpregOverlap::cv.grpregOverlap(X = x_train[, primary],
                                                  y = y_train,
                                                  group = list,
                                                  family = family)
      }
      if (!is.null(x_test)) {
        y_hat$grpregOverlap <- stats::predict(object = object,
                                              X = x_test[, primary],
                                              type = "response",
                                              lambda = object$lambda.min)
      }
      coef$grpregOverlap <- c(if (family == "cox") NA,
                              stats::coef(object = object,
                                          lambda = object$lambda.min))
    } else if (i == "multiview") {
      #--- multiview (agreement between different modalities) ---
      object <- list()
      if (family == "gaussian") {
        temp <- stats::gaussian()
      } else if (family == "binomial") {
        temp <- stats::binomial()
      } else if (family == "poisson") {
        temp <- stats::poisson()
      }
      #rho <- c(0.00, 0.10, 0.25, 0.50, 1.00)
      #for (j in seq_along(rho)) {
      #  object[[j]] <- multiview::cv.multiview(
      # x_list = lapply(X = unique(type[primary]),
      # FUN = function(z) x_train[, type[primary] == z]),
      # y = y_train, family = temp, rho = rho[j], foldid = foldid)
      #}
      #id <- which.min(sapply(object, function(x) min(x$cvm)))
      #if (!is.null(x_test)) {
      #  y_hat$multiview <- stats::predict(object = object[[id]],
      # newx = lapply(X = unique(type[primary]),
      # FUN = function(z) x_test[, type[primary] == z]),
      # type = "response", s = "lambda.min")
      #}
      #coef$multiview <- stats::coef(object = object[[id]], s = "lambda.min")
    } else if (i == "scoop") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      if (all(table(group[primary]) == 1)) {
        #group_temp <- rep(x = 1, times = length(group))
        object <- scoop::coop.lasso(x = x_train[, primary],
                                    y = y_train,
                                    group = group,
                                    family = family)
      } else {
        object <- scoop::sparse.coop.lasso(x = x_train[, primary],
                                           y = y_train,
                                           group = group[primary],
                                           family = family)
      }
      object_cv <- scoop::crossval(object)
      id <- which(object_cv@lambda == object_cv@lambda.min)
      if (!is.null(x_test)) {
        y_hat$scoop <- scoop::predict(object = object,
                                      newx = x_test[, primary])[, id]
      }
      coef$scoop <- object_cv@beta.min
    } else if (i == "MLGL") {
      #--- multi-layer group-lasso ---
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      loss <- ifelse(family == "gaussian", "ls", "logit")
      if (loss == "logit") {
        y_train_temp <- 2 * y_train - 1
      } else {
        y_train_temp <- y_train
      }
      cv <- MLGL::cv.MLGL(X = x_train[, primary],
                          y = y_train_temp,
                          loss = loss)
      object <- MLGL::MLGL(X = x_train[, primary],
                           y = y_train_temp,
                           loss = loss)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, primary],
                               type = "fit",
                               s = cv$lambda.min)
        if (loss == "ls") {
          y_hat$MLGL <- temp
        } else {
          y_hat$MLGL <- 1 / (1 + exp(-temp))
        }
      }
      coef$MLGL <- stats::coef(object = object, s = cv$lambda.min)
    } else if (i == "ecpc") {
      if (family == "poisson") {
        next
      }
      if (family == "cox") {
        y_temp <- y_train
      } else {
        y_temp <- matrix(y_train, ncol = 1)
      }
      #--- ecpc ---
      model <- ifelse(test = family == "gaussian",
                      yes = "linear",
                      no = ifelse(test = family == "binomial",
                                  yes = "logistic",
                                  no = family))
      if (is.numeric(group[primary])) {
        invisible(utils::capture.output(
          groupset <- ecpc::createGroupset(values = as.factor(group[primary]))
        ))
      } else if (is.list(group)) {
        base <- lapply(group, function(x) as.integer(x))
        # first alternative
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)]
        # second alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])
        groupset <- c(base, extra)
      }
      #invisible(utils::capture.output(
      #  typeset <- ecpc::createGroupset(values = as.factor(type))
      #))
      #datablocks <- lapply(X = unique(type[primary]),
      #                     FUN = function(x) which(type[primary] == x))
      invisible(
        tryCatch(
          utils::capture.output(
            object <- ecpc::ecpc(
              Y = y_temp,
              X = x_train[, primary],
              groupsets = list(groupset),
              X2 = x_test[, primary],
              model = model,
              fold = nfolds,
              datablocks = NULL
            )
          ),
          error = function(x) NULL
        )
      )
      # Currently typeset/datablocks is ignored!
      if (!is.null(object)) {
        coef$ecpc <- unlist(stats::coef(object))
      }
      if (!is.null(object) && !is.null(x_test)) {
        y_hat$ecpc <- object$Ypred
      }
    } else if (i == "gren") {
      #partitions <- list(group = lapply(X = unique(group),
      #                                  FUN = function(x) which(group == x)),
      #                   type = lapply(X = unique(type),
      #                                 FUN = function(x) which(type == x)))
      #object <- gren::cv.gren(x = x_train[, primary],
      #                        y = y_train,
      #                        partitions = list(group = group, type = type),
      #                        trace = TRUE)
      warning("Implement GREN.")
    } else if (i == "squeezy") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      if (is.numeric(group)) {
        groupset <- lapply(X = unique(group[primary]),
                           FUN = function(x) which(group[primary] == x))
      } else if (is.list(group)) {
        base <- lapply(group, function(x) as.integer(x))
        # 1st alternative
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)]
        # 2nd alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])
        groupset <- c(base, extra)
      }
      object <- squeezy::squeezy(Y = y_train,
                                 X = x_train[, primary],
                                 groupset = groupset,
                                 X2 = x_test[, primary])
      # Check whether type can be included (e.g., as groups).
      y_hat$squeezy <- object$YpredApprox
      coef$squeezy <- c(object$a0Approx, object$betaApprox)
    } else if (i == "CBPE") {
      #if (FALSE) {
      #  n <- 100
      #  p <- 50
      #  x_train <- matrix(rnorm(n * p), n, p)
      #  beta_true <- c(0.5, -1, 2, 5, rep(0, times = p - 4))
      #  y_train <- rbinom(n, 1, 1 / (1 + exp(-x_train %*% beta_true)))
      #}
      #if (family == "gaussian") {
      #  cbpe <- CBPE::CBPLinearE
      #} else if (family == "binomial") {
      #  cbpe <- CBPE::CBPLogisticE
      #} else {
      #  next
      #}
      #lambda <- exp(seq(from = log(1e06), to = log(1e-06), length.out = 20))
      # no predict function implemented
      #for (i in seq_len(nfolds)) {
      #  coef <- CBPE(X = x_train[foldid !=  i, primary],
      #               y = y_train[foldid != i],
      #               lambda = 0)
      #  x_train[foldid == i, primary] %*% coef
      #}
      # internal cross-validation to tune lambda
      # refit on full training data with optimal lambda
      stop("Not yet implemented.")
    } else if (i == "pcLasso") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      group_temp <- lapply(X = unique(group[primary]),
                           FUN = function(x) which(x == group[primary]))
      # duplicating singletons:
      .duplicate_singletons <- function(x) {
        if (length(x) == 1) {
          rep(x, times = 2)
        } else {
          x
        }
      }
      group_temp <- lapply(X = group_temp, FUN = .duplicate_singletons)
      # combining remaining features
      indices <- seq_len(ncol(x_train[, primary]))
      extra <- indices[!indices %in% unlist(group_temp)]
      if (length(extra) > 0) {
        group_temp <- c(group_temp, extra)
      }
      #group_temp <- c(group_temp[!cond], list(unlist(group_temp[cond])))
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      ratio <- c(seq(from = 0.25, to = 0.75, by = 0.25), 0.9, 0.95, 1)
      # NB: set is from paper
      object <- list()
      for (j in seq_along(ratio)) {
        invisible(utils::capture.output(
          object[[j]] <- pcLasso::cv.pcLasso(x = x_train[, primary],
                                             y = y_train,
                                             family = family,
                                             groups = group_temp,
                                             ratio = ratio[j])
        ))
      }
      id <- which.min(vapply(X = object,
                             FUN = function(x) min(x$cvm),
                             FUN.VALUE = numeric(1)))
      object <- object[[id]]
      if (!is.null(x_test)) {
        y_hat$pcLasso <- pcLasso::predict.cv.pcLasso(object = object,
                                                     xnew = x_test[, primary],
                                                     s = "lambda.min")
      }
      coef$pcLasso <- c(
        object$glmfit$a0[which(object$lambda == object$lambda.min)],
        object$glmfit$beta[, which(object$lambda == object$lambda.min)]
      )
      # Under overlapping groups, use object$glmfit$origbeta.
    } else if (i == "corila") {
      #--- lasso with feature groups and modalities ---
      object <- cv.corila(x = x_train,
                          y = y_train,
                          group = group,
                          primary = primary,
                          alpha_init = alpha_init,
                          alpha_final = alpha_final,
                          family = family,
                          foldid = foldid,
                          tune = tune)
      print(object$hyper[object$id_hyper, ])
      if (!is.null(x_test)) {
        y_hat$corila <- stats::predict(object = object,
                                       newx = x_test[, primary])
      }
      coef$corila <- stats::coef(object = object)
    }
    end <- Sys.time()
    difftime[i] <- difftime(time1 = end, time2 = start, units = "secs")
  }
  #- - - checks - - -
  if (family != "cox") {
    method <- names(y_hat)
    if (!is.null(x_test)) {
      if (family == "binomial") {
        if (min(unlist(y_hat), na.rm = TRUE) < 0) {
          stop("too small")
        }
        if (max(unlist(y_hat), na.rm = TRUE) > 1) {
          stop("too large")
        }
      }
      for (i in seq_along(method)) {
        if (method[i] == "pcLasso") {
          next
        }
        original <- y_hat[[i]]
        if (all(is.na(original))) {
          next
        }
        eta <- coef[[i]][1] + x_test[, primary] %*% coef[[i]][-1]
        if (family %in% c("gaussian", "cox")) {
          manual <- eta
        } else if (family == "binomial") {
          manual <- 1 / (1 + exp(-eta))
        } else if (family == "poisson") {
          manual <- exp(eta)
        }
        #cond <- is.na(original)|is.na(manual)
        #if (any(cond)) {
        #  message("coef:", paste(head(coef[[i]]), collapse = " "))
        #  message("original:", paste(head(original), collapse = " "))
        #  message("manual:", paste(head(manual), collapse = " "))
        #}
        #message("original: ", paste0(original[cond], collapse = " "))
        #message("manual: ", paste0(manual[cond], collapse = " "))
        if (any(abs(original - manual) > 0.001)) {
          warning(paste("unequal:", method[i]))
        }
        if (stats::sd(original) != 0 &&
            stats::sd(manual) != 0 &&
            stats::cor(original, manual) < 0.999) {
          warning(paste("correlation:", method[i]))
        }
      }
    }
    
    if (!is.null(x_test)) {
      range <- range(unlist(y_hat), na.rm = TRUE)
      if (family == "binomial" && (range[1] < 0 || range[2] > 1)) {
        stop("invalid y_hat range")
      }
      if (any(vapply(X = y_hat,
                     FUN = base::length,
                     FUN.VALUE = numeric(1)) != n1)) {
        stop("invalid y_hat length")
      }
    }
    if (any(vapply(X = coef,
                   FUN = base::length,
                   FUN.VALUE = numeric(1)) != sum(primary) + 1)) {
      stop("invalid coef length")
    }
  } else {
    warning("Implement checks for Cox regression.")
  }
  list(y_hat = y_hat, coef = coef, difftime = difftime)
}

#' @title
#' Cross-validation method
#'
#' @description
#' Compares methods with cross-validation method
#'
#' @inheritParams cv.corila
#'
#' @inheritParams holdout
#'
#' @param iter
#' number of cross-validation iterations
#'
#' @param nfolds
#' number of cross-validation folds
#'
#' @param foldid
#' cross-validation folds
#'
#' @details
#' This function implements repeated \eqn{k}-fold cross-validation
#' (e.g., 5 repetitions of 10-fold cross-validation).
#'
#' @return
#' Returns a list with the following slots:
#' \itemize{
#' \item \code{nzero} non-zero coefficients
#' \item \code{metric} metric
#' }
#' Both slot contain a data frame
#' with one row for each iteration (\code{iter})
#' and one column for each \code{method}.
#'
#' @examples
#' \donttest{
#' n <- 100
#' p <- 20
#' x <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- stats::rnorm(n)
#' foldid <- rep(c(0, 1), times = c(50, 50))
#' results <- crossval(x, y, family = "gaussian",
#'                     method = c("mean", "corila"), foldid = foldid)
#' }
#'
#' @keywords iteration
#'
#' @export
crossval <- function(x, y, family, group = NULL, primary = NULL,
                     alpha_init = 0, alpha_final = 1, iter = 5, foldid = NULL,
                     nfolds = 10, method = NULL, tune = "both") {
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(group)) {
    group <- seq_len(p)
  }
  list <- list()
  list$metric <- list$nzero <- list()
  for (k in seq_len(iter)) {
    set.seed(k)
    cat("iter", k, "\n")
    if (is.null(foldid)) {
      #foldid <- sample(rep(x = seq_len(nfolds), length.out = n))
      foldid <- .folds(y = y,
                       family = family,
                       nfolds = nfolds) # balanced/stratified folds
    } else {
      nfolds <- max(foldid)
    }
    y_hat <- data.frame(row.names = seq_len(n))
    for (i in seq_len(nfolds)) {
      set.seed(i)
      cat("fold", i, "\n")
      cond <- foldid == i
      results <- holdout(x_train = x[!cond, ],
                         y_train = y[!cond],
                         x_test = x[cond, ],
                         y_test = y[cond],
                         group = group,
                         primary = primary,
                         alpha_init = alpha_init,
                         alpha_final = alpha_final,
                         family = family,
                         nfolds = 10,
                         foldid = NULL,
                         method = method,
                         seed = NULL,
                         tune = tune)
      for (j in seq_along(results$y_hat)) {
        y_hat[[names(results$y_hat)[j]]][cond] <- results$y_hat[[j]]
      }
    }
    if (family %in% c("gaussian", "poisson")) {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) mean((y[foldid != 0] - x[foldid != 0])^2)
      )
    } else if (family == "binomial") {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) {
          pROC::auc(response = y[foldid != 0],
                    predictor = as.vector(x[foldid != 0]),
                    levels = c(0, 1), direction = "<")
        }
      )
    } else if (family == "cox") {
      list$metric[[k]] <- apply(
        X = y_hat,
        MARGIN = 2,
        FUN = function(x) {
          survival::concordance(y[foldid != 0] ~ I(-x[foldid != 0]))$concordance
        }
      )
    }
    set.seed(k)
    if (nfolds == 1) {
      list$nzero[[k]] <- vapply(X = results$coef,
                                FUN = function(x) sum(x[-1] != 0),
                                FUN.VALUE = numeric(1))
    } else {
      refit <- holdout(x_train = x[foldid != 0, ],
                       y_train = y[foldid != 0],
                       group = group,
                       primary = primary,
                       alpha_init = alpha_init,
                       alpha_final = alpha_final,
                       family = family,
                       nfolds = 10,
                       foldid = NULL,
                       method = method,
                       seed = NULL,
                       tune = tune)
      list$nzero[[k]] <- vapply(X = refit$coef,
                                FUN = function(x) sum(x[-1] != 0),
                                FUN.VALUE = numeric(1))
    }
  }
  list <- lapply(X = list, FUN = function(x) do.call(what = "rbind", args = x))
  list$family <- family
  list
}

.wilcox_test <- function(x, y, ...) {
  if (all(is.na(x)) || all(is.na(y))) {
    NA
  } else {
    stats::wilcox.test(x = x, y = y, ...)$p.value
  }
}

#' @title
#' Custom box plot function
#'
#' @description
#' Creates box plots for paired/matched data,
#' using Wilcoxon's signed-rank test to compare a group with the other groups.
#'
#' @param x
#' data frame with names slots
#'
#' @param base
#' character string naming the slot of interest
#' (e.g., \code{"corila"})
#'
#' @param main
#' character string used as a title
#'
#' @param decrease
#' \code{TRUE} for decreasing arrow,
#' \code{FALSE} for increasing arrow
#'
#' @param ylim
#' limits for the vertical axis, or \code{NULL}
#'
#' @param cex.main
#' numeric
#'
#' @return
#' Returns \code{NULL} (and plots a figure).
#'
#' @examples
#' x <- data.frame(mean = 0, corila = rnorm(100) - 1, other = rnorm(100))
#' .plot_boxes(x)
#'
#' @keywords graphs
#'
#' @export
.plot_boxes <- function(x, base = "corila", main = "", decrease = TRUE,
                       ylim = NULL, cex.main = 1.2) {
  #--- hypothesis testing ---
  pvalue <- list()
  for (i in c("less", "greater")){
    label <- ifelse(decrease == (i == "less"), "better", "worse")
    pvalue[[label]] <- apply(
      X = x,
      MARGIN = 2,
      FUN = function(col) {
        .wilcox_test(x = col,
                     y = x[, base],
                     paired = TRUE,
                     alternative = i,
                     exact = FALSE)
      }
    )
  }
  
  col <- ifelse(test = pvalue$worse <= 0.05,
                yes = "red",
                no = ifelse(test = pvalue$better <= 0.05,
                            yes = "blue",
                            no = "grey"))
  #--- boxplot ---
  graphics::boxplot(x = x,
                    main = main,
                    las = 2,
                    col = col,
                    frame.plot = FALSE,
                    xaxt = "n",
                    yaxt = "n",
                    ylim = ylim,
                    cex.main = cex.main)
  #--- horizontal axis ---
  col <- list(grey = which(pvalue$worse <= 0.05 | is.na(pvalue$worse)),
              black = which(pvalue$worse > 0.05))
  for (i in seq_along(col)) {
    graphics::axis(side = 1,
                   at = seq_len(ncol(x))[col[[i]]],
                   labels = colnames(x)[col[[i]]],
                   las = 2,
                   col.axis = names(col)[i],
                   tick = FALSE,
                   line = -0.5)
  }
  if ("mean" %in% colnames(x)) {
    graphics::abline(h = stats::median(x[, "mean"]), lty = 2, col = "grey")
  }
  #--- vertical axis ---
  usr <- graphics::par("usr")
  mar_big <- 0.05 * (usr[4] - usr[3])
  mar_small <- 0.05 * (usr[4] - usr[3])
  graphics::axis(side = 2, col = "grey", col.axis = "grey")
  graphics::arrows(x0 = usr[1],
                   y0 = usr[3] + mar_big,
                   x1 = usr[1],
                   y1 = usr[4] - mar_big,
                   length = 0.1,
                   xpd = TRUE,
                   code = ifelse(decrease, 1, 2),
                   lwd = 2)
  graphics::text(
    x = usr[1],
    y = c(usr[4] + mar_small, usr[3] - mar_small)[1 + c(!decrease, decrease)],
    labels = c("-", "+"),
    col = c("red", "blue"),
    xpd = TRUE,
    cex = 1.5,
    font = 2
  )
  invisible(NULL)
}

#@title
#Combine variables
#
#@description
#Calculates the mean or the first principal component of a group of variables
#
#@inheritParams construct_matrices
#@param x \eqn{n_0 \times p_k} matrix, where \eqn{n_0}
# is the number of observations used for model training and
# \eqn{p_k} is the number of variables inside a group
#@param fuse character string \code{"mean"}
# for arithmetic mean  or \code{"pca"} for first principal component
#
#@return
#Returns an \eqn{n_0}-dimensional numeric vector.
#
#@seealso
#This function is called by \code{\link{corila}()}
# and thereby \code{\link{cv.corila}()}.
#
#@examples
#n <- 100; p <- 5
#x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
#mean <- combine_features(x = x, fuse = "mean")
#comp <- combine_features(x = x, fuse = "pca")
#plot(mean, comp)
#
#@export
# combine_features <- function(x, fuse = "mean") {
#   if (!fuse %in% c("mean", "pca")) {
#     stop("Argument 'fuse' must equal 'mean' or 'pca'.")
#   }
#   if (fuse == "mean") {
#     rowMeans(x)
#   } else if (fuse == "pca") {
#     stats::princomp(x = x)$scores[, "Comp.1"]
#   }
# }

#@title
#Construct Matrices
#
#@description
#Constructs matrices with
# (i) the original data concatenated with the inverted data,
# (ii) one meta-variable for each group, and
# (iii) one meta-variable for each group in each type.
#
#@param group \eqn{p}-dimensional vector of group labels or indices,
# or list with one slot for each group containing the variable labels or indices
#@param type \eqn{p}-dimensional vector
#@inheritParams corila
#
#@examples
#n <- 5
#p <- 6
#x <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
#group <- rep(1:2, each = p / 2)
#type <- rep(x = 1, times = p)
#x <- construct_matrices(x = x, group = group, type = type)
#
#@seealso
#This function is called by \code{\link{corila}()}
# and thereby \code{\link{cv.corila}()}.
#
#@return
#See description.
#@export
# construct_matrices <- function(x, group, type, fuse = "mean") {
#   if ((is.numeric(group) && ncol(x) != length(group)) |
# ncol(x) != length(type)) {
#     stop("For each variable, the matrix 'x' must have one column,
# and the vectors 'group' (if applicable) and 'type' must have one entry.")
#   }
#   index <- seq_len(ncol(x))
#   n <- nrow(x)
#   if (is.numeric(group)) {
#     q <- length(unique(group))
#   } else {
#     q <- length(group)
#   }
#   m <- length(unique(type))
#   com <- matrix(data = NA, nrow = n, ncol = q,
# dimnames = list(NULL, seq_len(q)))
#   sep <- replicate(n = m, expr = com, simplify = FALSE)
#   for (i in seq_len(m)) {
#     for (j in seq_len(q)) {
#       if (is.numeric(group)) {
#         sep[[i]][, j] <-
# combine_features(x = x[, type== i & group == j, drop = FALSE], fuse = fuse)
#       } else {
#         sep[[i]][, j] <-
# combine_features(x = x[, type == i & index %in% group[[j]], drop = FALSE],
# fuse = fuse)
#       }
#     }
#   }
#   for (j in seq_len(q)) {
#     if (is.numeric(group)) {
#       com[, j] <- combine_features(x = x[, group == j, drop = FALSE],
# fuse = fuse)
#     } else {
#       com[, j] <-
# combine_features(x = x[, index %in% group[[j]], drop = FALSE],
# fuse = fuse)
#     }
#   }
#   list(all = cbind(x, -x), com = com, sep = sep)
# }
