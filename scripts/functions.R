

#' @title
#' Simulation with Privileged Information
#'
#' @description
#' Simulates data for learning using privileged information (LUPI).
#'
#' @param mode
#' character string `"upstream"`, `"aggregated"`, `"surrogate"`, `"baseline"`,
#' or `"uninformative"`
#'
#' @param n0
#' number of observations used for fitting the model
#' ("training samples", integer \eqn{>= 2})
#'
#' @param n1
#' number of observations used for making predictions
#' ("testing samples", integer \eqn{>=2})
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
#' the \eqn{p}-dimensional logical vector `include`
#' indicating primary predictors,
#' and the \eqn{p}-dimensional effect vector `beta`.
#'
#' @keywords internal
#'
#' @examples
#' data <- corila:::.simulate_lupi_data(mode = "baseline")
#'
.simulate_lupi_data <- function(mode, n0 = 100, n1 = 10000, p = 200, q = 4,
                                plot = FALSE) {
  .assert(x = n0, type = "integer", min = 2)
  .assert(x = n1, type = "integer", min = 2)
  .assert(x = p, type = "integer", min = 2)
  .assert(x = q, type = "integer", min = 2)
  .assert(x = mode, type = "nominal",
          support = c("upstream", "aggregated", "surrogate", "baseline"))
  .assert(x = plot, type = "logical")
  fold <- rep(x = c(0, 1), times = c(n0, n1))
  n <- n0 + n1
  if (p %% q != 0) {
    stop("This function simulates equally sized groups.",
         "So `p` must be a multiple of `q`.")
  }
  if (mode == "upstream") {
    #--- upstream and downstream predictors ---
    group <- rep(x = seq_len(p / q),
                 each = q)
    include <- rep(x = rep(x = c(TRUE, FALSE), times = c(1, q - 1)),
                   times = p / q)
    causal <- rep(x = sample(rep(x = c(TRUE, FALSE), times = c(5, p / q - 5))),
                  each = q)
    x <- matrix(data = NA, nrow = n, ncol = p)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & include
      sel_aux <- group == j & !include
      x[, sel_pry] <- stats::rnorm(n = n)
      #w <- c(0.2,0.5,0.8) # original
      w <- stats::runif(3) # trial
      x[, sel_aux] <- x[, sel_pry] %*% t(sqrt(w)) + t(t(matrix(
        stats::rnorm(n * sum(sel_aux)),
        nrow = n,
        ncol = sum(sel_aux)
      )) * sqrt(1 - w))
    }
    beta <- (!include) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "aggregated") {
    #--- fine-grained and aggregated predictors ---
    group <- rep(x = seq_len(p / q), each = q)
    include <- rep(x = rep(x = c(TRUE, FALSE), times = c(1, q - 1)), times =
                     p / q)
    causal <- rep(x = sample(rep(
      x = c(TRUE, FALSE), times = c(5, p / q - 5)
    )), each = q)
    x <- matrix(data = NA,
                nrow = n,
                ncol = p)
    #w <- 0.5
    #w <- c(1/3,1/3,1/3)
    #w <- stats::rgamma(n=3,shape=1,rate=1)
    #w <- w/sum(w)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & include
      sel_aux <- group == j & !include
      w <- stats::runif(n = 1)
      #x[,sel_aux] <- sqrt(w)*stats::rnorm(n=n)+
      #sqrt(1-w)*stats::rnorm(n=n*sum(sel_aux))
      #x[,sel_pry] <- rowSums(x[,sel_aux])
      x[, sel_aux] <- stats::rnorm(n = n * sum(sel_aux))
      w <- stats::runif(n = 4)
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
    beta <- (!include) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "surrogate") {
    #--- canonical and surrogate predictors ---
    group <- rep(x = seq_len(p / q), each = q)
    include <- rep(x = rep(x = c(FALSE, TRUE), times = c(1, q - 1)), times =
                     p / q)
    causal <- rep(x = sample(rep(
      x = c(TRUE, FALSE), times = c(5, p / q - 5)
    )), each = q)
    x <- matrix(data = NA,
                nrow = n,
                ncol = p)
    for (j in seq_len(p / q)) {
      sel_pry <- group == j & include
      sel_aux <- group == j & !include
      x[, sel_aux] <- stats::rnorm(n = n)
      # w <- c(0.2,0.5,0.8) # TRIAL
      # was c(0.7,0.5,0.3)#  stats::runif(n=q-1) # rep(x=0.9,times=q-1)
      w <- stats::runif(3)
      x[, sel_pry] <- x[, sel_aux] %*% t(sqrt(w)) + t(t(matrix(
        stats::rnorm(n * sum(sel_pry)),
        nrow = n,
        ncol = sum(sel_pry)
      )) * sqrt(1 - w))
    }
    beta <- (!include) * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  } else if (mode == "baseline") {
    #--- baseline and follow-up predictors ---
    w <- c(NA, 0.9, 0.9, 0.9)
    #w <- c(NA,runif(3)) # TRIAl
    list <- list()
    list[[1]] <- matrix(
      data = stats::rnorm(n = n * p / q),
      nrow = n,
      ncol = p / q
    )
    for (j in seq(from = 2, to = q)) {
      list[[j]] <- sqrt(w[j]) * list[[j - 1]] +
        sqrt(1 - w[j]) * stats::rnorm(n = n * p / q)
    }
    x <- do.call(what = "cbind", args = list)
    group <- rep(x = seq_len(p / q), times = q)
    include <- rep(x = c(TRUE, FALSE), times = c(p / q, p / q * (q - 1)))
    beta <- sample(rep(x = c(0, 1), times = c(p / q - 5, 5))) * 
      abs(stats::rnorm(n = p / q))
    causal <- NULL
    eta <- list[[length(list)]] %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * sd(eta))
  } else if (mode == "uninformative") {
    group <- rep(x = seq_len(p / q), each = q)
    include <- rep(x = rep(x = c(TRUE, FALSE), times = c(1, q - 1)),
                   times = p / q)
    causal <- rep(x = sample(rep(x = c(TRUE, FALSE), times = c(5, p / q - 5))),
                  each = q)
    x <- matrix(data = stats::rnorm(n = n * p), nrow = n, ncol = p)
    beta <- include * causal * abs(stats::rnorm(n = p))
    eta <- x %*% beta
    y <- eta + stats::rnorm(n = n, sd = 0.5 * stats::sd(eta))
  }
  # else if(mode=="adversarial"){
  #   group <- rep(x=seq_len(p/q),each=q)
  #   include <- rep(x=rep(x=c(TRUE,FALSE),times=c(1,q-1)),times=p/q)
  #   causal <- rep(x=sample(rep(x=c(TRUE,FALSE),times=c(5,p/q-5))),each=q)
  #   x <- matrix(data=NA,nrow=n,ncol=p)
  #   for(j in seq_len(p/q)){
  #     sel.pry <- group==j & include
  #     sel.aux <- group==j & !include
  #     x[,sel.pry] <- stats::rnorm(n=n)
  #     w <- stats::runif(3)
  #     x[,sel.aux] <- x[,sel.pry] %*% t(sqrt(w)) + t(t(matrix(stats::rnorm(n*sum(sel.aux)),nrow=n,ncol=sum(sel.aux))) * sqrt(1-w))
  #   }
  #   beta <- ifelse(include,1,-1/3) * causal * abs(stats::rnorm(n=p))
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
  #   include <- rep(x=c(TRUE,FALSE),times=c(p/q,p/q*(q-1)))
  #   beta <- sample(rep(x=c(0,1),times=c(p/q-5,5)))*abs(stats::rnorm(n=p/q))
  #   eta <- z %*% beta
  #   y <- eta + stats::rnorm(n=n,sd=0.5*sd(eta))
  # }
  sd <- apply(X = x, MARGIN = 2, FUN = function(x) stats::sd(x))
  if (any(sd <= 0.95) || any(sd >= 1.05)) {
    warning("no unit variance")
  }
  if (plot) {
    graphics::par(mfrow = c(1, 2))
    graphics::plot(beta, col = group)
    graphics::image(t(stats::cor(x)[p:1, ]))
  }
  list(y_train = y[fold == 0],
       x_train = x[fold == 0, ],
       y_test = y[fold == 1],
       x_test = x[fold == 1, ],
       group = group,
       include = include,
       causal = causal,
       beta = beta)
}


#' @title
#' Visualise Simulation Settings
#'
#' @examples
#' graphics::par(mar=c(0,0,1.5,0))
#' corila:::.visualise_lupi_data(mode = "baseline")
#'
.visualise_lupi_data <- function(mode, lwd = 1.5, length_arrow = 0.06,
                                 mar = 0.3, xlim = c(1, 5), ylim = c(11, 0),
                                 cex = 0.9) {
  .assert(x = mode, type = "nominal",
          support = c("upstream", "aggregated", "surrogate", "baseline"))
  .assert(x = lwd, type = "numeric", min = 0)
  .assert(x = length_arrow, type = "numeric", min = 0)
  .assert(x = mar, type = "numeric", min = 0)
  .assert(x = xlim, type = "numeric", dim = 2)
  .assert(x = ylim, type = "numeric", dim = 2)
  .assert(x = cex, type = "numeric", min = 0)
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim)
  if (identical(mode, "upstream")) {
    graphics::mtext(
      at = c(NA, 3),
      adj = c(0, NA),
      text = c("upstream", "downstream"),
      col = c("blue", "red"),
      side = 3,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1,
      y = c(1, 5, 10),
      label = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 1 + mar,
      y0 = c(1, 5, 10),
      x1 = 2,
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 2,
      y0 = rep(c(1, 5, 10), each = 3),
      x1 = 3 - mar,
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
    knot <- c(2.5, 5, 8) # c(2,5,9)
    graphics::segments(
      x0 = 3 + mar,
      y0 = c(0:2, 4:6, 9:11),
      x1 = 4,
      y = rep(knot, each = 3),
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
      x = c(1, 3),
      y = 7.5,
      label = "...",
      srt = 90,
      font = 2,
      col = c("blue", "red"),
      cex = cex
    )
  } else if (identical(mode, "aggregated")) {
    graphics::mtext(
      at = c(NA, 3),
      adj = c(0, NA),
      text = c("aggregated", "fine-grained"),
      col = c("blue", "red"),
      side = 3,
      line = 0.5,
      cex = cex
    )
    graphics::text(
      x = 1,
      y = c(1, 5, 10),
      label = expression(x["1,0"], x["2,0"], x["50,0"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 3 - mar,
      y0 = c(0:2, 4:6, 9:11),
      x1 = 2,
      y = rep(c(1, 5, 10), each = 3),
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
      y = rep(knot, each = 3),
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
      x = c(1, 3),
      y = 7.5,
      label = "...",
      srt = 90,
      font = 2,
      col = c("blue", "red"),
      cex = cex
    )
  } else if (identical(mode, "surrogate")) {
    graphics::mtext(
      at = c(NA, 3),
      adj = c(0, NA),
      text = c("surrogate", "canonical"),
      col = c("blue", "red"),
      side = 3,
      line = 0.5,
      cex = cex,
    )
    graphics::text(
      x = 1,
      y = c(0:2, 4:6, 9:11),
      label = expression(x["1,1"], x["1,2"], x["1,3"],
                         x["2,1"], x["2,2"], x["2,3"],
                         x["50,1"], x["50,2"], x["50,3"]),
      col = "blue",
      cex = cex
    )
    graphics::segments(
      x0 = 3 - mar,
      y0 = c(1, 5, 10),
      x1 = 2,
      lwd = lwd,
      col = "grey"
    )
    graphics::arrows(
      x0 = 2,
      y0 = rep(c(1, 5, 10), each = 3),
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
      side = 3,
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
      srt = 90,
      font = 2,
      col = rep(x = c("blue", "red"), times = c(1, 3)),
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
.plot_change <- function(x, ylab = "", main = names(x) , alternative = "both"){
  if(!is.list(x)){stop("Expect list.")}
  nslot <- length(x)
  for (i in seq_len(nslot)) {
    .assert(x = x[[i]], type = "numeric", dim = c(Inf, Inf))
  }
  .assert(x = main, type = "nominal", dim = nslot)
  .assert(x= alternative, type = "nominal",
          support = c("both", "greater", "less"))
  ylim <- range(x)
  if(graphics::par()$mfrow[2] != nslot){
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
      graphics::axis(side = 2)
      graphics::segments(x0 = usr[1], y0 = usr[3], y1 = usr[4])
      graphics::segments(x0 = usr[1], x1 = 99, y0 = usr[3])
      graphics::title(ylab = ylab)
    }
    graphics::title(main = main[i], line = 0.5)
    for (k in seq_len(nrow)) {
      graphics::lines(x = seq_len(ncol), y = x[[i]][k, ],
        col = "grey", lwd = 1.2)
    }
    col <- matrix(data = "grey", nrow = nrow , ncol = ncol)
    col[, 1] <- "blue"
    col[, ncol] <- "red"
    graphics::points(x = col(x[[i]]), y = x[[i]],
                     col = col, pch = 16, cex = 1.1)
    pvalue <- stats::t.test(x = x[[i]][, 1],
                            y = x[[i]][, ncol],
                            paired = TRUE,
                            alternative = alternative)$p.value
    text <- paste0("p=", format(x = signif(pvalue, digits = 2),
                                scientific = TRUE))
    graphics::mtext(text = text, side = 1, cex = 0.7, line = 0.2)
  }
  invisible(NULL)
}

# dependencies: imports: , mvtnorm, pROC, survival
# suggests: CBPE, MLGL, Matrix, SGL, ecpc, gglasso,
# grpreg, grpregOverlap, multiview, pcLasso,
# scoop, sparsegl, squeezy, graper

simulate_overlap <- function() {
  n0 <- 100
  n1 <- 10000
  n <- n0 + n1
  p <- 100
  n_group <- 20
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
#'                    include = rep(c(TRUE, FALSE), each = 80),
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
holdout <- function(x_train, y_train, group, include, family,
                    alpha_init = 0, alpha_final = 1,
                    x_test = NULL, y_test = NULL,
                    nfolds = 10, foldid = NULL, method = NULL,
                    seed = NULL, tune = "both") {
  # nfolds <- 10; foldid <- NULL; seed <- NULL
  
  if (!is.null(include) && any(include == 0) && !is.numeric(group)) {
    stop(paste0("Function holdout is not fully implemented",
                "for privileged learning with overlapping groups."))
  }
  
  p <- ncol(x_train)
  #n0 <- nrow(x_train)
  n1 <- nrow(x_test)
  
  if (is.null(include)) {
    include <- rep(x = TRUE, times = p)
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
                     rep(x = 0, times = sum(include)))
    } else if (i == "ridge") {
      #--- ridge ---
      object <- glmnet::cv.glmnet(x = x_train[, include],
                                  y = y_train,
                                  family = family,
                                  alpha = 0,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$ridge <- stats::predict(object = object,
                                      newx = x_test[, include],
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
      object <- multiridge(x = x_train[, include],
                           y = y_train,
                           z = group[include],
                           family = family)
      if (!is.null(x_test)) {
        y_hat$multiridge <- stats::predict(object = object,
                                           newx = x_test[, include])
      }
      coef$multiridge <- stats::coef(object = object)
    } else if (i == "lasso") {
      #--- lasso ---
      object <- glmnet::cv.glmnet(x = x_train[, include],
                                  y = y_train,
                                  family = family,
                                  alpha = 1,
                                  foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$lasso <- stats::predict(object = object,
                                      newx = x_test[, include],
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
      object <- gglasso::cv.gglasso(x = x_train[, include],
                                    y = temp_y_train,
                                    loss = temp_loss,
                                    group = group[include],
                                    foldid = foldid)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, include],
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
        object <- grpreg::cv.grpsurv(X = x_train[, include],
                                     y = y_train,
                                     group = group[include],
                                     fold = foldid)
      } else {
        object <- grpreg::cv.grpreg(X = x_train[, include],
                                    y = y_train,
                                    family = family,
                                    group = group[include],
                                    fold = foldid)
      }
      if (!is.null(x_test)) {
        y_hat$grpreg <- stats::predict(object = object,
                                       X = x_test[, include],
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
      # lambda <- grplasso::lambdamax(x = cbind(1, x_train[, include]),
      # y = y_train, index = c(NA, group[include]),
      # penscale = base::sqrt, model = model) * 0.9^(0:100)
      # object <- grplasso::grplasso(x = cbind(1, x_train[, include]),
      #                              y = y_train,
      #                              index = c(NA, group[include]),
      #                              model = model,
      #                              lambda = lambda,
      # control = grplasso::grpl.control(update.hess = "lambda", trace = 0))
      # if (!is.null(x_test)) {
      #   y_hat$grplasso <- stats::predict(object = object,
      # newdata = cbind(1, x_test[, include]), type = "response")
      # }
      # coef$grplasso <- object$coefficients[, 1]
    } else if (i == "sparsegl") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      #--- sparse group lasso (sparsegl) ---
      object <- sparsegl::cv.sparsegl(x = x_train[, include],
                                      y = y_train,
                                      group = group[include],
                                      family = family,
                                      foldid = foldid)
      if (!is.null(x_test)) {
        y_hat$sparsegl <- stats::predict(object = object,
                                         newx = x_test[, include],
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
        data_temp <- list(x = x_train[, include],
                          time = as.matrix(y_train)[, "time"],
                          status = as.matrix(y_train)[, "status"])
      } else {
        data_temp <- list(x = x_train[, include], y = y_train)
      }
      cv_object <- SGL::cvSGL(data = data_temp,
                              index = group[include],
                              type = family_temp,
                              foldid = foldid)
      object <- SGL::SGL(data = data_temp,
                         index = group[include],
                         type = family_temp,
                         lambdas = cv_object$lambdas)
      if (!is.null(x_test)) {
        y_hat$SGL <- SGL::predictSGL(x = object,
                                     newX = x_test[, include],
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
          X = x_train[, include],
          y = y_train,
          annot = as.factor(group[include]),
          family = family
        ))
      ))
      if (!is.null(x_test)) {
        y_hat$graper <- stats::predict(object = object,
                                       newX = x_test[, include],
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
        list <- c(lapply(X = unique(group[include]),
                         FUN = function(z) which(group[include] == z)))
        #lapply(X = unique(type[include]),
        # FUN = function(z) which(type[include]  == z))
      } else {
        list <- group
      }
      if (family == "cox") {
        object <- grpregOverlap::cv.grpsurvOverlap(X = x_train[, include],
                                                   y = y_train,
                                                   group = list)
      } else {
        object <- grpregOverlap::cv.grpregOverlap(X = x_train[, include],
                                                  y = y_train,
                                                  group = list,
                                                  family = family)
      }
      if (!is.null(x_test)) {
        y_hat$grpregOverlap <- stats::predict(object = object,
                                              X = x_test[, include],
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
      # x_list = lapply(X = unique(type[include]),
      # FUN = function(z) x_train[, type[include] == z]),
      # y = y_train, family = temp, rho = rho[j], foldid = foldid)
      #}
      #id <- which.min(sapply(object, function(x) min(x$cvm)))
      #if (!is.null(x_test)) {
      #  y_hat$multiview <- stats::predict(object = object[[id]],
      # newx = lapply(X = unique(type[include]),
      # FUN = function(z) x_test[, type[include] == z]),
      # type = "response", s = "lambda.min")
      #}
      #coef$multiview <- stats::coef(object = object[[id]], s = "lambda.min")
    } else if (i == "scoop") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      if (all(table(group[include]) == 1)) {
        #group_temp <- rep(x = 1, times = length(group))
        object <- scoop::coop.lasso(x = x_train[, include],
                                    y = y_train,
                                    group = group,
                                    family = family)
      } else {
        object <- scoop::sparse.coop.lasso(x = x_train[, include],
                                           y = y_train,
                                           group = group[include],
                                           family = family)
      }
      object_cv <- scoop::crossval(object)
      id <- which(object_cv@lambda == object_cv@lambda.min)
      if (!is.null(x_test)) {
        y_hat$scoop <- scoop::predict(object = object,
                                      newx = x_test[, include])[, id]
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
      cv <- MLGL::cv.MLGL(X = x_train[, include],
                          y = y_train_temp,
                          loss = loss)
      object <- MLGL::MLGL(X = x_train[, include],
                           y = y_train_temp,
                           loss = loss)
      if (!is.null(x_test)) {
        temp <- stats::predict(object = object,
                               newx = x_test[, include],
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
      if (is.numeric(group[include])) {
        invisible(utils::capture.output(
          groupset <- ecpc::createGroupset(values = as.factor(group[include]))
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
      #datablocks <- lapply(X = unique(type[include]),
      #                     FUN = function(x) which(type[include] == x))
      invisible(
        tryCatch(
          utils::capture.output(
            object <- ecpc::ecpc(
              Y = y_temp,
              X = x_train[, include],
              groupsets = list(groupset),
              X2 = x_test[, include],
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
      #object <- gren::cv.gren(x = x_train[, include],
      #                        y = y_train,
      #                        partitions = list(group = group, type = type),
      #                        trace = TRUE)
      warning("Implement GREN.")
    } else if (i == "squeezy") {
      if (family %in% c("poisson", "cox")) {
        next
      }
      if (is.numeric(group)) {
        groupset <- lapply(X = unique(group[include]),
                           FUN = function(x) which(group[include] == x))
      } else if (is.list(group)) {
        base <- lapply(group, function(x) as.integer(x))
        # 1st alternative
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)]
        # 2nd alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])
        groupset <- c(base, extra)
      }
      object <- squeezy::squeezy(Y = y_train,
                                 X = x_train[, include],
                                 groupset = groupset,
                                 X2 = x_test[, include])
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
      #  coef <- CBPE(X = x_train[foldid !=  i, include],
      #               y = y_train[foldid != i],
      #               lambda = 0)
      #  x_train[foldid == i, include] %*% coef
      #}
      # internal cross-validation to tune lambda
      # refit on full training data with optimal lambda
      stop("Not yet implemented.")
    } else if (i == "pcLasso") {
      if (!family %in% c("gaussian", "binomial")) {
        next
      }
      group_temp <- lapply(X = unique(group[include]),
                           FUN = function(x) which(x == group[include]))
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
      indices <- seq_len(ncol(x_train[, include]))
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
          object[[j]] <- pcLasso::cv.pcLasso(x = x_train[, include],
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
                                                     xnew = x_test[, include],
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
                          include = include,
                          alpha_init = alpha_init,
                          alpha_final = alpha_final,
                          family = family,
                          foldid = foldid,
                          tune = tune)
      print(object$hyper[object$id_hyper, ])
      if (!is.null(x_test)) {
        y_hat$corila <- stats::predict(object = object,
                                       newx = x_test[, include])
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
        eta <- coef[[i]][1] + x_test[, include] %*% coef[[i]][-1]
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
                   FUN.VALUE = numeric(1)) != sum(include) + 1)) {
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
crossval <- function(x, y, family, group = NULL, include = NULL,
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
                         include = include,
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
                       include = include,
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
#' plot_boxes(x)
#'
#' @keywords graphs
#'
#' @export
plot_boxes <- function(x, base = "corila", main = "", decrease = TRUE,
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
