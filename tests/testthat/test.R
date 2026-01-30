
# Add unit tests for Cox model!

set.seed(1)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- functions "forescale" and "backscale" -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

for(family in c("gaussian","binomial","poisson")){
  message(paste0("family=\"",family,"\""))
  testthat::test_that("regression without and with standardisation returns same results",{
    
    # simulate data
    n0 <- 100
    n1 <- 10000
    n <- n0+n1
    p <- 20
    mu <- stats::rnorm(n=p)
    sd <- stats::rexp(n=p)
    x <- sapply(X=seq_len(p),FUN=function(i) stats::rnorm(n=n,mean=mu[i],sd=sd[i]))
    beta <- stats::rnorm(n=p)*stats::rbinom(n=p,size=1,prob=0.3)
    eta <- x %*% beta + stats::rnorm(n=n)
    foldid <- rep(x=c(0,1),times=c(n0,n1))
    if(family=="gaussian"){
      y <- eta
    } else if(family=="binomial"){
      y <- 1*(eta>=0)
    } else if(family=="poisson"){
      y <- round(exp(eta))
    } else if(family=="cox"){
      time <- stats::rexp(n=n,rate=exp(eta))
      status <- stats::rbinom(n=n,prob=0.5,size=1)
      y <- survival::Surv(time=time,event=status)
    }
    
    # without standardisation
    object.original <- glmnet::glmnet(x=x[foldid==0,],y=y[foldid==0],family=family,lambda=0)
    y_hat.original <- predict(object=object.original,newx=x[foldid==1,],type="response",s=0)
    coef.original <- c(NA[family=="cox"],as.numeric(coef(object.original,s=0)))
    #all.equal(as.numeric(y_hat.original),as.numeric(coef.original[1]+x[foldid==1,] %*% coef.original[-1])) # Gaussian
    
    # with standardisation
    data.scaled <- forescale(x=x[foldid==0,],y=y[foldid==0],family=family)
    object.scaled <- glmnet::glmnet(x=data.scaled$x,y=data.scaled$y,family=family,lambda=0)
    newx.scaled <- forescale(x=x[foldid==1,],pars=data.scaled$pars)
    y_hat.scaled <- predict(object=object.scaled,newx=newx.scaled$x,type="response",s=0)
    coef.scaled <- c(NA[family=="cox"],as.numeric(coef(object=object.scaled,s=0)))
    backscaled <- backscale(pars=data.scaled$pars,y=y_hat.scaled,coef=coef.scaled)
    #all.equal(as.numeric(y_hat.scaled),as.numeric(coef.scaled[1]+newx.scaled$x %*% coef.scaled[-1])) # Gaussian
    
    if(family=="cox"){
      testthat::expect_true(all.equal(y[foldid==0,1],data.scaled$y[,1]))
      testthat::expect_true(all.equal(y[foldid==0,2],data.scaled$y[,2]))
      testthat::expect_true(all.equal(y_hat.scaled,backscaled$y_original))
    }
    
    if(family!="gaussian"){
      testthat::expect_true(all.equal(y[foldid==0],data.scaled$y))
      testthat::expect_true(all.equal(y_hat.original,y_hat.scaled))
      testthat::expect_true(all.equal(y_hat.scaled,backscaled$y_original))
    }
    
    testthat::expect_true(all.equal(target=backscaled$coef,current=coef.original))
    testthat::expect_true(all.equal(target=backscaled$y_original,current=y_hat.original))
    
    
    if(FALSE){
      all(data.scaled$y[,1]==y[foldid==0,1])
      all(y_hat.scaled[,1]==backscaled$y_original[,1])
      
      # examine problem for Cox
      range(y_hat.original)
      range(exp(x[foldid==1,] %*% coef.original[-1]))
      #
      
      # use type="link" and backscale eta instead of y? (scaling y is only done in Gaussian case, where eta=y, use link function after this?)
  
      # scaling factor?
      factor <- unique(round(unique(as.numeric(rl))/unique(as.numeric(y_hat.scaled)),digits=10))
      range(y_hat.scaled*factor)
      
      # linear predictor?
      
      range(exp(newx.scaled$x %*% coef.scaled[-1]))
      
      temp <- predict(object=object.scaled,newx=newx.scaled$x,type="link")
      range(exp(temp))
      
      range(y_hat.scaled)
      range(backscaled$y_original)
    }
  })
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- function "corila" -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Run this code with trial=TRUE and trial=FALSE?
trial <- TRUE

for(family in c("gaussian","binomial","poisson")){
  message(paste0("family=\"",family,"\""))

  data <- simulate(family=family)
  group <- list()
  group$vector <- data$group
  group$list <- lapply(X=unique(group$vector),FUN=function(x) which(group$vector==x))
  group$matrix <- 1*outer(X=group$vector,Y=group$vector,FUN="==")

  model <- sapply(X=group,FUN=function(x) NULL)
  for(i in seq_along(group)){
    set.seed(1)
    model[[i]] <- cv.corila(x=data$x_train,y=data$y_train,group=group[[i]],type=data$type,trial=trial,family=family)
  }
  
  coef <- lapply(X=model,FUN=coef)
  y_hat <- lapply(X=model,FUN=function(x) predict(object=x,newx=data$x_test))

  testthat::test_that("corila returns same coefficients with argument group as vector, list, or matrix",{
    testthat::expect_true(all.equal(coef[[1]],coef[[2]]))
    testthat::expect_true(all.equal(coef[[2]],coef[[3]]))
  })
  
  testthat::test_that("corila returns same predictions with argument group as vector, list, or matrix",{
    testthat::expect_true(all.equal(y_hat[[1]],y_hat[[2]]))
    testthat::expect_true(all.equal(y_hat[[2]],y_hat[[3]]))
  })

  testthat::test_that("function predict returns same results as feature matrix times coef",{
  if(is.na(coef$vector[1])){
    coef$vector[1] <- 0
  }
  eta <- coef$vector[1] + data$x_test %*% coef$vector[-1]
  if(family %in% c("gaussian","cox")){
    pred <- eta
  } else if(family=="binomial"){
    pred <- 1/(1+exp(-eta))
  } else if(family=="poisson"){
    pred <- exp(eta)
  }
  testthat::expect_true(all.equal(as.numeric(pred),as.numeric(y_hat$vector)))
})
}

for(family in c("gaussian","binomial","poisson")){
  message(paste0("family=\"",family,"\""))
  n <- 100
  p <- 50
  sd <- abs(stats::rnorm(n=p))
  X <- y <- list()
  X$original <- sapply(X=sd,FUN=function(x) stats::rnorm(n=n,mean=0,sd=x))
  beta <- stats::rbinom(n=p,size=1,prob=0.2)*stats::rnorm(n=p)
  eta <- scale(X$original %*% beta) 
  if(family=="gaussian"){
    y <- eta + stats::rnorm(n=n,sd=0.5)
  } else if(family=="binomial"){
    y <- stats::rbinom(n=n,size=1,prob=1/(1+exp(-eta)))
  } else if(family=="poisson"){
    y <- stats::rpois(n=n,lambda=exp(eta))
  }
  X$scaled <- scale(X$original)
  y_hat <- list()
  for(i in seq_along(X)){
    set.seed(1)
    object <- cv.corila(x=X[[i]],y=y,group=rep(1:5,each=10),family=family,trial=TRUE)
    y_hat[[i]] <- predict(object=object,newx=X[[i]])
  }
  
  testthat::test_that("corila returns same predictions without and with standardisation",{
     testthat::expect_true(all.equal(y_hat[[1]],y_hat[[2]]))
  })
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- function "multiridge" -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

for(family in c("gaussian","binomial")){
  # simulate
  set.seed(1)
  n0 <- 100
  n1 <- 10000
  n <- n0 + n1
  p <- c(100,50)
  z <- rep(x=seq_along(p),times=p)
  x <- sapply(X=z,FUN=function(x) stats::rnorm(n=n,sd=x))
  beta <- stats::rnorm(n=sum(p),mean=1,sd=0)*stats::rbinom(n=sum(p),size=1,prob=0.2)
  eta <- x %*% beta
  if(family=="gaussian"){
    y <- eta + 0.5*stats::rnorm(n=n,sd=stats::sd(eta))
  } else if(family=="binomial"){
    y <- stats::rbinom(n=n,size=1,prob=1/(1+exp(-eta)))
  } else if(family=="cox"){
    time <- stats::rexp(n=n,rate=exp(eta))
    status <- stats::rbinom(n=n,prob=0.5,size=1)
    y <- survival::Surv(time=time,event=status)
  }
  cond <- rep(x=c(TRUE,FALSE),times=c(n0,n1))
  
  # equality
  object <- multiridge(x=x[cond,],y=y[cond],z=z,family=family)
  y_hat <- stats::predict(object,newx=x[!cond,])
  temp <- starnet:::.mean.function(coef(object)[1] + x[!cond,] %*% coef(object)[-1],family=family)
  
  testthat::test_that("multiridge predict can be reconstructed with coef",{
    testthat::expect_true(all.equal(y_hat,temp))
  })
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- function "nfolds" -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

n <- stats::rpois(n=1,lambda=50)
for(family in c("gaussian","binomial","poisson","cox")){
  if(family=="gaussian"){
    y <- stats::rnorm(n=n)
    index <- rep(x=1,times=n)
  } else if(family=="binomial"){
    y <- stats::rbinom(n=n,size=1,prob=0.2)
    index <- y
  } else if(family=="poisson"){
    y <- stats::rpois(n=n,lambda=4)
    index <- rep(x=1,times=n)
  } else if(family=="cox"){
    time <- stats::rexp(n=n,rate=2)
    status <- stats::rbinom(n=n,prob=0.2,size=1)
    y <- survival::Surv(time=time,event=status)
    index <- y[,"status"]
  }
  foldid <- folds(y=y,family=family,nfolds=10)
  diff <- tapply(X=foldid,INDEX=index,FUN=function(x) diff(range(table(x))))
  testthat::test_that("folds are stratified and balanced",{
    testthat::expect_true(all(diff<=1))
  })
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- function "calc_sign_prec" -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

truth <- sample(x=c(-1,0,1),size=10,replace=TRUE)
estim <- sample(x=c(-1,0,1),size=10,replace=TRUE)

testthat::test_that("precision equals zero if all signs are inverted",{
  prec <- calc_sign_prec(truth=truth,estim=-truth)
  testthat::expect_true(prec==0)
})

testthat::test_that("precision equals one if all signs are true",{
  prec <- calc_sign_prec(truth=truth,estim=truth)
  testthat::expect_true(prec==1)
})

testthat::test_that("precision is not defined if all signs equal zero",{
  prec <- calc_sign_prec(truth=truth,estim=0*estim)
  testthat::expect_true(is.na(prec))
})

testthat::test_that("precision is not influenced by estimated zeros",{
  prec1 <- calc_sign_prec(truth=truth,estim=estim)
  prec2 <- calc_sign_prec(truth=truth[estim!=0],estim=estim[estim!=0])
  testthat::expect_true(prec1==prec2)
})

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#----- privileged information -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

for(family in c("gaussian","binomial","poisson","cox")){
  # simulate data
  data <- simulate(family=family)
  include <- stats::rbinom(n=data$info$p,size=1,prob=0.5)==1
  # fit model
  object <- cv.corila(x=data$x_train,y=data$y_train,group=data$group,include=include,family=family)
  testthat::test_that("predict is not influenced by auxiliary predictors",{
    y_hat1 <- predict(object=object,newx=data$x_test)
    y_hat2 <- predict(object=object,newx=data$x_test[,include])
    newx <- data$x_test
    newx[,!include] <- 0
    y_hat3 <- predict(object=object,newx=newx)
    testthat::expect_true(all(y_hat1==y_hat2))
    testthat::expect_true(all(y_hat1==y_hat3))
    testthat::expect_true(all(y_hat2==y_hat3))
  })
}
