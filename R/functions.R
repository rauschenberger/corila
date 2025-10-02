

#'@title
#'Standardisation
#'
#'@export
#'
#'@description
#'Transforming variables to mean 0 and variance 1.
#'
#'@inheritParams corila
#'@param y \eqn{n_0}-dimensional response vector or \code{NULL}, only for Gaussian family 
#'@param family character string \code{"gaussian"}, \code{NULL}
#'@param pars list or \code{NULL}
#'
#'@return
#'- standardised \eqn{n_0 \times p} predictor matrix \eqn{x}
#'- standardised \eqn{n_0}-dimensional response vector \eqn{y} (only if \code{family="gaussian"} or \code{pars$family="gaussian"}; otherwise output equals input)
#'- list \code{pars} with slots \code{mu.x} and \code{sd.x} (\eqn{p}-dimensional vectors of means and standard deviations of the predictor variables), and \code{mu.y} and \code{sd.y} (mean and standard deviation of response variable for Gaussian family, 0 and 1 for other families)
#'- character string \code{family} indicates the model (\code{"gaussian"}, \code{"binomial"}, \code{"poisson"}, or \code{"cox"}), determined by argument \code{family} or \code{pars$family}
#'
forescale <- function(x,y=NULL,family=NULL,pars=NULL){
  if(is.null(family)==is.null(pars)){stop("Provide either family or pars.")}
  if(is.null(family)){family <- pars$family}
  if(is.null(pars)){
    pars <- list()
    pars$family <- family
    pars$mu.x <- apply(X=x,MARGIN=2,FUN=base::mean)
    pars$sd.x <- apply(X=x,MARGIN=2,FUN=stats::sd)
    if(!is.null(y) & family=="gaussian"){
      pars$mu.y <- mean(y)
      pars$sd.y <- stats::sd(y)
    } else if(!is.null(y)){
      pars$mu.y <- 0
      pars$sd.y <- 1
    }
  }
  x_scaled <- t((t(x)-pars$mu.x)/pars$sd.x)
  x_scaled[,pars$sd.x==0] <- 0
  if(!is.null(y) & family=="gaussian"){
    y_scaled <- (y-pars$mu.y)/pars$sd.y
  } else if(!is.null(y)){
    y_scaled <- y
  } else {
    y_scaled <- NULL 
  }
  list <- list(x=x_scaled,y=y_scaled,pars=pars,family=family)
  return(list)
}

#'@title
#'Inverse Standardisation
#'
#'@export
#'
#'@description
#'Transforms response variable back to original scale or transforms coefficients for predictor variables and response variable on original scales.
#'
#'@inheritParams forescale
#'@param y \eqn{n_1}-dimensional response vector
#'@param coef \eqn{(1+p)-dimensional vector} containing the estimated intercept and the estimated slopes or \code{NULL} (default)
#'
#'@return
#'Returns a list with slots \code{y_original} or \code{coef}.
#'
#'@examples
#'
#'# simulate data
#'n <- 100; p <- 3
#'sd <- stats::rpois(n=p,lambda=5)
#'x <- sapply(X=sd,FUN=function(x) stats::rnorm(n=n,sd=x))
#'beta <- stats::rnorm(n=p)
#'y <- x %*% beta + stats::rnorm(n=n)
#'
#'# without standardisation
#'lm1 <- stats::lm(y~x)
#'y_hat1 <- fitted(lm1)
#'coef1 <- coef(lm1)
#'
#'# with standardisation
#'scale <- forescale(x=x,y=y,family="gaussian")
#'lm2 <- stats::lm(scale$y~scale$x)
#'result <- backscale(pars=scale$pars,y=fitted(lm2),coef=coef(lm2))
#'y_hat2 <- result$y_original
#'coef2 <- result$coef
#'
#'# equality
#'all.equal(y_hat1,y_hat2)
#'all.equal(coef1,coef2,check.attributes=FALSE)
#'
backscale <- function(pars,y=NULL,coef=NULL){
  list <- list()
  if(!is.null(y) & pars$family=="gaussian"){
    list$y_original <- pars$mu.y+pars$sd.y*y
  } else if(!is.null(y)){
    list$y_original <- y
  }
  if(!is.null(coef)){
    alpha <- pars$mu.y+pars$sd.y*(coef[1]-sum(coef[-1]*ifelse(test=pars$sd.x==0,yes=0,no=pars$mu.x/pars$sd.x))) # why yes=0? (same for below)
    beta <- coef[-1]*ifelse(test=pars$sd.x==0,yes=0,no=pars$sd.y/pars$sd.x)
    list$coef <- c(alpha,beta)
  }
  return(list)
}

#----- group-ridge -----

#'@title
#'Multi-Penalty Ridge Regression
#'
#'@export
#'
#'@description
#'Fits multi-penalty ridge regression (tuning regularisation parameters and estimating regression coefficients).
#'
#'@param x predictors: \eqn{n \times p} matrix, or list of length \eqn{q} of \eqn{n \times p_k} matrices, with \eqn{k} in \eqn{\{1,\ldots,q\}}.
#'@param y response: \eqn{n}-dimensional vector
#'@param z groups: \eqn{p}-dimensional vector with entries in \eqn{\{1,\ldots,q\}} (if \code{x} is a matrix), or \code{NULL} (if \code{x} is a list of matrices)
#'@param family character \code{"linear"} (or \code{"gaussian"}), \code{"logistic"} (or \code{"binomial"}), or \code{"cox"} 
#'@param penalties \eqn{q}-dimensional vector of penalty parameters, or \code{NULL} (cross-validation)
#'
#'@references
#'\href{https://orcid.org/0000-0003-4780-8472}{Mark A. van de Wiel},
#'\href{https://orcid.org/0000-0001-7715-1446}{Mirrelijn M. van Nee},
#'and
#'\href{https://orcid.org/0000-0001-6498-4801}{Armin Rauschenberger}
#'(2021).
#'"Fast cross-validation for multi-penalty high-dimensional ridge regression"
#'\emph{Journal of Computational and Graphical Statistics}
#'30(4):835-847.
#'\href{https://doi.org/10.1080/10618600.2021.1904962}{doi: 10.1080/10618600.2021.1904962}.
#'
#'@return
#'Returns an object of class \code{multiridge}.
#'
#'@seealso
#'Extract coefficients with \code{\link[=coef.multiridge]{coef}()} or make predictions with \code{\link[=predict.multiridge]{predict}()}.
#'
multiridge <- function(x,y,z,family,penalties=NULL){
  if(!family %in% c("gaussian","linear","binomial","logistic","cox")){
    stop("Argument \"family\" must equal \"gaussian\" (or \"linear\"), \"binomial\" (or \"logistic\"), or \"cox\".")
  }
  scale <- forescale(x=x,y=y,family=family)
  model <- ifelse(family=="gaussian",yes="linear",no=ifelse(family=="binomial",yes="logistic",no=family))
  X <- lapply(X=unique(z),FUN=function(i) scale$x[,z==i])
  XXblocks <- multiridge::createXXblocks(datablocks=X)
  invisible(utils::capture.output(init <- multiridge::fastCV2(XXblocks=XXblocks,Y=scale$y,model=model)))
  if(is.null(penalties)){
    folds <- multiridge::CVfolds(Y=scale$y)
    invisible(utils::capture.output(final <- multiridge::optLambdasWrap(penaltiesinit=init$lambdas,
                                                                        XXblocks=XXblocks,Y=scale$y,folds=folds)))
    penalties <- final$optpen
  }
  XXT <- multiridge::SigmaFromBlocks(XXblocks=XXblocks,penalties=penalties)
  if(family=="cox"){
    object <- multiridge::IWLSCoxridge(XXT=XXT,Y=scale$y)
  } else {
    object <- multiridge::IWLSridge(XXT=XXT,Y=scale$y,model=model)
  }
  object$family <- family
  object$penalties <- penalties
  object$datablocks <- X
  object$z <- z
  object$pars <- scale$pars
  class(object) <- "multiridge"
  return(object)
}

#'@title
#'Make Predictions
#'
#'@export
#'
#'@description
#'Makes predictions from a multi-penalty ridge regression model.
#'
#'@inheritParams coef.multiridge
#'@inheritParams predict.corila
#'
#'@seealso \code{\link{multiridge}()}, \code{\link{coef.multiridge}()}
#'
predict.multiridge <- function(object,newx,...){
  scale <- forescale(x=newx,pars=object$pars)
  newX <- lapply(X=unique(object$z),FUN=function(x) scale$x[,object$z==x])
  XXblocks <- multiridge::createXXblocks(datablocks=object$datablocks,datablocksnew=newX)
  Sigmanew <- multiridge::SigmaFromBlocks(XXblocks=XXblocks,penalties=object$penalties)
  eta <- multiridge::predictIWLS(IWLSfit=object,Sigmanew=Sigmanew)
  y_hat <- starnet:::.mean.function(eta,family=object$family)
  y_hat <- backscale(pars=object$pars,y=y_hat)$y
  return(y_hat)
}

#'@title
#'Extract Coefficients
#'
#'@export
#'
#'@description
#'Extracts coefficients from a multi-penalty ridge regression model.
#'
#'@param object object of class \code{"multiridge"}
#'@param ... (not used)
#'
#'@seealso \code{\link{multiridge}()}, \code{\link{predict.multiridge}()}
#'
coef.multiridge <- function(object,...){
  Xblocks <- multiridge::createXblocks(datablocks=object$datablocks)
  coef <- multiridge::betasout(object,Xblocks=Xblocks,penalties=object$penalties)
  if(object$family=="cox" & is.null(coef[[1]])){
    coef[[1]] <- 0
  }
  coef <- backscale(pars=object$pars,coef=unlist(coef))$coef
  return(coef)
}

if(FALSE){
  # Make multiridge work (Gaussian and binomial case)
  
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
  family <- "binomial"
  if(family=="gaussian"){
    y <- eta + 0.5*stats::rnorm(n=n,sd=stats::sd(eta))
  } else if(family=="binomial"){
    y <- stats::rbinom(n=n,size=1,prob=1/(1+exp(-eta)))
  } else if(family=="cox"){
    time <- stats::rexp(n=n,rate=exp(eta))
    status <- stats::rbinom(n=n,prob=0.5,size=1)
    #y <- cbind(time=time,status=status)
    y <- survival::Surv(time=time,event=status)
  }
  cond <- rep(x=c(TRUE,FALSE),times=c(n0,n1))

  y_hat <- list()
  # equality
  object <- multiridge(x=x[cond,],y=y[cond],z=z,family=family)
  y_hat$multiridge <- stats::predict(object,newx=x[!cond,])
  temp <- starnet:::.mean.function(coef(object)[1] + x[!cond,] %*% coef(object)[-1],family=family)
  all.equal(y_hat$multiridge,temp)
  
  # comparison
  glmnet <- glmnet::cv.glmnet(x=x[cond,],y=y[cond],family=family,alpha=0)
  y_hat$glmnet <- stats::predict(object=glmnet,newx=x[!cond,],type="response")
  
  if(family=="gaussian"){
    metric <- sapply(X=y_hat,FUN=function(x) mean((x-y[!cond])^2))
  } else if(family=="binomial"){
    metric <- sapply(X=y_hat,FUN=function(x) pROC::auc(response=y[!cond],predictor=as.vector(x),levels=c(0,1),direction="<"))
  } else if(family=="cox"){
    metric <- sapply(X=y_hat,FUN=function(x) survival::concordance(y[!cond]~I(-x))$concordance)
  }
  metric
}

#----- group-lasso -----

#'@title
#'Combine variables
#'
#'@export
#'
#'@description
#'Calculates the mean or the first principal component of a group of variables
#'
#'@inheritParams construct_matrices
#'@param x \eqn{n_0 \times p_k} matrix, where \eqn{n_0} is the number of observations used for model training and \eqn{p_k} is the number of variables inside a group
#'
#'@examples
#'n <- 100; p <- 5
#'x <- matrix(data=stats::rnorm(n=n*p),nrow=n,ncol=p)
#'mean <- combine_features(x=x,mode="mean")
#'comp <- combine_features(x=x,mode="pca")
#'plot(mean,comp)
#'
combine_features <- function(x,mode="mean"){
  if(mode=="mean"){
    rowMeans(x)
  } else if(mode=="pca"){
    stats::princomp(x=x)$scores[,"Comp.1"]
  }
}

#'@title
#'Construct Matrices
#'
#'@export
#'
#'@description
#'Constructs matrices with (i) the original data concatenated with the inverted data, (ii) one meta-variable for each group, and (iii) one meta-variable for each group in each type.
#'
#'@param group \eqn{p}-dimensional vector of group labels or indices, or list with one slot for each group containing the variable labels or indices
#'@param type \eqn{p}-dimensional vector
#'@inheritParams corila
#'
#'@examples
#'n <- 5
#'p <- 6
#'x <- matrix(data=rnorm(n*p),nrow=n,ncol=p)
#'group <- rep(1:2,each=p/2)
#'type <- rep(x=1,times=p)
#'x <- construct_matrices(x=x,group=group,type=type)
#'
#'@return
#'See description.
#'
construct_matrices <- function(x,group,type,mode="mean"){
  index <- seq_len(ncol(x))
  n <- nrow(x)
  if(is.numeric(group)){
    q <- length(unique(group))
  } else {
    q <- length(group)
  }
  m <- length(unique(type))
  com <- matrix(data=NA,nrow=n,ncol=q,dimnames=list(NULL,seq_len(q)))
  sep <- replicate(n=m,expr=com,simplify=FALSE)
  for(i in seq_len(m)){
    for(j in seq_len(q)){
      if(is.numeric(group)){
        sep[[i]][,j] <- combine_features(x=x[,type==i & group==j,drop=FALSE],mode=mode)
      } else {
        sep[[i]][,j] <- combine_features(x=x[,type==i & index %in% group[[j]],drop=FALSE],mode=mode)
      }
    }
  }
  for(j in seq_len(q)){
    if(is.numeric(group)){
      com[,j] <- combine_features(x=x[,group==j,drop=FALSE],mode=mode)
    } else {
      com[,j] <- combine_features(x=x[,index %in% group[[j]],drop=FALSE],mode=mode)
    }
  }
  return(list(all=cbind(x,-x),com=com,sep=sep))
}

if(FALSE){
  n <- 100
  p <- 50
  x <- matrix(data=stats::rnorm(n*p),nrow=n,ncol=p)
  y <- stats::rnorm(n=n)
  group <- rep(1:10,each=5)
  type <- rep(1,times=p)
  family <- "gaussian"
  hyper <- data.frame(com=0.5,sep=0.25,ind=0.25)
}

#'@title
#'Group lasso
#'
#'@description
#'Fits an initial ridge regression to obtain weights for an adaptive lasso 
#'regression that allows for heterogeneous, overlapping and unknown groups of correlated variables.
#'
#'@param x \eqn{n_0 \times p} predictor matrix, where \eqn{n_0} is the number of observations used for model training and \eqn{p} is the number of variables
#'@param y \eqn{n}-dimensional response vector
#'@param group XXX
#'@param type \eqn{p}-dimensional vector
#'@param family character string "gaussian", "binomial", "poisson" or "cox"
#'@param hyper list of of \eqn{m}-dimensional vectors or a data frame with $m$ rows containing candidate values for hyperparameters
#'@param cor character string \code{"pearson"}, \code{"spearman"} (default), or \code{"kendall"}; or \eqn{p \times p} correlation matrix 
#'@param cond \code{NULL}
#'@param lambda.com,lambda.sep,lambda.ind \code{NULL}
#'@param mode character string "mean" for arithmetic mean  or "pca" for first principal component
#'@param init.multi logical
#'@param trial logical
#'
#'@return
#'Returns an object of class \code{"corila"}.
#'
#'@examples
#'NULL
#'
corila <- function(x,y,group,type,family,hyper,cor="spearman",cond=NULL,lambda.com=NULL,lambda.sep=NULL,lambda.ind=NULL,trial=TRUE,mode="mean",init.multi=FALSE){
  # cond=NULL;lambda.com=NULL;lambda.sep=NULL;lambda.ind=NULL;trial=TRUE;mode<-"mean";cor="spearman"
  
  n <- nrow(x) # sample size
  p <- ncol(x) # number of features
  if(is.null(group)){group <- seq_len(p)}
  if(is.null(type)){type <- rep(x=1,times=p)}
  #if(length(group)!=p){stop("Argument \"group\" must be a vector of length p.")}
  if(length(type)!=p){stop("Argument \"type\" must be a vector of length p.")}
  if(is.numeric(group)&&!is.array(group)){
    q <- length(unique(group)) # number of groups = number of unique values
  } else if(is.list(group)){
    q <- length(group) # number of groups = number of slots
  }
  m <- length(unique(type)) # number of modalities
  
  if(is.numeric(group)&&!is.array(group)){
    if(length(group)!=p||max(group)!=length(unique(group))||any(sort(unique(group))!=seq(from=1,to=max(group),by=1))){
      stop("Argument \"group\" should be of length p, with all entries in {1,...,q}.")
    }
  } else {
    if(is.character(group[[1]])){
      #test <- lapply(group,function(slot) sapply(slot,function(entry) which(colnames(x)==entry)))
      warning("Implement this.")
    }
  }
  
  scale <- forescale(x=x,y=y,family=family)
  
  if(trial){
    mat <- list()
    mat$all <- cbind(scale$x,-scale$x) # scale was forgotten! (2029-09-05)
  } else {
    mat <- construct_matrices(x=scale$x,group=group,type=type,mode=mode)
  }
  
  index <- seq_len(p)
  
  if(is.null(cond)){
    cond <- rep(x=TRUE,times=n)
  }
  
  if(!trial){
    # group-wise combination
    cond.coef <- rep(c(FALSE,TRUE),times=c(family!="cox",q))
    if(is.null(lambda.com)){
      fit.com <- glmnet::cv.glmnet(x=mat$com[cond,,drop=FALSE],y=scale$y[cond],family=family,alpha=0)
      s <- "lambda.min"
      #coef.com <- stats::coef(object=fit.com,s="lambda.min")[-1]
    } else {
      fit.com <- glmnet::glmnet(x=mat$com[cond,,drop=FALSE],y=scale$y[cond],family=family,alpha=0)
      s <- lambda.com
      #coef.com <- stats::coef(object=fit.com,s=lambda.com)[-1]  
    }
    coef.com <- stats::coef(object=fit.com,s=s)[cond.coef]
    
    # group-and-type-wise combination
    # (This could also be done with multiridge, using the paired setting.)
    fit.sep <- list()
    coef.sep <- matrix(data=NA,nrow=q,ncol=m) # Why not nrow=q,ncol=m? # was nrow=p,ncol=q
    for(i in seq_len(m)){
      if(is.null(lambda.sep)){
        fit.sep[[i]] <- glmnet::cv.glmnet(x=mat$sep[[i]][cond,],y=scale$y[cond],family=family,alpha=0)
        s <- "lambda.min"
        coef.sep[,i] <- stats::coef(object=fit.sep[[i]],s="lambda.min")[cond.coef]
      } else {
        fit.sep[[i]] <- glmnet::glmnet(x=mat$sep[[i]][cond,],y=scale$y[cond],family=family,alpha=0)
        coef.sep[,i] <- stats::coef(object=fit.sep[[i]],s=lambda.sep[i])[cond.coef]
      }
    }
    
  }
  
  if(init.multi){
    message("using multiridge for initialisation")
    if(is.null(lambda.ind)){
      fit.ind <- multiridge(x=scale$x[cond,],y=scale$y[cond],z=type,family=family)
      coef.ind <- stats::coef(object=fit.ind,s="lambda.min")[-1]
      lambda.ind <- fit.ind$penalties
    } else {
      fit.ind <- multiridge(x=scale$x[cond,],y=scale$y[cond],z=type,family=family,penalties=lambda.ind)
      coef.ind <- stats::coef(object=fit.ind)[-1]
    }
  } else {
    # no combination
    cond.coef <- rep(c(FALSE,TRUE),times=c(family!="cox",p))
    if(is.null(lambda.ind)){
      fit.ind <- glmnet::cv.glmnet(x=scale$x[cond,],y=scale$y[cond],family=family,alpha=0)
      coef.ind <- stats::coef(object=fit.ind,s="lambda.min")[cond.coef]
      lambda.ind <- fit.ind$lambda.min
    } else {
      fit.ind <- glmnet::glmnet(x=scale$x[cond,],y=scale$y[cond],family=family,alpha=0)
      coef.ind <- stats::coef(object=fit.ind,s=lambda.ind)[cond.coef]
    }
  }
  
  if(!trial){
    
    # prior coefficients
    prior <- list()
    
    if(is.numeric(group)){
      prior$com <- prior$sep <- rep(x=NA,times=p)
    } else {
      prior$com <- prior$sep <- rep(x=NA,times=2*p)
    }
    #prior$ind <- rep(x=NA,times=p)
    
    if(is.numeric(group)){
      for(j in seq_len(q)){
        prior$com[group==j] <- coef.com[j]
      }
    } else {
      for(j in seq_len(q)){
        group_index <- sapply(group,function(x) j %in% x)
        prior$com[j] <- sum(pmax(0,coef.com[group_index])) # positive
        prior$com[p+j] <- -sum(pmin(0,coef.com[group_index])) # negative
      }
    }
    
    if(is.numeric(group)){
      for(i in seq_len(m)){
        for(j in seq_len(q)){
          prior$sep[type==i & group==j] <- coef.sep[j,i]
        }
      }
    } else {
      for(j in seq_len(q)){
        group_index <- sapply(group,function(x) j %in% x)
        prior$sep[j] <- sum(pmax(0,coef.sep[group_index,type[j]])) # positive
        prior$sep[p+j] <- -sum(pmin(0,coef.sep[group_index,type[j]])) # negative
      }
    }
    
    prior$ind <- pmax(0,c(coef.ind,-coef.ind))
    
  }
  
  # CONTINUE HERE: Replace by prior coef without cor.
  if(!is.matrix(cor)){
    cor <- stats::cor(x=scale$x,method=cor)
  }
  cor[is.na(cor)] <- 0
  
  
  #if(is.numeric(group)){
  #  for(j in seq_len(p)){
  #    temp <- (cor[,j]*coef.ind)*(group[j]==group)
  #    prior$ind[j] <- sum(pmax(0,temp))
  #    prior$ind[p+j] <- -sum(pmin(0,temp))
  #  }
  #}
  # CONTINUE HERE: Implement this for overlapping groups.
  
  if(!trial){
    
    weight <- list()
    eps <- 0 # 1e-09
    if(is.numeric(group)){
      weight$com <- pmax(eps,c(prior$com,-prior$com))
      weight$sep <- pmax(eps,c(prior$sep,-prior$sep))
    } else {
      weight$com <- pmax(eps,prior$com)
      weight$sep <- pmax(eps,prior$sep)
    }
    weight$ind <- pmax(eps,prior$ind)
    
    if(any(unlist(weight)<0,na.rm=TRUE)){stop("negative weights")}
    # let each set of weights sum to p
    weight <- lapply(X=weight,FUN=function(x) x/sum(x)*p) # trial 2025-06-24
    
  }
  
  # if(trial){
  #    warning("Using trial version.")
  #    fit.all <- glmnet::cv.glmnet(x=mat$all[cond,],y=y[cond],family=family,alpha=0,lower.limits=0)
  #    coef.all <- stats::coef(fit.all,s="lambda.min")[-1]
  #    weight <- list()
  #    com.pos <- com.neg <- sep.pos <- sep.neg <- numeric()
  #    eps <- 0
  #    for(i in seq_len(p)){
  #        com.pos[group==i] <- sum(coef.all[1:(length(coef.all)/2)][group==i])
  #        com.neg[group==i] <- sum(coef.all[(length(coef.all)/2+1):length(coef.all)][group==i])
  #    }
  #    weight$com <- pmax(0,c(com.pos,com.neg)) # double-check
  #    weight$sep <- pmax(0,coef.all) #  double-check
  # }
  
  object <- list()
  for(i in seq_len(nrow(hyper))){
    #pf.ext <- 1/(hyper$com[i]*weight$com+hyper$sep[i]*weight$sep) # original
    #warning("next lines are temporary")
    #if(all(type==1)){
    #  pf.ext <- 1/(weight$com^hyper$com[i]) # temporary!
    #} else {
    #  pf.ext <- 1/(weight$com^hyper$com[i]+weight$sep^hyper$sep[i]) # temporary!
    #}
    #if(trial){
    #  #if(all(type==1)){
    #  #  pf.ext <- 1/(weight$com^hyper$com[i]) # trial
    #  #} else {
    #  #  pf.ext <- 1/(weight$com^hyper$com[i]+weight$sep^hyper$sep[i]) # trial 
    #  #}
    #  pf.ext <- 1/(weight$com*hyper$com[i]+weight$sep*hyper$sep[i]+weight$ind*hyper$ind[i])
    #}
    if(trial){
      weight <- list()
      weight$com <- weight$sep <- weight$ind <- rep(x=NA,times=p)
      for(j in seq_len(p)){
        # features in same group and same modality
        if(is.numeric(group)&&!is.array(group)){
          cond.temp <- (group[j]==group) & (type[j]==type)
        } else if(is.list(group)){
          group_index <- sapply(group,function(x) j %in% x)
          cond.temp <- seq_len(p) %in% unlist(group[group_index]) & type==type[j]
        } else if(is.matrix(group)){
          cond.temp <- group[,j]==1 & type==type[j]
        }
        temp <- (sign(cor[,j])*abs(cor[,j])^1*coef.ind)*cond.temp
        weight$sep[j] <- sum(pmax(0,temp)[cond.temp]) # was mean!
        weight$sep[p+j] <- sum(pmax(0,-temp)[cond.temp]) # was mean!
        # features in same group
        if(is.numeric(group)&&!is.array(group)){
          cond.temp <- group[j]==group
        } else if(is.list(group)){
          group_index <- sapply(group,function(x) j %in% x)
          cond.temp <- seq_len(p) %in% unlist(group[group_index]) 
        } else if(is.matrix(group)){
          cond.temp <- group[,j]==1
        }
        temp <- (sign(cor[,j])*abs(cor[,j])^1*coef.ind)*cond.temp
        weight$com[j] <- sum(pmax(0,temp)[cond.temp]) # was mean!
        weight$com[p+j] <- sum(pmax(0,-temp)[cond.temp]) # was mean!
        # all features
        temp <- (sign(cor[,j])*abs(cor[,j])^1*coef.ind)
        weight$ind[j] <- sum(pmax(0,temp)) # was mean!
        weight$ind[p+j] <- sum(pmax(0,-temp)) # was mean!
        # Ad-hoc solution for features that are in no group:
        weight$com[is.na(weight$com)] <- 0 # Consider 0 and weight$ind
      }
      # # temporary code with beta distribution:
      # for(j in seq_len(p)){
      #    if(is.numeric(group)){
      #      cond.temp <- group[j]==group
      #    } else {
      #      group_index <- sapply(group,function(x) j %in% x)
      #      cond.temp <- seq_len(p) %in% unlist(group[group_index]) 
      #    }
      #    temp <- sign(cor[,j])*stats::qbeta(p=abs(cor[,j]),shape1=hyper$alpha[i],shape2=hyper$beta[i])*coef.ind*cond.temp
      #    weight$com[j] <- mean(pmax(0,temp)[cond.temp])
      #    weight$com[p+j] <- mean(pmax(0,-temp)[cond.temp])
      # }
      weight <- lapply(weight,function(x) 2*p*x/sum(x)) # standardising weights
    }
    #pf.ext <- 1/pmax(0,weights)
    #pf.ext <- 1/(weights$com^hyper$com[i]*weights$sep^hyper$sep[i]*weights$ind^hyper$ind[i])
    #pf.ext <- 1/(weight$com*hyper$com[i]+weight$sep*hyper$sep[i]+weight$ind*hyper$ind[i])
    warning("temporary next line")
    #pf.ext <- 1/weight$com
    #pf.ext <- 1/(weight$com*hyper$com[i]+weight$ind*hyper$ind[i])
    pf.ext <- 1/(weight$com*hyper$local[i]+weight$ind*hyper$global[i])
    if(any(pf.ext<0)){stop(paste0("negative pf:",min(pf.ext)))}
    object[[i]] <- glmnet::glmnet(x=mat$all[cond,],y=scale$y[cond],family=family,penalty.factor=pf.ext,lower.limits=0,alpha=1)
  }
  
  if(!trial){
    if(is.null(lambda.com)){
      lambda.com <- fit.com$lambda.min
    }
    
    if(is.null(lambda.sep)){
      lambda.sep <- sapply(X=fit.sep,FUN=function(x) x$lambda.min)
    }
  }
  
  list <- list(model=object,lambda.com=lambda.com,lambda.sep=lambda.sep,lambda.ind=lambda.ind,scale=scale$pars)
  class(list) <- "corila"
  return(list)
}

#'@title
#'predict (S3 method) 
#'
#'@description
#'Makes prediction from an object of class \code{corila}.
#'
#'@inheritParams predict.cv.corila
#'
#'@param object object of class \code{corila}
#'@param index integer scalar specifying the index of the mixing hyperparameter(s)
#'@param s numeric scalar specifying the value of the regularisation hyperparameter
#'@param ... (not used)
#'
#'@seealso \code{\link{corila}()}
#'
#'@return
#'Returns fitted or predicted values in an \eqn{n_0}-dimensional or \eqn{n_1}-dimensional vector, respectively.
#'
predict.corila <- function(object,newx,index,s,...){
  newx_stand <- forescale(x=newx,pars=object$scale)$x
  y_hat_stand <- stats::predict(object=object$model[[index]],newx=cbind(newx_stand,-newx_stand),s=s,type=ifelse(object$scale$family=="cox","link","response"))
  y_hat <- backscale(y=y_hat_stand,pars=object$scale)$y
  return(y_hat)
}

#'@title
#'Sparse Group Lasso
#'
#'@description
#'Optimises the parameters and the hyperparameters of the sparse group lasso.
#'
#'@inheritParams corila
#'@param foldid \eqn{n}-dimensional vector containing the fold identifiers
#'
#'@seealso \code{\link{corila}()}
#'
cv.corila <- function(x,y,group,type=NULL,family="gaussian",cor="spearman",mode="mean",init.multi=FALSE,trial=TRUE,foldid=NULL){
  if(is.null(type)){
    type <- rep(x=1,times=ncol(x))
  }
  
  # family="gaussian"; mode="mean"; foldid=NULL
  n <- nrow(x) # sample size
  #p <- ncol(x) # number of features
  #p <- length(unique(group)) # number of groups
  #q <- length(unique(type)) # number of modalities
  
  #if(all(type==1)){
  #  cand <- seq(from=0,to=1,by=0.1)
  #  hyper <- data.frame(com=1,sep=0) # for weighted sums
  #  #hyper <- data.frame(com=cand,sep=0) # for exponents
  #} else {
  #  cand <- seq(from=0,to=1,by=0.1) # for weighted sums
  #  hyper <- data.frame(com=cand,sep=1-cand) # for weighted sums
  #  #cand <- seq(from=0,to=1,by=0.2) # for exponents
  #  #hyper <- data.frame(com=cand,sep=cand) # for exponents
  #}
  
  # if(trial){
  #   #cand <- seq(from=0,to=1,by=0.25)
  #   #if(all(type==1)){
  #   #  hyper <- data.frame(com=cand,sep=0)
  #   #} else {
  #   #  hyper <- expand.grid(com=cand,sep=cand)
  #   #}
  #   if(all(type==1)){
  #     cand <- seq(from=0,to=1,by=0.1)
  #     hyper <- expand.grid(com=cand,sep=0,ind=cand)
  #   } else {
  #     cand <- seq(from=0,to=1,by=0.2) # was by 0.2
  #     hyper <- expand.grid(com=cand,sep=cand,ind=cand)
  #   }
  #   hyper <- hyper[rowSums(hyper)==1,] # only for weighted sum
  #   #hyper <- expand.grid(exp=c(0,0.1,0.2,0.5,0.75,1,4/3,2,5,10,Inf))
  #   #hyper <- expand.grid(exp=c(0,0.2,0.4,0.6,0.8,1.0))
  #   #hyper <- expand.grid(exp=seq(0,2,0.2))
  #   rownames(hyper) <- NULL
  # }
  
  #if(trial){
  #  cand <- c(0.1,0.5,1,2,5)
  #  hyper <- expand.grid(alpha=cand,beta=cand)
  #}
  
  #if(trial){
  #  cand <- c(0,0.5,0.75,1,1.25,1.5,2)
  #  hyper <- data.frame(alpha=cand,beta=2-cand)
  #}
  
  if(trial){
    cand <- seq(from=0,to=1,by=0.1) # for weighted sums
    hyper <- data.frame(local=cand,global=1-cand) # for weighted sums
  }
  
  nfolds <- 10
  if(is.null(foldid)){
    foldid <- sample(rep(x=seq_len(nfolds),length.out=n))
  }
  
  # Use foldid already for full run?
  object.ext <- corila(x=x,y=y,group=group,type=type,family=family,cor=cor,hyper=hyper,mode=mode,init.multi=init.multi,trial=trial)
  lambda <- lapply(X=object.ext$model,FUN=function(x) x$lambda)
  
  hat <- list()
  for(j in seq_len(nrow(hyper))){
    hat[[j]] <- matrix(data=NA,nrow=n,ncol=length(object.ext$model[[j]]$lambda))
  }
  
  
  for(i in seq_len(nfolds)){
    object.int <- corila(x=x[foldid!=i,],y=y[foldid!=i],group=group,type=type,family=family,cor=cor,hyper=hyper,mode=mode,lambda.com=object.ext$lambda.com,lambda.sep=object.ext$lambda.sep,lambda.ind=object.ext$lambda.ind,init.multi=init.multi,trial=trial)
    for(j in seq_len(nrow(hyper))){
      hat[[j]][foldid==i,] <- stats::predict(object=object.int,newx=x[foldid==i,],index=j,s=lambda[[j]])
    }
  }
  
  cvm <- list()
  eps <- 1e-06
  for(l in seq_len(nrow(hyper))){
    if(family=="gaussian"){
      cvm[[l]] <- apply(X=hat[[l]],MARGIN=2,FUN=function(x) mean((x-y)^2))
    } else if(family=="binomial"){
      cvm[[l]] <- apply(X=hat[[l]],MARGIN=2,FUN=function(x) mean(-y*log(pmax(x,eps))-(1-y)*log(1-pmin(x,1-eps))))
    } else if(family=="cox"){
      cvm[[l]] <- apply(X=hat[[l]],MARGIN=2,FUN=function(x) glmnet::coxnet.deviance(pred=x,y=y))
    } else if(family=="poisson"){
      cvm[[l]] <- apply(X=hat[[l]],MARGIN=2,FUN=function(x) mean(2*(ifelse(y==0,0,y*log(y/x))-y+x)))
    } else {
      stop(paste0("Family \"",family,"\" is not implemented."))
    }
  }
  hyper$cvm <- cvm.min <- sapply(X=cvm,FUN=base::min)
  id.hyper <- which.min(cvm.min)
  lambda.min <- object.ext$model[[id.hyper]]$lambda[which.min(cvm[[id.hyper]])]
  
  list <- list(object=object.ext$model,hyper=hyper,id.hyper=id.hyper,lambda.min=lambda.min,scale=object.ext$scale)
  class(list) <- "cv.corila"
  return(list)
}

#'@title
#'predict (S3 method)
#'
#'@description
#'Makes predictions from an object of class \code{cv.corila}. 
#'
#'@param object object of class "cv.corila"
#'@param newx \eqn{n_1 \times p} matrix
#'@param s character "lambda.min" or numeric value
#'@param ... (not used)
#'
#'@inherit predict.corila return
#'
#'@seealso \code{\link{cv.corila}()}, \code{\link{coef.cv.corila}()}
#'
predict.cv.corila <- function(object,newx,s="lambda.min",...){
  if(s=="lambda.min"){
    s <- object$lambda.min
  } else if(!is.numeric(s)||length(s)!=1){
    stop("Set s=\"lambda.min\" or provide numeric value.")
  }
  newx_stand <- forescale(x=newx,pars=object$scale)$x
  x_all <- cbind(newx_stand,-newx_stand) 
  y_hat_stand <- stats::predict(object=object$object[[object$id.hyper]],newx=x_all,s=s,type="response")
  y_hat <- backscale(y=y_hat_stand,pars=object$scale)$y
  return(y_hat)
}

#'@title
#'Extract coefficients
#'
#'@description
#'Extracts coefficients from an object of class \code{cv.corila}.
#'
#'@inheritParams predict.cv.corila
#'
#'@seealso \code{\link{cv.corila}()}, \code{\link{predict.cv.corila}()}
#'
coef.cv.corila <- function(object,s="lambda.min"){
  if(s=="lambda.min"){
    s <- object$lambda.min
  } else if(!is.numeric(s)||length(s)!=1){
    stop("Set s=\"lambda.min\" or provide numeric value.")
  }
  coef_stand <- coef(object=object$object[[object$id.hyper]],s=s)
  if(object$scale$family=="cox"){
    alpha <- NA
    beta <- coef_stand
  } else {
    alpha <- coef_stand[1]
    beta <- coef_stand[-1]
  }
  if(any(beta<0)){stop("negative values")}
  beta_sum <- beta[1:(length(beta)/2)]-beta[(length(beta)/2+1):(length(beta))]
  coef <- c(alpha,beta_sum)
  coef <- backscale(coef=coef,pars=object$scale)$coef
  return(coef)
}

#----- simulation -----

simulate <- function(family="gaussian",n0=100,n1=10000,n.group=20,n.type=2,size.group=c(5,3),effect.size=c(1,1),corfac.feature=0.5,corfac.type=0.5,corfac.group=0.25,n.group.causal=2,prop.causal=0.5,noise.factor=1,plot=TRUE){
  # family="gaussian";n0=100;n1=10000;n.group=20;n.type=2;size.group=c(5,3);effect.size=c(1,1);corfac.feature=0.5;corfac.type=0.5;corfac.group=0.25;n.group.causal=2;prop.causal=0.5; noise.factor=1; plot=TRUE
  n <- n0 + n1
  
  if(n.type!=length(size.group)){stop("Wrong length.")}
  
  #- - - feature modalities and groups - - -
  p <- sum(n.group*size.group)
  type <- rep(x=seq_len(n.type),times=n.group*size.group) # original
  group <- unlist(lapply(size.group,function(x) rep(x=seq_len(n.group),each=x))) # original
  
  #group <- rep(x=seq_len(n.group),each=sum(size.group)) # trial 2025-09-22
  #type <- rep(x=rep(x=seq_len(n.type),times=size.group),times=n.group) # trial 2025-09-22
  
  #- - - effect vector - - -
  beta <- rep(x=0,times=p)
  index.common <- sample(x=seq_len(n.group),size=n.group.causal)
  cond <- group %in% index.common
  beta[cond] <- stats::rbinom(n=sum(cond),size=1,prob=prop.causal)*abs(stats::rnorm(n=sum(cond)))
  beta <- beta*rep(x=effect.size,times=table(type)) # original, added on 2025-06-20
  #for(i in seq_along(unique(type))){ # trial 2025-09-22
  #  beta[type==i] <- beta[type==i]*effect.size[i] # trial 2025-09-22
  #} # trial 2025-09-22
  if(plot){
    tryCatch(expr=graphics::plot(x=beta,col=group,pch=type),error=function(x) NULL)
  }
  
  #- - - feature matrix - - -
  mean <- rep(x=0,times=p)
  sigma <- matrix(data=NA,nrow=p,ncol=p)
  for(i in seq_len(p)){
    for(j in seq_len(p)){
      sigma[i,j] <- corfac.feature^(i!=j)*corfac.type^(type[i]!=type[j])*corfac.group^(group[i]!=group[j]) # original
      #sigma[i,j] <- ifelse(i==j,1,ifelse(group[i]==group[j] & type[i]==type[j],0.5,ifelse(group[i]==group[j],-0.25,ifelse(type[i]==type[j],0.125,-0.0625)))) # trial 2025-09-22
    }
  }
  if(any(diag(sigma)!=1)){stop("diagonal!=1")}
  if(plot){
    tryCatch(graphics::image(x=sigma[,p:1]),error=function(x) NULL)
  }
  x <- mvtnorm::rmvnorm(n=n,mean=mean,sigma=sigma)
  
  #- - - target vector - - -
  eta <- scale(x %*% as.vector(beta)) # was without scale
  if(family=="gaussian"){
    y <- eta + noise.factor*stats::rnorm(n=n,sd=stats::sd(eta)) # decrease/increase noise?
    if(stats::sd(y)==0){
      warning("Replacing constant y by random noise.")
      y <- stats::rnorm(n=n)
    }
  } else if(family=="binomial"){
    y <- stats::rbinom(n=n,size=1,prob=1/(1+exp(-2*eta))) # was without 2*
  } else if(family=="cox"){
    time <- stats::rexp(n=n,rate=exp(eta))
    status <- stats::rbinom(n=n,prob=0.5,size=1)
    #y <- cbind(time=time,status=status)
    y <- survival::Surv(time=time,event=status)
  } else if(family=="poisson"){
    y <- stats::rpois(n=n,lambda=exp(eta))
  } else {
    stop(paste("Family",family,"not implemented."))
  }
  
  #- - - outputs - - -
  fold <- rep(x=c(0,1),times=c(n0,n1))
  x_train <- x[fold==0,]
  y_train <- y[fold==0]
  x_test <- x[fold==1,]
  y_test <- y[fold==1]
  info <- data.frame(n0=n0,n1=n1,p=p,n.type=n.type,n.group=n.group,family=family)
  list <- list(x_train=x_train,type=type,group=group,y_train=y_train,x_test=x_test,y_test=y_test,beta=beta,info=info)
  return(list)
}

simulate_overlap <- function(){
  n0 <- 100
  n1 <- 10000
  n <- n0 + n1
  p <- 100
  n.group <- 20
  size.group <- rep(x=5,times=n.group) # sample(x=2:10,size=n.group,replace=TRUE)
  group <- lapply(X=seq_len(n.group),FUN=function(i) sort(sample(x=seq_len(p),size=size.group[i])))
  # Inside corila, put each feature that is in no group in a separate group?
  mean <- rep(x=0,times=p)
  sigma <- matrix(data=NA,nrow=p,ncol=p)
  #sigma <- 0.95^(abs(col(sigma)-row(sigma)))
  # alternative:
  for(i in seq_len(p)){
    for(j in seq_len(p)){
      group_i <- sapply(group,function(x) i %in% x)
      group_j <- sapply(group,function(x) j %in% x)
      sigma[i,j] <- 0.5^(i!=j)*0.25^(!any(group_i & group_j) & (i!=j))
    }
  }
  sigma <- as.matrix(Matrix::nearPD(x=sigma)$mat)
  x <- mvtnorm::rmvnorm(n=n,mean=mean,sigma=sigma)
  sel.group <- sample(x=seq_len(n.group),size=3)
  beta <- 1*(seq_len(p) %in% unlist(group[sel.group])) # multiply by abs(stats::rnorm(p))
  eta <- x %*% beta
  y <- eta + 0.5*stats::rnorm(n=n,sd=stats::sd(eta))
  fold <- rep(x=c(0,1),times=c(n0,n1))
  x_train <- x[fold==0,]
  y_train <- y[fold==0]
  x_test <- x[fold==1,]
  y_test <- y[fold==1]
  info <- data.frame(n0=n0,n1=n1,p=p,n.group=n.group)
  list <- list(x_train=x_train,group=group,y_train=y_train,x_test=x_test,y_test=y_test,beta=beta,info=info)
  return(list)
}

#----- comparison -----

calc_sign_prec <- function(truth,estim){
  sum(estim!=0 & truth!=0 & sign(estim)==sign(truth))/sum(estim!=0)
}

holdout <- function(x_train,y_train,group,type,family,x_test=NULL,y_test=NULL,nfolds=10,foldid=NULL,method=NULL,seed=NULL,init.multi=FALSE,trial=TRUE){
  # nfolds <- 10; foldid <- NULL; seed <- NULL; init.multi <- FALSE; trial <- TRUE
  
  p <- ncol(x_train)
  n0 <- nrow(x_train)
  n1 <- nrow(x_test)
  
  if(is.null(x_test)!=is.null(y_test)){
    stop("Provide either both or none of x_test and y_test.")
  }
  
  if(is.null(foldid)){
    foldid <- sample(rep(x=seq_len(nfolds),length.out=n0))
  }
  
  if(is.null(method)){
    if(is.numeric(group)){
      method <- c("mean","ridge","lasso","gglasso","grpreg","sparsegl","SGL","grpregOverlap","scoop","ecpc","squeezy","MLGL","pcLasso","corila") # multiridge could also be added (needs some coding) # multiview is not for groups (only modalities)
    } else if(is.list(group)){
      method <- c("mean","ridge","lasso","grpregOverlap","ecpc","squeezy","corila") # overlapping groups (multiridge could also be adapted)
    }
    warning("omitting slow methods ...")
    method <- method[!method %in% c("SGL","ecpc","squeezy","scoop")] # omit slow methods (temporary)
    method <- method[method!="pcLasso"] # bug in application (singletons?)
    #method <- method[method!="multiridge"] # bug in binomial case
  }
  
  if(!is.null(x_test)){
    y_hat <- sapply(X=method,FUN=function(x) rep(x=NA,times=n1),simplify=FALSE)
  } else {
    y_hat <- NULL
  }
  coef <- sapply(X=method,FUN=function(x) rep(x=NA,times=p),simplify=FALSE)
  #y_hat <- coef <- list()
  
  difftime <- numeric()
  
  for(i in method){
    if(!is.null(seed)){set.seed(seed)}
    start <- Sys.time()
    if(i=="mean"){
      #--- prediction by the mean ---
      if(!is.null(x_test)){
        y_hat$mean <- rep(x=mean(y_train),times=nrow(x_test))
      }
      if(family=="cox"){warning("Implement intercept-only model for Cox regression.")}
      coef$mean <- c(ifelse(family=="binomial",log(mean(y_train)/(1-mean(y_train))),mean(y_train)),rep(x=0,times=ncol(x_train)))
    } else if(i=="ridge"){
      #--- ridge ---
      object <- glmnet::cv.glmnet(x=x_train,y=y_train,family=family,alpha=0,foldid=foldid)
      if(!is.null(x_test)){
        y_hat$ridge <- stats::predict(object=object,newx=x_test,s="lambda.min",type="response")
      }
      coef$ridge <- stats::coef(object=object,s="lambda.min")
    } else if(i=="multiridge"){
      if(family %in% c("cox","poisson")){
        warning("Implement Cox regression for multiridge.")
        next
      }
      #--- multiridge ---
      object <- multiridge(x=x_train,y=y_train,z=type,family=family)
      if(!is.null(x_test)){
        y_hat$multiridge <- stats::predict(object=object,newx=x_test)
      }
      coef$multiridge <- stats::coef(object=object)
    } else if(i=="lasso"){
      #--- lasso ---
      object <- glmnet::cv.glmnet(x=x_train,y=y_train,family=family,alpha=1,foldid=foldid)
      if(!is.null(x_test)){
        y_hat$lasso <- stats::predict(object=object,newx=x_test,s="lambda.min",type="response")
      }
      coef$lasso <- stats::coef(object=object,s="lambda.min")
    } else if(i=="gglasso"){
      if(family=="cox"){next}
      #--- group lasso (gglasso) ---
      if(family=="binomial"){
        temp <- as.factor(y_train)
      } else {
        temp <- y_train
      }
      object <- gglasso::cv.gglasso(x=x_train,y=y_train,group=group,foldid=foldid)
      if(!is.null(x_test)){
        temp <- stats::predict(object=object,newx=x_test,s="lambda.min",type="link")
        if(family=="binomial"){
          y_hat$gglasso <- 1/(1+exp(-temp))
        } else {
          y_hat$gglasso <- temp
        }
      }
      coef$gglasso <- stats::coef(object,s="lambda.min")
    } else if(i=="grpreg"){
      #if(family=="cox"){next}
      #--- group lasso (grpreg) ---
      if(family=="cox"){
        object <- grpreg::cv.grpsurv(X=x_train,y=y_train,group=group,fold=foldid)
      } else {
        object <- grpreg::cv.grpreg(X=x_train,y=y_train,family=family,group=group,fold=foldid)
      }
      if(!is.null(x_test)){
        y_hat$grpreg <- stats::predict(object=object,X=x_test,type="response",lambda=object$lambda.min)
      }
      coef$grpreg <- stats::coef(object=object,lambda=object$lambda.min)
    } else if(i=="grplasso"){
      #--- group lasso (grplasso) ---
      ## This package requires the user to implement hyperparameter tuning.
      # if(family=="cox"){next}
      # if(family=="gaussian"){
      #   model <- grplasso::LinReg()
      # } else if(family=="binomial"){
      #   model <- grplasso::LogReg()
      # } else if(family=="poisson"){
      #   model <- grplasso::PoissReg()
      # }
      # lambda <- grplasso::lambdamax(x=cbind(1,x_train),y=y_train,index=c(NA,group),penscale=base::sqrt,model=model)*0.9^(0:100)
      # object <- grplasso::grplasso(x=cbind(1,x_train),y=y_train,index=c(NA,group),model=model,lambda=lambda,control=grplasso::grpl.control(update.hess="lambda",trace=0))
      # if(!is.null(x_test)){
      #   y_hat$grplasso <- stats::predict(object=object,newdata=cbind(1,x_test),type="response")
      # }
      # coef$grplasso <- object$coefficients[,1]
    } else if(i=="sparsegl"){
      if(family %in% c("poisson","cox")){next}
      #--- sparse group lasso (sparsegl) ---
      object <- sparsegl::cv.sparsegl(x=x_train,y=y_train,group=group,family=family,foldid=foldid)
      if(!is.null(x_test)){
        y_hat$sparsegl <- stats::predict(object=object,newx=x_test,type="response",s="lambda.min")
      }
      coef$sparsegl <- stats::coef(object,s="lambda.min")
    } else if(i=="SGL"){
      if(family=="poisson"){next}
      #--- sparse group lasso (SGL) ---
      family_temp <- ifelse(family=="gaussian","linear",ifelse(family=="binomial","logit",family))
      if(family=="cox"){
        data_temp <- list(x=x_train,time=as.matrix(y_train)[,"time"],status=as.matrix(y_train)[,"status"])
      } else {
        data_temp <- list(x=x_train,y=y_train)
      }
      cv_object <- SGL::cvSGL(data=data_temp,index=group,type=family_temp,foldid=foldid)
      object <- SGL::SGL(data=data_temp,index=group,type=family_temp,lambdas=cv_object$lambdas)
      if(!is.null(x_test)){
        y_hat$SGL <- SGL::predictSGL(x=object,newX=x_test,lam=which.min(cv_object$lldiff))
      }
      if(family=="gaussian"){
        coef$SGL <- c(object$intercept,object$beta[,which.min(cv_object$lldiff)])
      } else {
        coef$SGL <- c(object$intercept[which.min(cv_object$lldiff)],object$beta[,which.min(cv_object$lldiff)])
      }
    } else if(i=="grpregOverlap"){
      #--- grpregOverlap (only on GitHub) ---
      func <- grpregOverlap::expandX
      body(func)[[3]] <- quote(over.mat <- Matrix(incidence.mat %*% t(incidence.mat), sparse=TRUE))
      utils::assignInNamespace(x="expandX",value=func,ns="grpregOverlap")
      if(is.numeric(group)){
        list <- c(lapply(X=unique(group),FUN=function(z) which(group==z)),lapply(X=unique(type),FUN=function(z) which(type==z)))
      } else {
        list <- group
      }
      if(family=="cox"){
        object <- grpregOverlap::cv.grpsurvOverlap(X=x_train,y=y_train,group=list)
      } else {
        object <- grpregOverlap::cv.grpregOverlap(X=x_train,y=y_train,group=list,family=family)
      }
      if(!is.null(x_test)){
        y_hat$grpregOverlap <- stats::predict(object=object,X=x_test,type="response",lambda=object$lambda.min)
      }
      coef$grpregOverlap <- stats::coef(object=object,lambda=object$lambda.min)
    } else if(i=="multiview"){
      #--- multiview (agreement between different modalities) ---
      object <- list()
      if(family=="gaussian"){
        temp <- stats::gaussian()
      } else if(family=="binomial"){
        temp <- stats::binomial()
      } else if(family=="poisson"){
        temp <- stats::poisson()
      }
      rho <- c(0.00,0.10,0.25,0.50,1.00)
      for(j in seq_along(rho)){
        object[[j]] <- multiview::cv.multiview(x_list=lapply(X=unique(type),FUN=function(z) x_train[,type==z]),y=y_train,family=temp,rho=rho[j],foldid=foldid)
      }
      id <- which.min(sapply(object,function(x) min(x$cvm)))
      if(!is.null(x_test)){
        y_hat$multiview <- stats::predict(object=object[[id]],newx=lapply(X=unique(type),FUN=function(z) x_test[,type==z]),type="response",s="lambda.min")
      }
      coef$multiview <- stats::coef(object=object[[id]],s="lambda.min")
    } else if(i=="scoop"){
      if(!family %in% c("gaussian","binomial")){next}
      if(all(table(group)==1)){
        #group_temp <- rep(x=1,times=length(group))
        object <- scoop::coop.lasso(x=x_train,y=y_train,group=group,family=family)
      } else {
        object <- scoop::sparse.coop.lasso(x=x_train,y=y_train,group=group,family=family)
      }
      object.cv <- scoop::crossval(object)
      id <- which(object.cv@lambda==object.cv@lambda.min)
      if(!is.null(x_test)){
        y_hat$scoop <- scoop::predict(object=object,newx=x_test)[,id]
      }
      coef$scoop <- object.cv@beta.min
    } else if(i=="MLGL"){
      #--- multi-layer group-lasso ---
      if(!family %in% c("gaussian","binomial")){next}
      loss <- ifelse(family=="gaussian","ls","logit")
      if(loss=="logit"){
        y_train_temp <- 2*y_train-1
      } else {
        y_train_temp <- y_train
      }
      cv <- MLGL::cv.MLGL(X=x_train,y=y_train_temp,loss=loss)
      object <- MLGL::MLGL(X=x_train,y=y_train_temp,loss=loss)
      if(!is.null(x_test)){
        temp <- stats::predict(object=object,newx=x_test,type="fit",s=cv$lambda.min)
        if(loss=="ls"){
          y_hat$MLGL <- temp
        } else {
          y_hat$MLGL <- 1/(1+exp(-temp))
        }
      }
      coef$MLGL <- coef(object=object,s=cv$lambda.min)
    } else if(i=="ecpc"){
      if(model=="poisson"){next}
      if(family=="cox"){
        Y_temp <- y_train
      } else {
        Y_temp <- matrix(y_train,ncol=1)
      }
      #--- ecpc ---
      model <- ifelse(family=="gaussian","linear",ifelse(family=="binomial","logistic",family))
      if(is.numeric(group)){
        null <- utils::capture.output(groupset <- ecpc::createGroupset(values=as.factor(group)))
      } else if(is.list(group)){
        base <- lapply(group,function(x) as.integer(x))
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)] # first alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])# second alternative
        groupset <- c(base,extra)
      }
      null <- utils::capture.output(typeset <- ecpc::createGroupset(values=as.factor(type)))
      datablocks <- lapply(X=unique(type),FUN=function(x) which(type==x))
      null <- tryCatch(utils::capture.output(object <- ecpc::ecpc(Y=Y_temp,X=x_train,groupsets=list(groupset),X2=x_test,model=model,fold=nfolds,datablocks=NULL)),error=function(x) NULL)
      # Currently typeset/datablocks is ignored!
      if(!is.null(object)){
        coef$ecpc <- unlist(stats::coef(object))
      }
      if(!is.null(object) & !is.null(x_test)){
        y_hat$ecpc <- object$Ypred
      }
    } else if(i=="gren"){
      #partitions <- list(group=lapply(X=unique(group),FUN=function(x) which(group==x)),type=lapply(X=unique(type),FUN=function(x) which(type==x)))
      #object <- gren::cv.gren(x=x_train,y=y_train,partitions=list(group=group,type=type),trace=TRUE)
      warning("Implement GREN.")
    } else if(i=="squeezy"){
      if(family %in% c("poisson","cox")){next}
      if(is.numeric(group)){
        groupset <- lapply(unique(group),function(x) which(group==x))
      } else if(is.list(group)){
        base <- lapply(group,function(x) as.integer(x))
        #extra <- seq_len(p)[!seq_len(p) %in% unlist(base)] # first alternative
        extra <- list(seq_len(p)[!seq_len(p) %in% unlist(base)])# second alternative
        groupset <- c(base,extra)
      }
      object <- squeezy::squeezy(Y=y_train,X=x_train,groupset=groupset,X2=x_test) # Check whether type can be included (e.g., as groups).
      y_hat$squeezy <- object$YpredApprox
      coef$squeezy <- c(object$a0Approx,object$betaApprox)
    } else if(i=="CBPE"){
      if(FALSE){
        n <- 100
        p <- 200
        x_train <- matrix(rnorm(n * p), n, p)
        beta_true <- c(0.5, -1, 2, 5,rep(0,times=196))
        y_train <- rbinom(n, 1, 1 / (1 + exp(-x_train %*% beta_true)))
      }
      if(family=="gaussian"){
        CBPE <- CBPE::CBPLinearE
      } else if(family=="binomial"){
        CBPE <- CBPE::CBPLogisticE
      } else {
        next
      }
      lambda <- exp(seq(from=log(1e06),to=log(1e-06),length.out=20))
      # internal cross-validation to tune lambda
      # refit on full training data with optimal lambda
      stop("Not yet implemented.")
    } else if(i=="pcLasso"){
      group_temp <- lapply(unique(group),function(x) which(x==group))
      if(!family %in% c("gaussian","binomial")){next}
      ratio <- c(seq(from=0.25,to=0.75,by=0.25),0.9,0.95,1) # set is from paper
      object <- list()
      for(j in seq_along(ratio)){
        null <- utils::capture.output(object[[j]] <- pcLasso::cv.pcLasso(x=x_train,y=y_train,family=family,groups=group_temp,ratio=ratio[j]))
      }
      id <- which.min(sapply(X=object,FUN=function(x) min(x$cvm)))
      object <- object[[id]]
      if(!is.null(x_test)){
        y_hat$pcLasso <- pcLasso::predict.cv.pcLasso(object=object,xnew=x_test,s="lambda.min")
      }
      fit <- 
        coef$pcLasso <- c(object$glmfit$a0[which(object$lambda==object$lambda.min)],object$glmfit$beta[, which(object$lambda==object$lambda.min)]) # this is different for overlapping groups
    } else if(i=="corila"){
      #--- lasso with feature groups and modalities ---
      object <- cv.corila(x=x_train,y=y_train,group=group,type=type,family=family,mode="mean",foldid=foldid,init.multi=init.multi,trial=trial)
      print(object$hyper[object$id.hyper,])
      if(!is.null(x_test)){
        y_hat$corila <- stats::predict(object=object,newx=x_test)
      }
      coef$corila <- stats::coef(object=object)
    }
    end <- Sys.time()
    difftime[i] <- difftime(time1=end,time2=start,units="secs")
  }
  
  #- - - checks - - -
  if(family!="cox"){
    method <- names(y_hat)
    if(!is.null(x_test)){
      if(family=="binomial"){
        if(min(sapply(X=y_hat,FUN=function(x) base::min(x,na.rm=TRUE)))<0){stop("too small")}
        if(max(sapply(X=y_hat,FUN=function(x) base::max(x,na.rm=TRUE)))>1){stop("too large")}
      }
      for(i in seq_along(method)){
        original <- y_hat[[i]]
        if(all(is.na(original))){next}
        manual <- coef[[i]][1] + x_test %*% coef[[i]][-1]
        if(family=="binomial"){
          manual <- 1/(1+exp(-manual)) 
        }
        #cond <- is.na(original)|is.na(manual)
        #if(any(cond)){
        #  message("coef:",paste(head(coef[[i]]),collapse=" "))
        #  message("original:",paste(head(original),collapse=" "))
        #  message("manual:",paste(head(manual),collapse=" "))
        #}
        #message("original: ",paste0(original[cond],collapse=" "))
        #message("manual: ",paste0(manual[cond],collapse=" "))
        if(any(abs(original-manual)>0.001)){
          warning(paste("unequal:",method[i]))
        }
        if(any(stats::sd(original)!=0 & stats::sd(original)!=0 && stats::cor(original,manual)<0.999)){warning(paste("correlation:",method[i]))}
      }
    }
    
    if(!is.null(x_test)){
      if(family=="binomial" & any(sapply(X=y_hat,FUN=function(x) any(x<0|x>1,na.rm=TRUE)))){stop("invalid y_hat range")}
      if(any(sapply(X=y_hat,FUN=base::length)!=n1)){"invalid y_hat length"}
    }
    if(any(sapply(X=coef,FUN=base::length)!=p+1)){"invalid coef length"}
  } else {
    warning("Implement checks for Cox regression.")
  }
  
  list <- list(y_hat=y_hat,coef=coef,difftime=difftime)
  return(list)
}

crossval <- function(x,y,family,group=NULL,type=NULL,iter=5,nfolds=10,init.multi=FALSE,trial=FALSE,method=NULL,...){
  n <- nrow(x)
  p <- ncol(x)
  if(is.null(group)){group <- seq_len(p)}
  if(is.null(type)){type <- rep(x=1,times=p)}
  list <- list()
  list$metric <- list$nzero <- list()
  for(k in seq_len(iter)){
    set.seed(k)
    cat("iter",k,"\n")
    foldid <- sample(rep(x=seq_len(nfolds),length.out=n)) # use balanced folds for binomial family (same for internal folds)
    y_hat <- data.frame(row.names=seq_len(n))
    for(i in seq_len(nfolds)){
      cat("fold",i,"\n")
      cond <- foldid==i
      results <- holdout(x_train=x[!cond,],y_train=y[!cond],x_test=x[cond,],y_test=y[cond],group=group,type=type,family=family,nfolds=10,foldid=NULL,method=method,seed=NULL,init.multi=init.multi,trial=trial)
      for(j in seq_along(results$y_hat)){
        y_hat[[names(results$y_hat)[j]]][cond] <- results$y_hat[[j]]
      }
    }
    if(family %in% c("gaussian","poisson")){
      list$metric[[k]] <- apply(X=y_hat,MARGIN=2,FUN=function(x) mean((y-x)^2))
    } else if(family=="binomial"){
      list$metric[[k]] <- apply(X=y_hat,MARGIN=2,FUN=function(x) pROC::auc(response=y,predictor=as.vector(x),levels=c(0,1),direction="<"))
    } else if(family=="cox"){
      list$metric[[k]] <- apply(X=y_hat,MARGIN=2,FUN=function(x) survival::concordance(y~I(-x))$concordance)
    }
    set.seed(k)
    refit <- holdout(x_train=x,y_train=y,group=group,type=type,family=family,nfolds=10,foldid=NULL,method=method,seed=NULL,init.multi=init.multi,trial=trial)
    list$nzero[[k]] <- sapply(X=refit$coef,FUN=function(x) sum(x[-1]!=0))
  }
  list <- lapply(X=list,FUN=function(x) do.call(what="rbind",args=x))
  list$family <- family
  return(list)
}

plot_boxes <- function(x,base="corila",main="",decrease=TRUE,ylim=NULL){
  #--- hypothesis testing ---
  p.worse <- apply(x,2,function(c) ifelse(all(is.na(c)),NA,stats::wilcox.test(x=c,y=x[,base],paired=TRUE,alternative=ifelse(decrease,"greater","less"),exact=FALSE)$p.value))
  p.better <- apply(x,2,function(c) ifelse(all(is.na(c)),NA, stats::wilcox.test(x=c,y=x[,base],paired=TRUE,alternative=ifelse(decrease,"less","greater"),exact=FALSE)$p.value))
  col <- ifelse(p.worse<=0.05,"red",ifelse(p.better<=0.05,"blue","grey"))
  #--- boxplot ---
  graphics::boxplot(x=x,main=main,las=2,col=col,frame.plot=FALSE,xaxt="n",yaxt="n",ylim=ylim)
  #--- horizontal axis ---
  col <- list(grey=which(p.worse<=0.05|is.na(p.worse)),black=which(p.worse>0.05))
  for(i in seq_along(col)){
    graphics::axis(side=1,at=seq_len(ncol(x))[col[[i]]],labels=colnames(x)[col[[i]]],las=2,col.axis=names(col)[i],tick=FALSE,line=-0.5)
  }
  graphics::abline(h=stats::median(x[,"mean"]),lty=2,col="grey") # temporary
  #--- vertical axis ---
  usr <- graphics::par("usr")
  mar.big <- 0.05*(usr[4]-usr[3])
  mar.small <- 0.05*(usr[4]-usr[3])
  graphics::axis(side=2,col="grey",col.axis="grey")
  graphics::arrows(x0=usr[1],y0=usr[3]+mar.big,x1=usr[1],y1=usr[4]-mar.big,length=0.1,xpd=TRUE,code=ifelse(decrease,1,2),lwd=2)
  graphics::text(x=usr[1],y=c(usr[4]+mar.small,usr[3]-mar.small)[1+c(!decrease,decrease)],labels=c("-","+"),col=c("red","blue"),xpd=TRUE,cex=1.5,font=2)
}
