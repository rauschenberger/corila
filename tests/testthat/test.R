
testthat::test_that("1+1=1",{
  testthat::expect_true(TRUE)
})

#-------------------------------------------------
#----- functions "forescale" and "backscale" -----
#-------------------------------------------------

for(family in c("gaussian","binomial")){

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
}

# without standardisation
object.original <- glmnet::glmnet(x=x[foldid==0,],y=y[foldid==0],family=family,lambda=0)
y_hat.original <- predict(object=object.original,newx=x[foldid==1,],type="response")
coef.original <- as.numeric(coef(object.original,s=0))

# with standardisation
data.scaled <- forescale(x=x[foldid==0,],y=y[foldid==0],family=family)
object.scaled <- glmnet::glmnet(x=data.scaled$x,y=data.scaled$y,family=family,lambda=0,intercept=(family!="gaussian"))
newx.scaled <- forescale(x=x[foldid==1,],pars=data.scaled$pars)
y_hat.scaled <- predict(object=object.scaled,newx=newx.scaled$x,type="response")
coef.scaled <- coef(object=object.scaled,s=0)
backscaled <- backscale(pars=data.scaled$pars,y=y_hat.scaled,coef=coef.scaled)

all.equal(target=backscaled$y,current=y_hat.original)
all.equal(target=backscaled$coef,current=coef.original)

}

#-----------------------------
#----- function "corila" -----
#-----------------------------

# Run this code with trial=TRUE/FALSE and family="gaussian"/"binomial".
trial <- TRUE
family <- "binomial"

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

# Test if corila returns same results with argument group as vector, as list, or as matrix.
coef <- lapply(X=model,FUN=coef)
all.equal(coef[[1]],coef[[2]])
all.equal(coef[[1]],coef[[3]])
y_hat <- lapply(X=model,FUN=function(x) predict(object=x,newx=data$x_test))
all.equal(y_hat[[1]],y_hat[[2]])
all.equal(y_hat[[1]],y_hat[[3]])

# Test if function predict returns same results as feature matrix times coef.
eta <- coef$vector[1] + data$x_test %*% coef$vector[-1]
if(family=="gaussian"){
  pred <- eta
} else if(family=="binomial"){
  pred <- 1/(1+exp(-eta))
}
all.equal(as.numeric(pred),as.numeric(y_hat$vector))

# Test if regression with original and standardised features returns same predictions.
family <- "binomial" # try "gaussian" and "binomial"
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
}
X$scaled <- scale(X$original)
y_hat <- list()
for(i in seq_along(X)){
  set.seed(1)
  object <- cv.corila(x=X[[i]],y=y,group=rep(1:5,each=10),family=family,trial=TRUE)
  y_hat[[i]] <- predict(object=object,newx=X[[i]])
}
all.equal(y_hat[[1]],y_hat[[2]])

#-----------------------------
#--- function "multiridge" ---
#-----------------------------

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

