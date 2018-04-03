

#' DISCLAIMER: This function is not an example of a good programming style as it avoids using many advanced feature in order to enhance comprehension.
#'
#' This function checks the validity of the t-test under different conditions
#'
#' @param n total sample size group1+group2. integer number
#'
#' @param normal normality assumption fullfilled? TRUE or FALSE
#'
#' @param homogeneity homogeneity assumption fullfilled? TRUE or FALSE
#'
#' @param equalGroups equal group sizes? TRUE or FALSE
#'
#' @param independence independence assumption fullfilled? TRUE or FALSE
#'
#' @return estimate coverage of the confidence interval estimator
#' @author Julian D Karch, \email{j.d.karch@fsw.leidenuniv.nl}
checkTTest <- function(n,normal=TRUE,homogeneity=TRUE,equalGroups=TRUE,independence=TRUE){
  #define means for group1 and group2
  trueMeanDiff <- 0.1
  mean1 <- 1 + trueMeanDiff
  mean2 <- 1
  simReps <- 1000 #number of simulated data sets

  #Homogeneity?
  if (homogeneity){
    var1 <- 10
    var2 <- 10
  }else{
    var1 <- 10
    var2 <- 1000000 #one million
  }

  #Equal group sizes?
  if (equalGroups){
    proportion1 <- 0.5
  }else{
    proportion1 <- 0.9
  }
  n1 <- n*proportion1
  n2 <- n*(1-proportion1)

  #Normality?
  if (normal && independence){
    simulateG1 <- function(){rnorm(n1,mean=mean1,sd=sqrt(var1))}
    simulateG2 <- function(){rnorm(n2,mean=mean2,sd=sqrt(var2))}
  }else if (!normal && independence){
    simulateG1 <- function(){rlnorm(n1,meanlog = mean1)}
    simulateG2 <- function(){rlnorm(n2,meanlog = mean2)}
  }else if (normal && !independence){
    simulateG1 <- function(){
      factorScore <- rnorm(1,mean=mean1,sd=sqrt(var1))
      y <- factorScore+rnorm(n1,mean=0,sd=sqrt(var1))
    }
    simulateG2 <- function(){
      factorScore <- rnorm(1,mean=mean2,sd=sqrt(var2))
      y <- factorScore+rnorm(n1,mean=0,sd=sqrt(var2))
    }
    factorScore1 <- rnorm(1,mean=mean1,sd=sqrt(var1))
  }

  #actual loop, very similar to labSimple.R
  popInConf <- 0
  for (i in 1:simReps){
    ourData <- data.frame(groups=as.factor(c(rep(1,n1),rep(2,n2))),
                          measurements=c(simulateG1(),simulateG2()))
    results <- t.test(measurements ~ groups, data=ourData,var.equal=TRUE)
    if ((results$conf.int[1] <= trueMeanDiff) && (results$conf.int[2] >= trueMeanDiff)){
      popInConf <- popInConf + 1
    }
  }
  return(popInConf/simReps)
}

