## @knitr labSimple
inConf <- 0 #initialize counter
for (i in 1:1000){ #repeat 1000 times
  #generate data according to normal
  group1 <- rnorm(n=10,mean=20,sd=1)
  group2 <- rnorm(n=10,mean=3,sd=1)
  #get confidence interval from t-test
  tResult <- t.test(group1, group2, var.equal=TRUE)
  confInter <- tResult$conf.int
  #check if true mean difference (17) is in CI
  if (17 > confInter[1] && 17 < confInter[2]){
    inConf <- inConf + 1
  } #end of if
} #end of for
print(inConf/1000)
