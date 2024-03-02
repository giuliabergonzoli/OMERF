library(simstudy)
treef=function(a,b,c) {
  if(a<1) {
    if(abs(c)<3) y= 2
    else {
      if(b<5) y=-2
      else  y= 20
    }
  }
  else {
    if(b< -1) y=ifelse(c<0,1,-1)
    else  y=0
  }
  y
}


build.dataset=function (n,sigma1,sigma2, prop) {
  #n=number of data to generate (train+test)
  #sigma1 and sigma2=variances related to random effects
  #prop=proportion of data to be used for the train
  
  #random intercept and random slope
  gen.rand <- defData(varname = "b0i", dist = "normal", formula = 0, variance = sigma1,
                         id = "group")
  gen.rand <- defData(gen.rand, varname = "b1i", dist = "normal", formula = 0, variance = sigma2, id = "group")
  gen.rand <- defData(gen.rand,varname = "clustSize", formula = n, dist = "clusterSize")
  dtRandom <- genData(10, gen.rand)
  
  # normal  and uniform covariates
  gen.obs <- defDataAdd(varname = "x1", dist = "normal",
                        formula =  0, variance = 1)
  gen.obs <- defDataAdd(gen.obs, varname = "x2", dist = "normal", 
                        formula = 0, variance = 1)
  gen.obs <- defDataAdd(gen.obs, varname = "x3", dist = "normal", 
                        formula = 0, variance = 1)
  gen.obs <- defDataAdd(gen.obs, varname = "x4", dist = "uniform",
                        formula =  "-3;3")
  gen.obs <- defDataAdd(gen.obs, varname = "x5", dist = "uniform",
                        formula =  "-6;6")
  gen.obs <- defDataAdd(gen.obs, varname = "x6", dist = "uniform",
                        formula =  "-5;5")
  gen.obs <- defDataAdd(gen.obs, varname = "x7", dist = "uniform",
                        formula =  "-4;4")
  dtObs <- genCluster(dtRandom, cLevelVar = "group", numIndsVar = "clustSize",
                      level1ID = "id")
  dtObs <- addColumns(gen.obs, dtObs)
  
  gen.z <- defDataAdd(varname = "z",
                      formula = "0.7*(3 + 7*x1^2 -5*x2 + x2*x3^2) + 0.3*treef(x4,x5,x6) + b0i + b1i*x1", dist = "nonrandom")
  
  dtObs2 <- addColumns(gen.z, dtObs)
  
  baseprobs <- c(5/12,6/12,1/12)
  data <- genOrdCat(dtObs2, adjVar = 'z', baseprobs = baseprobs, catVar = "y")
  data <- as.data.frame(data)
  
  data$group=as.factor(data$group)
  data$y=as.factor(data$y)
  
  library(caret)
  split_indices <- createDataPartition(data$y, p = prop, list = FALSE)
  cov=data[,c('x1','x2','x3','x4','x5','x6','x7')]
  cov.test=cov[-split_indices,]
  cov.train=cov[split_indices,]
  y=data[,c('y')]
  y.test=y[-split_indices]
  y.train=y[split_indices]
  group=data[,c('group')]
  group.test=group[-split_indices]
  group.train=group[split_indices]
  bi=data[,c('b0i','b1i')]
  bi.test=bi[-split_indices,]
  bi.train=bi[split_indices,]
  
  res=list(cov.test, cov.train, y.test, y.train, group.test, group.train, bi.test, bi.train)
  names(res)=c('cov.test', 'cov.train', 'y.test', 'y.train', 'group.test', 'group.train', 'bi.test', 'bi.train')
  res
}





