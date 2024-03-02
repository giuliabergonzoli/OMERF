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


build.dataset=function (n,sigma, prop) {
  #n=number of data to generate (train+test)
  #sigma=variance related to random effects
  #prop=proportion of data to be used for the train
  
  #random intercept
  gen.randInt <- defData(varname = "bi", dist = "normal", formula = 0, variance = sigma,
                         id = "group")
  gen.randInt <- defData(gen.randInt,varname = "clustSize", formula = n, dist = "clusterSize")
  dtRandom <- genData(10, gen.randInt)
  
  # normal and unifrom covariates
  gen.obs <- defDataAdd(varname = "x1", dist = "normal",
                        formula =  0, variance = 1)
  gen.obs <- defDataAdd(gen.obs, varname = "x2", dist = "normal", 
                        formula = 0, variance = 1)
  gen.obs <- defDataAdd(gen.obs, varname = "x3", dist = "normal", 
                        formula = 0, variance = 1)
  dtObs <- genCluster(dtRandom, cLevelVar = "group", numIndsVar = "clustSize",
                      level1ID = "id")
  dtObs <- addColumns(gen.obs, dtObs)
  
  gen.z <- defDataAdd(varname = "z",
                      formula = "3 + 7*x1 -5*x2 + x2*x3 + bi", dist = "nonrandom")
  
  dtObs2 <- addColumns(gen.z, dtObs)
  
  baseprobs <- c(5/12,6/12,1/12)
  data <- genOrdCat(dtObs2, adjVar = 'z', baseprobs = baseprobs, catVar = "y")
  data <- as.data.frame(data)
  
  data$group=as.factor(data$group)
  data$y=as.factor(data$y)
  
  library(caret)
  split_indices <- createDataPartition(data$y, p = prop, list = FALSE)
  cov=data[,c('x1','x2','x3')]
  cov.test=cov[-split_indices,]
  cov.train=cov[split_indices,]
  y=data[,c('y')]
  y.test=y[-split_indices]
  y.train=y[split_indices]
  group=data[,c('group')]
  group.test=group[-split_indices]
  group.train=group[split_indices]
  bi=data[,c('bi')]
  bi.test=bi[-split_indices]
  bi.train=bi[split_indices]
  
	res=list(cov.test, cov.train, y.test, y.train, group.test, group.train, bi.test, bi.train)
	names(res)=c('cov.test', 'cov.train', 'y.test', 'y.train', 'group.test', 'group.train', 'bi.test', 'bi.train')
	res
}

	
	

	
