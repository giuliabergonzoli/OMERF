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

n=1000
prop=0.8

build.dataset=function (nr, prop) {
  #param=matrix with parameters
  #j=parameter line to be used
  #n=number of data to generate (train+test)
  #prop=proportion of data to be used for the train
  
  # 'formula=0' with 'dist="normal"' acts as a harmless placeholder since we don't need a real variable.
  # The key point is that 'id = "group"' will ensure unique IDs from 1 to 10.
  
  gen.group <- defData(varname = "temp_var", formula = 0, dist = "normal", id = "group")
  gen.group <- defData(gen.group, varname = "clustSize", dist = "uniformInt", formula = "50;100")
  set.seed(nr)
  dtGroups <- genData(15, gen.group)
  
  # normal and uniform covariates
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

  # dtObs <- genCluster(dtGroups, cLevelVar = "group", numIndsVar = n/10, level1ID = "id")
  # dtObs <- addColumns(gen.obs, dtObs)
  
  dtObs <- genCluster(dtGroups, cLevelVar = "group", numIndsVar = "clustSize",
                      level1ID = "id")
  dtObs <- addColumns(gen.obs, dtObs)
  
  gen.z <- defDataAdd(varname = "z",
                      formula = "0.3*(3 + 7*x1^2 - 5*x2 + x2*x3^2) + 0.7*treef(x4, x5, x6)",
                      dist = "nonrandom")
  
  dtObs2 <- addColumns(gen.z, dtObs)
  
  
  baseprobs <- c(0.32, 0.36, 0.29, 0.03)
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
  
	res=list(cov.test, cov.train, y.test, y.train, group.test, group.train)
	names(res)=c('cov.test', 'cov.train', 'y.test', 'y.train', 'group.test', 'group.train')
	res
}

	
	

	
