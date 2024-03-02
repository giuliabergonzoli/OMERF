omerf= function (y, cov, group, xnam=NULL, znam=NULL, bizero=NULL, 
                 itmax=100, toll=0.05) {
  #ordinal mixed-effects random forest (OMERF)
  #inputs: 
  #-y = vector of responses
  #-cov = data frame with all the fixed covariates for each statistical unit
  #-group = factor vector which, for each statistical unit, tells the group to which it belongs (random effect)
  #-interactions = vector with the the interactions between the fixed covariates to be used in the CLM
  #-quadratic = vector with the quadratic the fixed covariates to be used in the CLM
  #-xnam = vector with the names of the fixed covariates to be used in the random forest
  #-znam = vector with the names of the covariates to be used in the random effects
  #-bizero = matrix containing the coefficients of the random effects
  #          (the first value of each column is the intercept, the other ones are the covariates znam)
  # assume that group and bizero are coherent, ie b[,i] corresponds to levels(group)[i]
  
  
  
  ######################################
  ######	STEP 1: Initialization  ######
  ######################################
  
  N <- length(y) #number of observations
  n=length(levels(group)) #number of groups
  
  q <- length(znam)+1	#number of random covariates + random intercept
  Zi=NULL
  z.not.null=!(is.null(znam)) #check whether there are covariates included in the random effects
  if (z.not.null) Zi=cov[znam] #random effects covariates
  Zi.int= cbind(rep(1,N),Zi) #random intercept + random effects covariates
  
  #Initialize (if NULL) bi to 0
  if( is.null(bizero) ){
    bi <- NULL
    for(i in 1:n) bi=cbind(bi,rep(0,q))
  }	
  if( !is.null(bizero) ) bi=bizero
  lev=levels(group) #group names
  bi=data.frame(bi)
  names(bi)=lev
  all.bi=list()  #b_i of each iteration
  all.bi[[1]]=bi
  
  #if xnam is NULL assume that all cov variables are to be used
  if(is.null(xnam)) xnam=names(cov)
  
  #group must be a factor, otherwise it gives an error
  if(!is.factor(group)) stop('The "group" argument must be a factor')
  
  forest.formula=as.formula(paste("target ~ ", paste(xnam, collapse= "+")))
  
  if(z.not.null) {
    clmm.formula=as.formula(paste("y ~ offset(eta.est.rf) + ( 1+", paste(znam, collapse= "+"), " | group )"))
  }
  else {clmm.formula=as.formula(paste("y ~ offset(eta.est.rf) + ( 1 | group)"))}
  
  ##################################################
  ####	STEP 2: CLM per inizializzare mu_ij  #######
  ##################################################
  
  library(ordinal)
  library(ordinalForest)
  orf.data= cbind(y,cov[xnam])
  orf_init=ordfor(depvar = "y",perffunction = "probability", orf.data)
  preds=predict(orf_init, newdata=subset(orf.data, select = -y))
  levy=levels(y)
  preds.orf=preds$classprobs
  preds.orf.cum <- t(apply(preds.orf, 1, cumsum))
  colnames(preds.orf.cum) <- levy
  true_y <- as.character(y)
  preds.orf.cum <- preds.orf.cum[cbind(1:nrow(preds.orf.cum), match(true_y, levy))]
  preds.orf.cum <- round(preds.orf.cum,5)
  preds.orf.cum[preds.orf.cum==0]= 0.00001
  preds.orf.cum[preds.orf.cum==1]= 0.99999
  eta.est.orf = qlogis(preds.orf.cum)
  eta.est.rf = eta.est.orf
  
  ####################################################
  ######	STEP 3-4: Iterative model estimation  ######
  ####################################################
  library(randomForest)
  it=1
  converged=FALSE
  while(!converged && it<itmax) {
    
    
    clmm.data = data.frame(y,cov[znam],group,eta.est.rf)
    clmm.fit= clmm(clmm.formula, link='logit', data=clmm.data, Hess=TRUE, control=clmm.control(maxLineIter = 500, maxIter=1000, grtol=1e-3))
    
    #I want to maintain the order of the elements of bi
    select=c("(Intercept)",znam) #all bi to be extracted
    clmm.bi=ranef(clmm.fit)$group[select]
    
    bi.old=bi
    bi=data.frame(t(clmm.bi))
    
    #random forest
    target=rep(0,N) #target=eta-Z%*%b
    for (i in 1:N) {
      b.temp=as.matrix(bi[group[i]], nrow=q, ncol=1)
      z.temp=as.matrix(Zi.int[i,], nrow=1, ncol=q)
      target[i]= eta.est.orf[i] + z.temp%*%b.temp
    }
    forest.data=cbind(target, cov[xnam])
    forest=randomForest(forest.formula, forest.data, importance=TRUE) 
    eta.est.rf=forest$predicted
    
    #convergence of b_i
    names(bi)=lev
    diff.t=abs(bi.old-bi)
    n.diff=max(diff.t) #use the infinite norm (max)
    # ind=which(diff.t==n.diff, arr.ind=T)
    # n.old=abs(bi.old[ind])
    n.old=max(abs(bi.old))
    converged= n.diff/n.old <toll
    
    it=it+1
    all.bi[[it]]=bi
    
  }
  ###############################################
  ######	STEP 5: Output preparation  ###########
  ###############################################
  
  #if there is no convergence it gives an error message
  if(!converged) {
    warning('Maximum number of iterations exceeded, no convergence achieved')
  }
  
  result=list(clmm.fit,forest,target,bi,it,converged,all.bi,xnam)
  names(result)=c('clmm.model', 'forest.model','rf.target', 'rand.coef', 'n.iteration',
                  'converged','all.rand.coef','forest.var')
  class(result)='omerf'
  result
}


summary.omerf=function(om) {
  print('Mixed effects model') #summary of the mixed effects model
  print(summary(om$clmm.model)) 
  str=ifelse(om$converged, 'Converged', 'Did not converge')
  print(paste(str , 'after', om$n.iteration, 'iterations')) #says whether there is convergence
}



fitted.omerf=function(om, group, type='response') {
  #the correct fitted values are already those of the clmm function,
  #as they incorporate the fitted values of the random forest into the model
  mu_avg=om$clmm.model$fitted.values #for an avg random effect
  f.x_ij=om$forest.model$predicted
  y_train=as.numeric(om$clmm.model$y)
  
  ranef=rowSums(ranef(om$clmm.model)$gr)
  mu=rep(0,length(mu_avg))
  eta=rep(0,length(mu_avg))
  for (j in 1:length(mu_avg)) {
    i=group[j]
    c=y_train[j]-min(y_train)+1
    if(c==1) {
      eta[j]=om$clmm.model$Theta[c] - ranef[i] - f.x_ij[j]
      mu[j]=plogis(eta[j])
    } else if(c==max(y_train)-min(y_train)+1) {
      eta[j]=qlogis(0.99999999)
      mu[j]=1 - plogis(om$clmm.model$Theta[c-1] - ranef[i] - f.x_ij[j]) # 1 - P(yij<=c-1)
    } else {
      eta[j]=om$clmm.model$Theta[c] - ranef[i] - f.x_ij[j]
      mu[j]=plogis(eta[j]) - 
        plogis(om$clmm.model$Theta[c-1] - ranef[i] - f.x_ij[j]) # P(yij<=c) - P(yij<=c-1)
    }
  }
  #error if the answer type is not one of the following
  allowed=c('response', 'mu', 'eta')
  msg=paste('Possible choices are ', allowed[1],', ' ,allowed[2],' and ', 
            allowed[3], sep='')
  if(sum(type==allowed)==0) stop('Type of prediction not available:',msg)
  if(type=='mu') ans=mu # P(yij=c)
  if(type=='response') ans=as.factor(y_train)
  if(type=='eta') ans=eta
  ans
}



predict.omerf=function(om, y_train, newdata, group, type='response', 
                       predict.all=FALSE) {
  forest.data=newdata[om$forest.var]		
  f.x_ij=predict(om$forest.model,forest.data,predict.all=predict.all)
  
  preds <- as.data.frame(matrix(0, dim(newdata)[1], length(levels(y_train))))
  eta <- as.data.frame(matrix(0, dim(newdata)[1], length(levels(y_train))))
  ranef=rowSums(ranef(om$clmm.model)$group)
  lev=levels(y_train)
  names(preds) = lev
  names(eta) = lev
  for (c in 1:length(lev)) {
    for (j in 1:dim(newdata)[1]) {
      i=group[j]
      if(c==1) {
        eta[j,c]=om$clmm.model$Theta[c] - ranef[i] - f.x_ij[j]
        preds[j,c]=plogis(eta[j,c])
      } else if(c==length(lev)) {
        eta[j,c]=qlogis(0.99999999)
        preds[j,c]=1 - plogis(om$clmm.model$Theta[c-1] - ranef[i] - f.x_ij[j]) # 1 - P(yij<=c-1)
      } else {
        eta[j,c]=om$clmm.model$Theta[c] - ranef[i] - f.x_ij[j]
        preds[j,c]=plogis(eta[j,c]) - 
          plogis(om$clmm.model$Theta[c-1] - ranef[i] - f.x_ij[j]) # P(yij<=c) - P(yij<=c-1)
      }
    }
  }
  max_mu <- apply(preds, 1, max)
  pred_val <- names(preds)[apply(preds, 1, which.max)]
  
  
  #error if the answer type is not one of the following
  allowed=c('response', 'mu', 'eta')
  msg=paste('Possible choices are ', allowed[1],', ' ,allowed[2],' and ', 
            allowed[3], sep='')
  if(sum(type==allowed)==0) stop('Type of prediction not available:',msg)
  
  if(type=='eta') ans=eta # eta for the predicted class
  if(type=='mu') ans=preds # P(yij=c) c=1,...,C
  if(type=='response') ans=pred_val
  ans
}



ranef.omerf=function(om, group, znam=NULL) {
  ranef=ranef(om$clmm.model)$gr
  n = (length(znam)+1)*length(levels(group))
  step = length(levels(group))
  par(mfrow=c(1,length(znam)+1))
  for (i in 1:(length(znam)+1)) {
    condVar=rep(0,step)
    if(is.null(znam)) { condVar_temp = om$clmm.model$condVar
    } else { condVar_temp = om$clmm.model$condVar[((i - 1) * step + 1):(i * step),((i - 1) * step + 1):(i * step)] }
    for (l in 1:step) {
      if(is.null(znam)) { 
        condVar[l] <- condVar_temp[l]
      } else {condVar[l] <- condVar_temp[l,l]}
    }
    condVar_sqrt <- sqrt(condVar)
    ci <- ranef[,i] + qnorm(0.975) * condVar_sqrt %o% c(-1, 1)
    ord.re <- order(ranef[,i])
    lev <- levels(group)
    ci <- ci[order(ranef[,i]),]
    plot(1:length(lev), ranef[ord.re,i], axes=FALSE, ylim=range(ci),
         xlab="random variable", ylab="random effect")
    axis(1, at=1:length(lev), labels = lev[ord.re]) #,las=2
    axis(2)
    for(k in 1:length(lev)) segments(k, ci[k,1], k, ci[k, 2])
    abline(h = 0, lty=2)
  }
}


