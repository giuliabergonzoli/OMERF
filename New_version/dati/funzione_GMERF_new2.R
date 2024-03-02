
## SOLO PER UNA BINARY RESPONSE

gmerf_new_init= function (y, cov, group, xnam=NULL, znam=NULL, family='binomial', bizero=NULL, 
	itmax=30, toll=0.02) {
#argomenti: 
#-y=vettore con le risposte
#-cov=data frame con le covariate di ogni unit? statistica
#-group=vettore factor che, per ogni unit? statistica, dice il gruppo a cui appartiene
#-xnam=vettore coi nomi delle covariate da usare nella random forest
#-znam=vettore coi nomi delle covariate da usare nei random effects
#-bizero=matrice in cui ogni colonna contiene i coefficienti dei random effects
#	   il primo valore di ogni colonna ? l'intercept, gli altri le covariate znam

#assumo che group e bizero siano coerenti, cio? b[,i] corrisponda a levels(group)[i]


	######################################
	####	STEP 1: Inizializzazione  ######
	######################################
  if(family=='binomial'){
    y = as.factor(y)
    } 
	N <- length(y) #numero di osservazioni
	n=length(levels(group)) #numero di gruppi
	q <- length(znam)+1	# numero di covariate + random intercept
	Zi=NULL
	z.not.null=!(is.null(znam)) #controllo se ci sono covariate incluse nei random effects
	if (z.not.null) Zi=cov[znam] #covariate dei random effects
	Zi.int= cbind(rep(1,N),Zi) #random intercept + covariate dei random effects

	#Inizializzo (se ? NULL) bi a 0
	if( is.null(bizero) ){
		bi <- NULL
		for(i in 1:n) bi=cbind(bi,rep(0,q))
	}	
	if( !is.null(bizero) ) bi=bizero
	lev=levels(group) #nomi dei gruppi
	bi=data.frame(bi)
	names(bi)=lev
	all.bi=list()  #i b_i di ogni iterazione
	all.bi[[1]]=bi

	#se xnam ? NULL assumo che tutte le variabili di cov siano da usare
	if(is.null(xnam)) xnam=names(cov)

	#group deve essere un factor, altrimenti da errore
	if(!is.factor(group)) stop('Argomento "group" deve essere un factor')

	if(z.not.null){
		glmer.formula=as.formula(paste("y ~ ( 1+", paste(znam, collapse= "+"), " | group )"))
	}
	if(!z.not.null){glmer.formula=as.formula(paste("y ~ ( 1 | group)"))}
	forest.formula=as.formula(paste("target ~ ", paste(xnam, collapse= "+")))

	
	##################################################
	####	STEP 2: GLM per inizializzare mu_ij  #######
	##################################################

  ## old initialization with glm
	glm.formula=as.formula(paste("y ~ ", paste(xnam, collapse= "+")))
	glm.data= cbind(y,cov[xnam])
	glm.0=glm(glm.formula, data=glm.data, family=family)
	#mu.ij.0=glm.0$fitted.values
	#eta.est1=glm.0$family$linkfun(mu.ij.0) #stime del glm
	linkf=glm.0$family$linkfun
	linkinv=glm.0$family$linkinv
	
	## new initialization with rf --> Chiara
	rf_init <- randomForest(glm.formula, data=glm.data)
	pred.rf = predict(rf_init, type='prob')[,2]
	pred.rf[pred.rf==0]= 0.00001
	pred.rf[pred.rf==1]= 0.99999
	eta.est.rf = log(pred.rf/(1-pred.rf))
	
	####################################################
	####	STEP 3-4: Stima iterativa del modello  #######
	####################################################
	
	library(randomForest)
	library(lme4)
	it=1
	converged=FALSE
	while(!converged && it<itmax) {

	  #glm con mixed effects
	  #glmer.data=cbind(y,group)
	  #if(z.not.null) glmer.data=cbind(glmer.data, Zi)
	  #glmer.data=data.frame(glmer.data) #altrimenti glmer non funziona
	  glmer.data = data.frame(y,group)
	  if(z.not.null) glmer.data=data.frame(glmer.data, Zi)
	  glmer.fit= glmer(glmer.formula, glmer.data, family=family, offset=eta.est.rf)
	  
	  #voglio mantenere l'ordine degli elementi dei b_i
	  select=c("(Intercept)",znam) #tutti i b_i da estrarre
	  glmer.bi=ranef(glmer.fit)$group[select]
	  
	  bi.old=bi
	  bi=data.frame(t(glmer.bi))
	  
		#random forest
		target=rep(0,N) #target=eta-Z%*%b
		for (i in 1:N) {
			b.temp=as.matrix(bi[group[i]], nrow=q, ncol=1)
			z.temp=as.matrix(Zi.int[i,], nrow=1, ncol=q)
			target[i]= eta.est.rf[i] - z.temp%*%b.temp   ## qui ho messo eta.est.rf invece che eta.est ma il logit va bene per la RF?
		}
		forest.data=cbind(target, cov[xnam])
		forest=randomForest(forest.formula, forest.data) 
		eta.est.rf=forest$predicted

		

		
		
		#convergenza dei b_i
		
		names(bi)=lev
		diff.t=abs(bi.old-bi)
		n.diff=max(diff.t) #uso la norma infinito(max)
		ind=which(diff.t==n.diff, arr.ind=T)
		n.old=abs(bi.old[ind])
		converged= n.diff/n.old <toll
		it=it+1
		all.bi[[it]]=bi
	}

	###############################################
	####	STEP 5: Preparazione output  ############
	###############################################

	#se non ho convergenza do un messaggio di errore
	if(!converged) {
		warning('Numero massimo di iterazioni superato, non si ? arrivati a convergenza')
	}

	result=list(glmer.fit,forest,bi,it,converged,all.bi,linkf,linkinv,xnam,znam,family)
	names(result)=c('glmer.model', 'forest.model', 'rand.coef', 'n.iteration',
				'converged','all.rand.coef','linkf','linkinv','forest.var',
				'random.eff.var','family')
	class(result)='gmerf'
	result
}



summary.gmerf=function(gm) {
	print('Mixed effects model') #summary del mixxed effects model
	print(summary(gm$glmer.model)) 
	str=ifelse(gm$converged, 'Converged', 'Did not converge')
	print(paste(str , 'after', gm$n.iteration, 'iterations')) #dice se c'? convergenza
}



fitted.gmerf=function(gm, type='response', alpha=0.5) {
#i fitted values corretti sono gi? quelli della funzione glmer, poich?
#ho incorporato i fitted values della random forest nel modello
	mu=fitted(gm$glmer.model)
	#errore se il tipo di risposta non ? tra questi
	allowed=c('response', 'mu', 'eta')
	msg=paste('Possible choices are ', allowed[1],', ' ,allowed[2],' and ', 
			allowed[3], sep='')
	if(sum(type==allowed)==0) stop('Type of prediction not available:',msg)
	if(type=='mu') ans=mu
	if(gm$family=='binomial' && type=='response') ans=mu>alpha
	if(type=='eta') ans=gm$linkf(mu)
	ans
}



predict.gmerf=function(gm, newdata, group, type='response', alpha=0.5, 
				predict.all=FALSE, re.form=NULL, newparam=NULL, 
				terms=NULL, allow.new.levels=TRUE, na.action=na.pass,
				random.only=FALSE) {

#chiamo semplicemente i metodi predict di gmerf e randomForest
	forest.data=newdata[gm$forest.var]
	glmer.data=data.frame(newdata[gm$random.eff.var],group)
	p1=predict(gm$glmer.model,newdata=glmer.data,newparam=newparam,re.form=re.form,
			random.only=random.only, type='link', na.action=na.action,
			allow.new.levels=allow.new.levels)			
	p2=predict(gm$forest.model,forest.data,predict.all=predict.all)
	
	#errore se il tipo di risposta non ? tra questi
	allowed=c('response', 'mu', 'eta')
	msg=paste('Possible choices are ', allowed[1],', ' ,allowed[2],' and ', 
			allowed[3], sep='')
	if(sum(type==allowed)==0) stop('Type of prediction not available:',msg)
	
	eta=p1+p2 
	mu=gm$linkinv(eta)
	if(type=='eta') ans=eta #eta:predico eta
	if(type=='mu') ans=mu #predico la probabilit?
	if(gm$family=='binomial' && type=='response') ans=mu>alpha #predico la risposta basandomi su alpha
	ans
}


