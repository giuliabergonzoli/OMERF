library(ordinal)
library(randomForest)
library(ordinalForest)
library(pdfCluster)
library(psych)

# tables with results
results.acc=matrix(nrow=4, ncol=5, data=rep(0,20))
results.mse=matrix(nrow=4, ncol=5, data=rep(0,20))
results.oc=matrix(nrow=4, ncol=5, data=rep(0,20))
results.ari=matrix(nrow=4, ncol=5, data=rep(0,20))
results.ck=matrix(nrow=4, ncol=5, data=rep(0,20))
results.newi=matrix(nrow=4, ncol=5, data=rep(0,20))
#acc=accuracy, mse=MSE, ari=adjusted rand index, ck=cohen's kappa, oc=cardoso idx, newi=ballante idx
colnames(results.acc) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
colnames(results.mse) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
colnames(results.oc) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
colnames(results.ari) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
colnames(results.ck) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
colnames(results.newi) = c('clm', 'clmm', 'ordforest', 'omerf', 'omerf_clm')
rownames(results.newi)=c('mean', 'variance', 'max', 'min')
rownames(results.ck)=c('mean', 'variance', 'max', 'min')
rownames(results.ari)=c('mean', 'variance', 'max', 'min')
rownames(results.oc)=c('mean', 'variance', 'max', 'min')
rownames(results.mse)=c('mean', 'variance', 'max', 'min')
rownames(results.acc)=c('mean', 'variance', 'max', 'min')

# parameters for the loop
nruns=100
n= 1000#number of data to use (test+train)
prop=0.8 #proportion of data going into the train set

source('OMERF.R')
source('OMERF_clm.R')
source('build_dataset_no_ranef_new.R')
source('ord_class_index.R')
source('index.R')

acc.clm=rep(0,nruns)
mse.clm=acc.clm
oc.clm=acc.clm
ari.clm=acc.clm
ck.clm=acc.clm
newi.clm=acc.clm
acc.clmm=acc.clm
mse.clmm=acc.clm
oc.clmm=acc.clm
ari.clmm=acc.clm
ck.clmm=acc.clm
newi.clmm=acc.clm
acc.ordfor=acc.clm
mse.ordfor=acc.clm
oc.ordfor=acc.clm
ari.ordfor=acc.clm
ck.ordfor=acc.clm
newi.ordfor=acc.clm
acc.omerf=acc.clm
mse.omerf=acc.clm
oc.omerf=acc.clm
ari.omerf=acc.clm
ck.omerf=acc.clm
newi.omerf=acc.clm
acc.omerf_clm = acc.clm
mse.omerf_clm = acc.clm
oc.omerf_clm = acc.clm
ari.omerf_clm = acc.clm
ck.omerf_clm = acc.clm
newi.omerf_clm = acc.clm

for(nr in 1:nruns) {
  set.seed(nr)
  print(nr)
  # train set preparation
  dati=build.dataset(nr,prop)
  y=factor(dati$y.train)
  cov=dati$cov.train
  gr=factor(dati$group.train)
  cv2=NULL
  for(lv in 1:15) {
    dummy=NULL
    for(d in 1:length(gr)) dummy=c(dummy, ifelse(gr[d]==lv,1,0))
    cv2=cbind(cv2,dummy)
  }
  colnames(cv2)=paste0('d',1:15)
  covd = cbind(cov, cv2)
  
  # build all 4 models
  clm.data=data.frame(covd,y)
  clm.mod=clm(y ~ x1+x2+x3+x4+x5+x6+x7+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15 , data=clm.data, link='logit') #d1: base case
  for.data=data.frame(cov,y,gr)
  ordfor.mod=ordfor(depvar = 'y', perffunction = 'probability', for.data)
  clmm.mod=clmm(y ~ x1+x2+x3+x4+x5+x6+x7+(1|gr), link='logit', data=for.data, Hess=TRUE, control=clmm.control(maxLineIter = 500, maxIter=1000, grtol=1e-3))
  omerf.mod=omerf(y, cov, gr)
  omerf_clm.mod = omerf_clm(y, cov, gr)
  
  # test set preparation
  y.t=factor(dati$y.test)
  cov.t=dati$cov.test
  gr.t=factor(dati$group.test)
  cv2=NULL
  for(lv in 1:15) {
    dummy=NULL
    for(d in 1:length(gr.t)) dummy=c(dummy, ifelse(gr.t[d]==lv,1,0))
    cv2=cbind(cv2,dummy)
  }
  colnames(cv2)=paste0('d',1:15)
  cov.td=cbind(cov.t,cv2)
  test.data=data.frame(cov.td, gr.t)
  names(test.data)[dim(test.data)[2]]='gr'
  
  # calculate performance measures

  mu_clm_t=predict(clm.mod,test.data)$fit
  lev=levels(y)
  names(mu_clm_t) = lev

  y_clm_t <- as.numeric(names(mu_clm_t)[apply(mu_clm_t, 1, which.max)])
  y_test=as.numeric(y.t)
  acc.clm[nr]=sum(y_test==y_clm_t)/length(y_test)
  mse.clm[nr]=mean((y_test - y_clm_t)^2)
  cm.clm <- table(y_test, y_clm_t)
  num_classes_clm <- max(nrow(cm.clm), ncol(cm.clm))
  cm.clm2 <- matrix(0, nrow = num_classes_clm, ncol = num_classes_clm)
  rownames(cm.clm2) <- colnames(cm.clm2) <- lev
  cm.clm2[rownames(cm.clm), colnames(cm.clm)] <- cm.clm[rownames(cm.clm), colnames(cm.clm)]
  oc.clm[nr]=OrdinalClassificationIndex(cm.clm2, dim(cm.clm2)[1])
  ari.clm[nr]=adj.rand.index(y_test, as.numeric(y_clm_t))
  ck.clm[nr]=cohen.kappa(x=cbind(y_test,as.numeric(y_clm_t)))$kappa
  newi.clm[nr]=newindex(mu_clm_t,y_test,num_classes_clm)$norm.index

  mu_ord_t <- predict(ordfor.mod, newdata=test.data)$classprobs
  y_ord_t <- predict(ordfor.mod, newdata=test.data)$ypred
  acc.ordfor[nr]=sum(y_test==as.numeric(y_ord_t))/length(y_test)
  mse.ordfor[nr]=mean((y_test - as.numeric(y_ord_t))^2)
  cm.ordfor <- table(y_test, y_ord_t)
  num_classes_ordfor <- max(nrow(cm.ordfor), ncol(cm.ordfor))
  cm.ordfor2 <- matrix(0, nrow = num_classes_ordfor, ncol = num_classes_ordfor)
  rownames(cm.ordfor2) <- colnames(cm.ordfor2) <- lev
  cm.ordfor2[rownames(cm.ordfor), colnames(cm.ordfor)] <- cm.ordfor[rownames(cm.ordfor), colnames(cm.ordfor)]
  oc.ordfor[nr]=OrdinalClassificationIndex(cm.ordfor2, dim(cm.ordfor2)[1])
  ari.ordfor[nr]=adj.rand.index(y_test, as.numeric(y_ord_t))
  ck.ordfor[nr]=cohen.kappa(x=cbind(y_test,as.numeric(y_ord_t)))$kappa
  newi.ordfor[nr]=newindex(mu_ord_t,y_test,num_classes_ordfor)$norm.index

  mu_omerf_t =  predict.omerf(omerf.mod, y, test.data, test.data$gr, type='mu')
  y_omerf_t = predict.omerf(omerf.mod, y, test.data, test.data$gr, type='response')
  acc.omerf[nr]=sum(y_test==as.numeric(y_omerf_t))/length(y_test)
  mse.omerf[nr]=mean((y_test - as.numeric(y_omerf_t))^2)
  cm.omerf <- table(y_test, y_omerf_t)
  num_classes_omerf <- max(nrow(cm.omerf), ncol(cm.omerf))
  cm.omerf2 <- matrix(0, nrow = num_classes_omerf, ncol = num_classes_omerf)
  rownames(cm.omerf2) <- colnames(cm.omerf2) <- lev
  cm.omerf2[rownames(cm.omerf), colnames(cm.omerf)] <- cm.omerf[rownames(cm.omerf), colnames(cm.omerf)]
  oc.omerf[nr]=OrdinalClassificationIndex(cm.omerf2, dim(cm.omerf2)[1])
  ari.omerf[nr]=adj.rand.index(y_test, as.numeric(y_omerf_t))
  ck.omerf[nr]=cohen.kappa(x=cbind(y_test,as.numeric(y_omerf_t)))$kappa
  newi.omerf[nr]=newindex(mu_omerf_t,y_test,num_classes_omerf)$norm.index
  
  mu_omerf_clm_t =  predict.omerf_clm(omerf_clm.mod, y, test.data, test.data$gr, type='mu')
  y_omerf_clm_t = predict.omerf_clm(omerf_clm.mod, y, test.data, test.data$gr, type='response')
  acc.omerf_clm[nr] = sum(y_test==as.numeric(y_omerf_clm_t))/length(y_test)
  mse.omerf_clm[nr] = mean((y_test - as.numeric(y_omerf_clm_t))^2)
  cm.omerf_clm <- table(y_test, y_omerf_clm_t)
  num_classes_omerf_clm <- max(nrow(cm.omerf_clm), ncol(cm.omerf_clm))
  cm.omerf_clm2 <- matrix(0, nrow = num_classes_omerf_clm, ncol = num_classes_omerf_clm)
  rownames(cm.omerf_clm2) <- colnames(cm.omerf_clm2) <- lev
  cm.omerf_clm2[rownames(cm.omerf_clm), colnames(cm.omerf_clm)] <- cm.omerf_clm[rownames(cm.omerf_clm), colnames(cm.omerf_clm)]
  oc.omerf_clm[nr] = OrdinalClassificationIndex(cm.omerf_clm2, dim(cm.omerf_clm2)[1])
  ari.omerf_clm[nr] = adj.rand.index(y_test, as.numeric(y_omerf_clm_t))
  ck.omerf_clm[nr] = cohen.kappa(x=cbind(y_test,as.numeric(y_omerf_clm_t)))$kappa
  newi.omerf_clm[nr] = newindex(mu_omerf_clm_t,y_test,num_classes_omerf_clm)$norm.index
  
  mu_clmm_t=as.data.frame(matrix(0, dim(test.data)[1], length(lev)))
  eta_clmm_t=as.data.frame(matrix(0, dim(test.data)[1], length(lev)))
  names(mu_clmm_t) = lev
  names(eta_clmm_t) = lev
  for (c in lev) {
    for (j in 1:dim(test.data)[1]) {
      if (c==lev[1]) {
        eta_clmm_t[j,c]=as.numeric(clmm.mod$Theta[which(lev==c)]) - test.data$x1[j] * clmm.mod$beta[1] - test.data$x2[j] * clmm.mod$beta[2] - test.data$x3[j] * clmm.mod$beta[3] - test.data$x4[j] * clmm.mod$beta[4] - test.data$x5[j] * clmm.mod$beta[5] - test.data$x6[j] * clmm.mod$beta[6] - test.data$x7[j] * clmm.mod$beta[7] - clmm.mod$ranef[test.data$gr[j]]
        mu_clmm_t[j,c]=plogis(eta_clmm_t[j,c])
      } else if(c==lev[length(lev)]) {
        eta_clmm_t[j,c]=qlogis(0.999999)
        mu_clmm_t[j,c]=1 - plogis(as.numeric(clmm.mod$Theta[which(lev==c)-1]) - test.data$x1[j] * clmm.mod$beta[1] - test.data$x2[j] * clmm.mod$beta[2] - test.data$x3[j] * clmm.mod$beta[3] - test.data$x4[j] * clmm.mod$beta[4] - test.data$x5[j] * clmm.mod$beta[5] - test.data$x6[j] * clmm.mod$beta[6]- test.data$x7[j] * clmm.mod$beta[7] - clmm.mod$ranef[test.data$gr[j]])
      } else {
        eta_clmm_t[j,c]=as.numeric(clmm.mod$Theta[which(lev==c)]) - test.data$x1[j] * clmm.mod$beta[1] - test.data$x2[j] * clmm.mod$beta[2] - test.data$x3[j] * clmm.mod$beta[3] - test.data$x4[j] * clmm.mod$beta[4] - test.data$x5[j] * clmm.mod$beta[5] - test.data$x6[j] * clmm.mod$beta[6]- test.data$x7[j] * clmm.mod$beta[7] - clmm.mod$ranef[test.data$gr[j]]
        mu_clmm_t[j,c]=plogis(eta_clmm_t[j,c]) -
          plogis(as.numeric(clmm.mod$Theta[which(lev==c)-1]) - test.data$x1[j] * clmm.mod$beta[1] - test.data$x2[j] * clmm.mod$beta[2] - test.data$x3[j] * clmm.mod$beta[3] - test.data$x4[j] * clmm.mod$beta[4] - test.data$x5[j] * clmm.mod$beta[5] - test.data$x6[j] * clmm.mod$beta[6]- test.data$x7[j] * clmm.mod$beta[7] - clmm.mod$ranef[test.data$gr[j]])
      }
    }

  }

  y_clmm_t <- as.numeric(names(mu_clmm_t)[apply(mu_clmm_t, 1, which.max)])
  acc.clmm[nr]=sum(y_test==y_clmm_t)/length(y_test)
  mse.clmm[nr]=mean((y_test - y_clmm_t)^2)
  cm.clmm <- table(y_test, y_clmm_t)
  num_classes_clmm <- max(nrow(cm.clmm), ncol(cm.clmm))
  cm.clmm2 <- matrix(0, nrow = num_classes_clmm, ncol = num_classes_clmm)
  rownames(cm.clmm2) <- colnames(cm.clmm2) <- lev
  cm.clmm2[rownames(cm.clmm), colnames(cm.clmm)] <- cm.clmm[rownames(cm.clmm), colnames(cm.clmm)]
  oc.clmm[nr]=OrdinalClassificationIndex(cm.clmm2, dim(cm.clmm2)[1])
  ari.clmm[nr]=adj.rand.index(y_test, as.numeric(y_clmm_t))
  ck.clmm[nr]=cohen.kappa(x=cbind(y_test,as.numeric(y_clmm_t)))$kappa
  newi.clmm[nr]=newindex(mu_clmm_t,y_test,num_classes_clmm)$norm.index

}

# calculate the actual results 
results.acc[1,1]=mean(acc.clm)
results.acc[2,1]=var(acc.clm)*(nruns-1)/nruns
results.acc[3,1]=max(acc.clm)
results.acc[4,1]=min(acc.clm)

results.acc[1,2]=mean(acc.clmm)
results.acc[2,2]=var(acc.clmm)*(nruns-1)/nruns
results.acc[3,2]=max(acc.clmm)
results.acc[4,2]=min(acc.clmm)

results.acc[1,3]=mean(acc.ordfor)
results.acc[2,3]=var(acc.ordfor)*(nruns-1)/nruns
results.acc[3,3]=max(acc.ordfor)
results.acc[4,3]=min(acc.ordfor)

results.acc[1,4]=mean(acc.omerf)
results.acc[2,4]=var(acc.omerf)*(nruns-1)/nruns
results.acc[3,4]=max(acc.omerf)
results.acc[4,4]=min(acc.omerf)

results.mse[1,1]=mean(mse.clm)
results.mse[2,1]=var(mse.clm)*(nruns-1)/nruns
results.mse[3,1]=max(mse.clm)
results.mse[4,1]=min(mse.clm)

results.mse[1,2]=mean(mse.clmm)
results.mse[2,2]=var(mse.clmm)*(nruns-1)/nruns
results.mse[3,2]=max(mse.clmm)
results.mse[4,2]=min(mse.clmm)

results.mse[1,3]=mean(mse.ordfor)
results.mse[2,3]=var(mse.ordfor)*(nruns-1)/nruns
results.mse[3,3]=max(mse.ordfor)
results.mse[4,3]=min(mse.ordfor)

results.mse[1,4]=mean(mse.omerf)
results.mse[2,4]=var(mse.omerf)*(nruns-1)/nruns
results.mse[3,4]=max(mse.omerf)
results.mse[4,4]=min(mse.omerf)

results.oc[1,1]=mean(oc.clm)
results.oc[2,1]=var(oc.clm)*(nruns-1)/nruns
results.oc[3,1]=max(oc.clm)
results.oc[4,1]=min(oc.clm)

results.oc[1,2]=mean(oc.clmm)
results.oc[2,2]=var(oc.clmm)*(nruns-1)/nruns
results.oc[3,2]=max(oc.clmm)
results.oc[4,2]=min(oc.clmm)

results.oc[1,3]=mean(oc.ordfor)
results.oc[2,3]=var(oc.ordfor)*(nruns-1)/nruns
results.oc[3,3]=max(oc.ordfor)
results.oc[4,3]=min(oc.ordfor)

results.oc[1,4]=mean(oc.omerf)
results.oc[2,4]=var(oc.omerf)*(nruns-1)/nruns
results.oc[3,4]=max(oc.omerf)
results.oc[4,4]=min(oc.omerf)

results.ari[1,1]=mean(ari.clm)
results.ari[2,1]=var(ari.clm)*(nruns-1)/nruns
results.ari[3,1]=max(ari.clm)
results.ari[4,1]=min(ari.clm)

results.ari[1,2]=mean(ari.clmm)
results.ari[2,2]=var(ari.clmm)*(nruns-1)/nruns
results.ari[3,2]=max(ari.clmm)
results.ari[4,2]=min(ari.clmm)

results.ari[1,3]=mean(ari.ordfor)
results.ari[2,3]=var(ari.ordfor)*(nruns-1)/nruns
results.ari[3,3]=max(ari.ordfor)
results.ari[4,3]=min(ari.ordfor)

results.ari[1,4]=mean(ari.omerf)
results.ari[2,4]=var(ari.omerf)*(nruns-1)/nruns
results.ari[3,4]=max(ari.omerf)
results.ari[4,4]=min(ari.omerf)

results.ck[1,1]=mean(ck.clm)
results.ck[2,1]=var(ck.clm)*(nruns-1)/nruns
results.ck[3,1]=max(ck.clm)
results.ck[4,1]=min(ck.clm)

results.ck[1,2]=mean(ck.clmm)
results.ck[2,2]=var(ck.clmm)*(nruns-1)/nruns
results.ck[3,2]=max(ck.clmm)
results.ck[4,2]=min(ck.clmm)

results.ck[1,3]=mean(ck.ordfor)
results.ck[2,3]=var(ck.ordfor)*(nruns-1)/nruns
results.ck[3,3]=max(ck.ordfor)
results.ck[4,3]=min(ck.ordfor)

results.ck[1,4]=mean(ck.omerf)
results.ck[2,4]=var(ck.omerf)*(nruns-1)/nruns
results.ck[3,4]=max(ck.omerf)
results.ck[4,4]=min(ck.omerf)

results.newi[1,1]=mean(newi.clm)
results.newi[2,1]=var(newi.clm)*(nruns-1)/nruns
results.newi[3,1]=max(newi.clm)
results.newi[4,1]=min(newi.clm)

results.newi[1,2]=mean(newi.clmm)
results.newi[2,2]=var(newi.clmm)*(nruns-1)/nruns
results.newi[3,2]=max(newi.clmm)
results.newi[4,2]=min(newi.clmm)

results.newi[1,3]=mean(newi.ordfor)
results.newi[2,3]=var(newi.ordfor)*(nruns-1)/nruns
results.newi[3,3]=max(newi.ordfor)
results.newi[4,3]=min(newi.ordfor)

results.newi[1,4]=mean(newi.omerf)
results.newi[2,4]=var(newi.omerf)*(nruns-1)/nruns
results.newi[3,4]=max(newi.omerf)
results.newi[4,4]=min(newi.omerf)

results.acc[1,5] = mean(acc.omerf_clm)
results.acc[2,5] = var(acc.omerf_clm)*(nruns-1)/nruns
results.acc[3,5] = max(acc.omerf_clm)
results.acc[4,5] = min(acc.omerf_clm)

results.mse[1,5] = mean(mse.omerf_clm)
results.mse[2,5] = var(mse.omerf_clm)*(nruns-1)/nruns
results.mse[3,5] = max(mse.omerf_clm)
results.mse[4,5] = min(mse.omerf_clm)

results.oc[1,5] = mean(oc.omerf_clm)
results.oc[2,5] = var(oc.omerf_clm)*(nruns-1)/nruns
results.oc[3,5] = max(oc.omerf_clm)
results.oc[4,5] = min(oc.omerf_clm)

results.ari[1,5] = mean(ari.omerf_clm)
results.ari[2,5] = var(ari.omerf_clm)*(nruns-1)/nruns
results.ari[3,5] = max(ari.omerf_clm)
results.ari[4,5] = min(ari.omerf_clm)

results.ck[1,5] = mean(ck.omerf_clm)
results.ck[2,5] = var(ck.omerf_clm)*(nruns-1)/nruns
results.ck[3,5] = max(ck.omerf_clm)
results.ck[4,5] = min(ck.omerf_clm)

results.newi[1,5] = mean(newi.omerf_clm)
results.newi[2,5] = var(newi.omerf_clm)*(nruns-1)/nruns
results.newi[3,5] = max(newi.omerf_clm)
results.newi[4,5] = min(newi.omerf_clm)

# save the results
r=10
name.acc=paste('results_acc_',r,'.txt', sep='')
name.mse=paste('results_mse_',r,'.txt', sep='')
name.oc=paste('results_oc_',r,'.txt', sep='')
name.ari=paste('results_ari_',r,'.txt', sep='')
name.ck=paste('results_ck_',r,'.txt', sep='')
name.newi=paste('results_newi_',r,'.txt', sep='')
write.table(results.acc, file=name.acc)
write.table(results.mse, file=name.mse)
write.table(results.oc, file=name.oc)
write.table(results.ari, file=name.ari)
write.table(results.ck, file=name.ck)
write.table(results.newi, file=name.newi)
