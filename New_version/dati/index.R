newindex<-function(pred_prob,y,m) {

  require(dplyr)
  require(ggplot2)
  require(pracma)
  
  n<-dim(pred_prob)[1]
  pred_prob<-unname(pred_prob) 
  ID<-1:n
  pred_class <- apply(pred_prob, 1, which.max)
  pred_class_fac <-as.factor(pred_class)
  ind<-cbind(ID,pred_class)
  prob_estclass<-pred_prob[ind]
  df<-data.frame(ID,prob_estclass,pred_class_fac,pred_class,y)
  ord_obs<- df %>% 
    arrange(pred_class_fac,desc(prob_estclass)) 
  y_real<-ord_obs$y
  x_graph<-seq(0,1,length.out=n+1)
  fun<-stepfun(x_graph,c(0,y_real,y_real[n]))
  y1=fun(x_graph)

  lenclass_ind_0<-c(0,which(diff(sort(pred_class))!=0))+1
  n_int<-length(lenclass_ind_0)
  lenclass_ind_0[n_int+1]<-n+1
  lenclass<-(lenclass_ind_0-1)/n
  lenclass[1]=0
  lenclass[n_int+1]<-1
  f.goal<-stepfun(x_graph,c(1,ord_obs$pred_class,m))
  y.goal=f.goal(x_graph)
  fun_mod<-data.frame(x_graph,y1)
  h<-as.numeric(c(1,levels(pred_class_fac)))
  class_point<-data.frame(lenclass,h)
  graph<-ggplot(fun_mod,aes(x=x_graph,y=y1))+ geom_step()+geom_point(class_point,mapping=aes(x=lenclass,y=h))+xlab("observations")+ylab("real class")
  fun_goal<-data.frame(x_graph,y.goal)
  graph_goal<-ggplot(fun_goal,aes(x=x_graph,y=y.goal))+ geom_step()+geom_point(class_point,mapping=aes(x=lenclass,y=h))+xlab("observations")+ylab("real class")


  fun_int<-function (x) abs(fun(x)-f.goal(x))
  int<-rep(NA,n_int)
  w<-rep(NA,n_int)
  for (i in 1:n_int) {
    int[i]<-integral(fun_int,lenclass[i],lenclass[i+1])
    last_class<-lenclass_ind_0[i+1]-lenclass_ind_0[i]
      error_indeces<-which(y_real[lenclass_ind_0[i]:(lenclass_ind_0[i+1]-1)]!=ord_obs$pred_class[lenclass_ind_0[i]:(lenclass_ind_0[i+1]-1)])
  if(length(error_indeces)>0){
    err<-(last_class-min(error_indeces)+1)
  } else {
    err<-lenclass_ind_0[i+1]-lenclass_ind_0[i]
  }

    w[i]<-(err/n)/(lenclass[i+1]-lenclass[i])
  }
  k<-rep(NA,n_int)
  for (i in 1:n_int){
   k[i]<-(lenclass[i+1]-lenclass[i])*max(m-i,i-1)
  }
  index<-sum(int*w)
  K<-sum(k)
  norm_index<-index/K
  obj<-list("index"=index, "norm.index"=norm_index, "graph"=graph)
  return(obj)
}

