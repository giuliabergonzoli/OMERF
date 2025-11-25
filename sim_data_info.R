library(ordinal)
library(randomForest)
library(ordinalForest)
library(pdfCluster)
library(psych)
source('build_dataset_lin.R')

nruns=100
n= 1000#number of data to use (test+train)
prop=0.8 #proportion of data going into the train set
n_levels=3
t_train = matrix(NA, nrow = nruns, ncol = n_levels)
t_test = matrix(NA, nrow = nruns, ncol = n_levels)

for(nr in 1:nruns) {
  set.seed(nr)
  dati=build.dataset(n,5,prop)
  t_train[nr,] = table(dati$y.train) # + table(dati$y.test)
  t_test[nr,] = table(dati$y.test)
}

stats_train <- data.frame(
  Min = apply(t_train, 2, min),
  Max = apply(t_train, 2, max),
  Media = apply(t_train, 2, mean),
  Varianza = apply(t_train, 2, var)
)

stats_test <- data.frame(
  Min = apply(t_test, 2, min),
  Max = apply(t_test, 2, max),
  Media = apply(t_test, 2, mean),
  Varianza = apply(t_test, 2, var)
)

print(stats_train)
print(stats_test)

r=12
name=paste('stats_train',r,'.txt', sep='')
write.table(stats_train, file=name)
name=paste('stats_test',r,'.txt', sep='')
write.table(stats_test, file=name)


