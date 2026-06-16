.libPaths(c(.libPaths(), "/scratch/11175031/packages"))
require(MASS)
require(AUC)
require(InformationValue)
require(glmnet)
require(iterators)
require(gtable)
require(foreach)
require(rlang)
require(scales)
dados <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data"),
                     header=FALSE)
dados = na.omit(dados)
Y = vector()
for(i in 1:nrow(dados)){
  if(dados$V35[i] =="g") {Y[i]=0
  }else{Y[i]=1}
}
dados = dados[,-35]
dados = cbind(dados,Y)
sam_size = floor(0.7*nrow(dados))
auc = vector()
auc2 = vector()
auc3 = vector()
auc4 = vector()
var1 = 0
var2 = 0
var4 = 0
las = 0
las_mv = 0
las_aic = 0
mv_aic = 0
for(i in 1:100){
  train_ind = sample(seq_len(nrow(dados)),size=sam_size)
  train = dados[train_ind,]
  test = dados[-train_ind,]
  
  #lasso
  d = model.matrix(train$Y ~ 0+.,data=train)
  
  lass = cv.glmnet(y=train$Y,x=d,alpha=1,family="binomial",
                   type.measure = "auc")
  lambda_opt = lass$lambda.min 
  lass = glmnet(y=train$Y,x=d,alpha=1,family="binomial",lambda = lambda_opt)
  d3 = model.matrix(test$Y ~ 0+.,data=test,s=lambda_opt,type="response")
  predict3 = predict(lass, newx  = d3)
  p3 = roc(predict3,as.factor(test$Y))
  auc3[i] = AUC::auc(p3)
  
  #lass+mv
  vec = coef(lass)
  
  no = vector()
  for(j in 2:nrow(vec)){
    if(vec[j]==0)no[j] = j 
  }
  no = na.omit(no)
  no = no-1
  train_1 = train[,-no]
  if(ncol(train_1)==0) train_1 = train
  var1 = ncol(train_1)+var1-1
  model1 = glm(train$Y ~ .,data=train_1,family = binomial(link = "logit")
               ,control = list(maxit = 100))
  
  
  predict1 = predict(model1, newdata = test, type = 'response')
   p1 = roc(predict1,as.factor(test$Y))
  auc[i] = AUC::auc(p1)
  
  #lasso+aic
  
  model4 = glm(train$Y ~ .,data=train,family = binomial(link = "logit")
               ,control = list(maxit = 100))
  step = stepAIC(model4)
  d2 = model.matrix(step$formula,data=train)
  d2 = d2[,-1]
  lass = cv.glmnet(y=train$Y,x=d2,alpha=1,family="binomial",
                   type.measure = "auc")
  lambda_opt = lass$lambda.min 
  lass = glmnet(y=train$Y,x=d2,alpha=1,family="binomial",lambda = lambda_opt)
  var4 = ncol(d2)+var4
  d4 = as.matrix(subset(test,select = c(colnames(d2))))
  predict4 = predict(lass, newx  = d4,s=lambda_opt,type="response")
  p4 = roc(predict4,as.factor(test$Y))
  auc4[i] = AUC::auc(p4)
  
  #mv+aic
  model2 = glm(train$Y ~ .,data=train,family = binomial(link = "logit")
               ,control = list(maxit = 100))
  
  step = stepAIC(model2)
  
  model2 = glm(step$formula,data=train,family = binomial(link = "logit"),
               control = list(maxit = 100))
  predict2 = predict(model2, test, type = 'response')
  p2 = roc(predict2,as.factor(test$Y))
  var2 = length(as.vector(model2$coefficients))+var2-1
  auc2[i] = AUC::auc(p2)
  if((auc[i]>auc2[i])&&(auc[i]>auc3[i])&&auc[i]>auc4[i])las_mv=las_mv+1
  if((auc2[i]>auc[i])&&(auc2[i]>auc3[i])&&auc2[i]>auc4[i])mv_aic=mv_aic+1
  if((auc3[i]>auc[i])&&(auc3[i]>auc2[i])&&auc3[i]>auc4[i])las=las+1
  if((auc4[i]>auc[i])&&(auc4[i]>auc2[i])&&auc4[i]>auc3[i])las_aic=las_aic+1
}
var1/100
mean(auc3)
sd(auc3)
las
#
var1/100
mean(auc)
sd(auc)
las_mv
#
var4/100
mean(auc4)
sd(auc4)
las_aic
#
var2/100
mean(auc2)
sd(auc2)
mv_aic
