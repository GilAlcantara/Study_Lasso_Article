set.seed(1997)
pro = 0.2
ro = 0.5
p = 30
mi = rep(0,p)
sig = matrix(0,p,p)
for(k in 1:p){
  for(t in 1:p){
    sig[k,t] = ((-1)^{abs(k-t)}) * (ro^{abs(k-t)})
  }
}
n = c(100,1000)
for(h in 2:2){
  auc = vector()
  auc2 = vector()
  auc3 = vector()
  auc4 = vector()
  var1 = vector()
  var2 = vector()
  var4 = vector()
  l=0
  for(i in 1:500){
    X = matrix(ncol=p,nrow=n[h],0)
    X = mvrnorm(n[h],mi,sig)
    b0 = rep(0,24)
    b1 = rep(-0.5,3)
    b2 = rep(0.5,3)
    beta = c(0,b0,b1[1],b2[1],b1[2],b2[2],b1[3],b2[3])
    X = cbind(1,X)
    dados = as.data.frame(X)
    pi = exp(X%*%beta)/(1+exp(X%*%beta))
    Y = vector()
    for(z in 1:n[h]){
      Y[z] = rbinom(1,1,pi[z])
    }
    dados = cbind(dados,Y)
    sam_size = floor(0.7*nrow(dados))
    train_ind = sample(seq_len(nrow(dados)),size=sam_size)
    train = dados[train_ind,]
    test = dados[-train_ind,]
    
    d = model.matrix(train$Y~0+ .,data=train)
    
    lass = cv.glmnet(y=train$Y,x=d,alpha=1,family="binomial",
                     type.measure = "auc")
    
    lambda_opt = lass$lambda.min 
    lass = glmnet(y=train$Y,x=d,alpha=1,family="binomial",lambda = lambda_opt)
    d3 = model.matrix(test$Y ~ 0+.,data=test)
    predict3 = predict(lass, newx  = d3,s=lambda_opt,type="response")
    p3 = roc(predict3,factor(test$Y))
    auc3[i] = AUC::auc(p3)  
    
    #lasso+mv
    vec = coef(lass)
    no = vector()
    for(j in 2:nrow(vec)){
      if(vec[j]==0)no[j] = j 
    }
    no = na.omit(no)
    no = no-1
    train_1 = train[,-c(no,12), drop=FALSE]
    if(all(vec==0)) {train_1 =  train[,1, drop=FALSE]}
    if(length(no)==0){train_1 = train}
    if(ncol(train_1)==0) train_1 = train[,1, drop=FALSE]
    var1[i] = ncol(train_1)-1
    model1 = glm(train$Y ~ .,data=train_1,family = binomial(link = "logit")
                 ,control = list(maxit = 100))
    
    predict1 = predict(model1, newdata = test, type = 'response')
    p1 = roc(predict1,as.factor(test$Y))
    auc[i] = AUC::auc(p1)
    
    #lasso+aic
    
    model4 = glm(train$Y ~ .,data=train,family = binomial(link = "logit"),
                 control = list(maxit = 100))
    step = stepAIC(model4, direction = "both")
    
    #mv+aic
    model2 = glm(step$formula,data=train,family = binomial(link = "logit"),
                 control = list(maxit = 100))
    predict2 = predict(model2, test, type = 'response')
    p2 = roc(predict2,as.factor(test$Y))
    
    var2[i] = sum(as.vector(model2$coefficients)!=0)-1
    auc2[i] = AUC::auc(p2)
    
    dados$Y = NULL
  }
  print(round(2*mean(auc3)-1,5))
  print(round(2*sd(auc3),5))
  #
  print(round(2*mean(auc)-1,5))
  print(round(2*sd(auc),5))
  #
  print(round(2*mean(auc4)-1,5))
  print(round(2*sd(auc4),5))
  #
  print(round(2*mean(auc2)-1,5))
  print(round(2*sd(auc2),5))
  
  print("variancia")
  
  print(round(mean(var1),5))
  print(round(sd(var1),5))
  
  print(round(mean(var4),5))
  print(round(sd(var4),5))
  
  print(round(mean(var2),5))
  print(round(sd(var2),5))
}

