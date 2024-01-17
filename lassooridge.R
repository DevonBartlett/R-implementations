require(ISLR)
data('College')
set.seed(1)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(College))
train_ind <- sample(seq_len(nrow(College)), size = smp_size)

## Creating the sets.
train <- College[train_ind, ]
test <- College[-train_ind, ]

## Linear Model with Training
model=lm(Apps ~ ., data=train)
summary(model)
## Fitting to Test set
pred <- predict(model, test)

## Calculating MSE
MSE=mean((test$Apps-pred)^2)
print(MSE)


## Setting x and y values for training and test. 
xtrain=model.matrix (Apps~.,train)[,-1]
ytrain=train$Apps
xtest=model.matrix (Apps~.,test)[,-1]
ytest=test$Apps
## Cross-Validation, looking at lambda
set.seed (1)
cv.out=cv.glmnet (xtrain,ytrain,alpha =0)
plot(cv.out)

## Finding the best lambda value
best=cv.out$lambda.min
## Creating training model using ridge regression
model2=glmnet(xtrain,ytrain,alpha=0,lambda=best2)
model2$beta

## Fitting training model on test set
pred=predict(model2,s=best2 ,newx=xtest)
## Calculating MSE
MSE2=mean((pred-ytest)^2)
print(MSE2)



#Same as before
set.seed (1)
cv.out=cv.glmnet (xtrain,ytrain,alpha =1)
plot(cv.out)

best3=cv.out$lambda.min
#Creating training model using lasso regression where alpha is 1
model3=glmnet(xtrain,ytrain,alpha=1,lambda=best3)
model3$beta

#Fitting trainning model on test set
pred=predict(model3,s=best3 ,newx=xtest)
#Calculating Accuracy
MSE3=mean((pred-ytest)^2)
#Printing MSE
print(MSE3)

## Lasso coefficients
lasscoef=predict(model3,type="coefficients",s=bestlam)[1:length(model$beta),]
## Nonzero Coefficients
lasscoef[lasscoef!=0]


mse.models <- data.frame(
  lasso.mse <- MSE3,
  ridge.mse <- MSE2,
  lm.mse <- MSE
)

mse.models
