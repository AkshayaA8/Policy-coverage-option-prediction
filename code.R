#get the train data, datatable is my_data

plcy_data <- my_data[my_data$record_type==1,]
plcy_data <- plcy_data[,-c(1:7)]
str(plcy_data)

#converting independent variables to appropriate datatype
col_names <- colnames(plcy_data[,c(2,8)])
for(i in col_names){
  plcy_data[[i]] <- as.factor(plcy_data[[i]])
}
plcy_data$C_previous <- ordered(plcy_data$C_previous)

#converting dependent variables to appropriate datatype
col_names <- colnames(plcy_data[,c(11:17)])
for(i in col_names){
  plcy_data[[i]] <- ordered(plcy_data[[i]])
}

#sampling the data
plcy_data_sample = plcy_data[sample(nrow(plcy_data),20000),]

#checking for missing values
sum(is.na(plcy_data_sample$group_size))
sum(is.na(plcy_data_sample$homeowner))
sum(is.na(plcy_data_sample$car_age))
sum(is.na(plcy_data_sample$car_value))
sum(is.na(plcy_data_sample$risk_factor)) #7028 missing values
sum(is.na(plcy_data_sample$age_oldest))
sum(is.na(plcy_data_sample$age_youngest))
sum(is.na(plcy_data_sample$married_couple))
sum(is.na(plcy_data_sample$C_previous)) #175 missing values
sum(is.na(plcy_data_sample$duration_previous)) #175 missing values
sum(is.na(plcy_data_sample$cost))

#handling missing values
library(mice)
set.seed(1000)
plcy_data_mice = mice(plcy_data_sample,maxit = 3,m=3, method = 'pmm')
final_data = data.frame(complete(plcy_data_mice,1))
str(final_data)

#Removing age_oldest & age_youngest
final_data$age_diff = final_data$age_oldest - final_data$age_youngest
final_data <- final_data[,-c(6:7)]

#Grouping car value b,c & i to a and h
final_data$car_value[final_data$car_value=='b' | final_data$car_value=='c'] = 'a' 
final_data$car_value[final_data$car_value=='i'] = 'h' 
final_data$car_value <- factor(final_data$car_value,levels = c('a','d','e','f','g','h'))
table(final_data$car_value)

#checking for outliers
bp = boxplot(final_data$car_age)
summary(bp)
length(boxplot.stats(final_data$car_age)$out) #203 outliers
hist(final_data$car_age)
sort(bp$out,decreasing = TRUE)

#correlation 
library(corrplot)
library(RColorBrewer)
corr = cor(final_data[,c(1,3,8,17)])
corr_m = cor.mtest(final_data[,c(1,3,8,17)],conf.level = 0.95)
corrplot(corr, type='upper',method='number',p.mat = corr_m$p, col = brewer.pal(n = 8, name = "RdYlBu"))
View(corr)


#------------ORDINAL LOGISTIC REGRESSION to predict coverage options-----------
require(MASS)
require(caret)

#Product A
modelA <- polr(A ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelA)

ctable <- coef(summary(modelA))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(modelA))
confint.default(modelA)
exp(coef(modelA))
exp(cbind(OR = coef(modelA), ci))
confusionMatrix(predict(modelA, newdata = test_data), test_data$A, dnn = c("Predictions", "Actual Values"))

#Product B
modelB <- glm(B ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                married_couple + C_previous + duration_previous,family=binomial(link='logit'),data=train_data)
pred = predict(modelB, newdata=test_data)
accuracy <- table(pred, test_data$B)
sum(diag(accuracy))/sum(accuracy)

#Product C
modelC <- polr(C ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelC)
confusionMatrix(predict(modelC, newdata = test_data), test_data$C, dnn = c("Predictions", "Actual Values"))

#Product D
modelD <- polr(D ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelD)
confusionMatrix(predict(modelD, newdata = test_data), test_data$D, dnn = c("Predictions", "Actual Values"))

#Product E
modelE <- glm(E ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                married_couple + C_previous + duration_previous,family=binomial(link='logit'),data=train_data)
pred = predict(modelE, newdata=test_data)
accuracy <- table(pred, test_data$B)
sum(diag(accuracy))/sum(accuracy)

#Product F
modelF <- polr(F ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelF)
confusionMatrix(predict(modelF, newdata = test_data), test_data$F, dnn = c("Predictions", "Actual Values"))

#Product G
modelG <- polr(G ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelG)
confusionMatrix(predict(modelG, newdata = test_data), test_data$G, dnn = c("Predictions", "Actual Values"))

for (i in 1:nrow(train_data)){
  if (train_data$G[i] == 2){
    train_data$G_new[i] = 1
  } else {train_data$G_new[i] = train_data$G[i]}
}
summary(train_data$G_new)
for (i in 1:nrow(test_data)){
  if (test_data$G[i] == 2){
    test_data$G_new[i] = 1
  } else {test_data$G_new[i] = test_data$G[i]}
}
summary(test_data$G_new)
train_data$G_new = factor(train_data$G_new)

modelG_new <- polr(G_new ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous, data = train_data, Hess=TRUE)
summary(modelG_new)
confusionMatrix(predict(modelG_new, newdata = test_data), test_data$G, dnn = c("Predictions", "Actual Values"))



#---------------LINEAR MODEL to predict cost----------------------------
set.seed(1000)
index = sample(2,nrow(final_data),replace=TRUE,prob = c(0.7,0.3))
train_data = final_data[index==1,]
test_data = final_data[index==2,]

train_data$cost = log10(train_data$cost)
test_data$cost = log10(test_data$cost)

lm_model = lm(cost~.,data=train_data)
summary(lm_model)
no_model = lm(cost~1,data=train_data)
step(lm_model,scope = list(lower = no_model,upper = lm_model),direction = "backward")

lm_model_new = lm(cost ~ homeowner + car_age + car_value + risk_factor + 
                    married_couple + C_previous + duration_previous + A + 
                    C + D + E + F + G + age_diff, data = train_data)
summary(lm_model_new)
plot(lm_model_new)

pred_lm = predict(lm_model_new,test_data)
sqrt(mean(pred_lm-test_data$cost)^2)


#------------RANDOM FOREST to predict cost-------------------
library(randomForest)
rf_model = randomForest(cost ~ group_size + homeowner + car_age + car_value + risk_factor + 
                          married_couple + C_previous + duration_previous + A + 
                          C + E + F + G + age_diff, data = train_data, mtry = 10, ntree=100)
pred_rf = predict(rf_model,test_data)
sqrt(mean(pred_rf-test_data$cost)^2)

#extracting some decision rules
library(inTrees)
X <- train_data[,c(2:15,17)]
treeList = RF2List(rf_model)
exec <- extractRules(treeList, X)
ruleMetric <- getRuleMetric(exec,X,train_data$cost)
readableRules <- presentRules(ruleMetric, colnames(X))
