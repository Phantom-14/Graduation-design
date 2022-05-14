set.seed(2021)
rm(list = ls())
data = read.csv("csv_all.csv")
data = data[,-1]
a.vec = c(0,1,1/2)

plot(density(data$PbB),main = "PbB")

data$PbB = log(data$PbB)
plot(density(data$PbB),main = "Log PbB")
dim(na.omit(data))


library(mice)

# Deterministic regression imputation via mice
imp <- mice(data, method = "norm.predict" , m = 1 )
# Store data
data_imp <- complete(imp)
# Multiple Imputation
imp <- mice(data, m = 5 )
#build predictive model
fit <- with(data = imp, lm(PbB ~.,data = data))
#combine results of all 5 models
combine <- pool(fit)
combine

# preProcess()
# library(caret)
# dt = preProcess(x = data,method= "knnImpute",k=2)
# 
# dim(dt$data)
# 
library(DMwR2)
# DMwR2::knnImputation()
# 
# data(algae)
# summary(algae)

k=7
cleandata <- knnImputation(data)

cdata = knnImputation(data)
summary(data)
summary(cleandata)
colnames(cleandata)[40]
fac.vec = c(1,2,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
            24,25,27,34,35,36,37,38,39,40,41)


for (i in c(1,2,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
            24,25,27,34,35,36,37,38,39,40,41)) {
  cleandata[,i] = as.factor(cleandata[,i])
}


dt = cleandata

dt.fac = dt[,fac.vec]
dt.num = dt[,-fac.vec]
dt.fac = cbind(cleandata$PbB,dt.fac)
colnames(dt.fac)[1] = "Log PbB"
colnames(dt.num)[1] = "Log PbB"
for (i in 1:ncol(dt.fac)) {
  dt.fac[,i] = as.numeric(as.character(dt.fac[,i]))
}

library(corrplot)


cor = cor(dt.fac)
corrplot(cor)

cor = cor(dt.num)
corrplot(cor)
colnames(dt.fac)[1] = "PbB"
colnames(dt.num)[1] = "PbB"

# Multiple Linear Regression

fit11 = lm(PbB ~ ., data = cdata)
summary(fit11)

# anova 分析 变量筛选


aov = aov(PbB ~ .,data = cdata)
summary(aov)

layout(matrix(c(1,2,3,4),2,2)) # 创建2行2列的画布
plot(aov) # 绘制析因设计结果的诊断图
layout(matrix(c(1),1,1))
aovdata = data[,c("PbB","age","lead_activities","tot_allot_yrs",
                  "dog_s","house_dusted","shrub_Pggkgbwday",
                  "shrub_HGgkgbwday","tuber_HGgkgbwday")]

#Reduced Model

fit12 <- lm(formula = PbB~age+lead_activities+tot_allot_yrs+
             dog_s+house_dusted+shrub_Pggkgbwday+
             shrub_HGgkgbwday+tuber_HGgkgbwday, data = cdata)
summary(fit12)

#Comparison of Regression Models

anova(fit12, fit11)

# lasso ridge and elastic net

x = as.matrix(aovdata[,-1])
y = as.matrix(aovdata[,1])

library(lars)

lar1<-lars(x,y,type="lasso")


library(glmnet)

# ridge
fit1 = glmnet(x, y,alpha = a.vec[1])
# print(fit1)
plot(fit1, xvar="lambda", label = TRUE)
# coef(fit1, s = 0.01)
y.pred1 = coef(fit1, s = 0.01)[1] + x %*% coef(fit1, s = 0.01)[-1]

# lasso
fit2 = glmnet(x, y,alpha = a.vec[2])
# print(fit2)
plot(fit2, xvar="lambda", label = TRUE)
# coef(fit2, s = 0.01)
y.pred2 = coef(fit2, s = 0.01)[1] + x %*% coef(fit2, s = 0.01)[-1]

# elastic net
fit3 = glmnet(x, y,alpha = a.vec[3])
# print(fit3)
plot(fit3, xvar="lambda", label = TRUE)
# coef(fit3, s = 0.01)
y.pred3 = coef(fit3, s = 0.01)[1] + x %*% coef(fit3, s = 0.01)[-1]

library(caret)
MAE(y.pred1,y)
MAE(y.pred2,y)
MAE(y.pred3,y)

RMSE(y.pred1,y)
RMSE(y.pred2,y)
RMSE(y.pred3,y)


R2(y.pred1,y)
R2(y.pred2,y)
R2(y.pred3,y)

# Gradient Descent
cvfit = cv.glmnet(x, y)
cvfit$lambda.min
plot(cvfit)

final_model = glmnet(x, y,lambda = cvfit$lambda.min,alpha = a.vec[1])
coef(final_model)

lambda = cvfit$lambda.min
alpha = a.vec[1]
lambda
alpha

lm = lm(formula = PbB~age+lead_activities+tot_allot_yrs+
          dog_s+house_dusted+shrub_Pggkgbwday+
          shrub_HGgkgbwday+tuber_HGgkgbwday,
        data = cdata)
library(car)
vif(lm)