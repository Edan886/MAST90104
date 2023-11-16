library(gmodels)
#Q1
#1

judge=function(n){
  for (i in 2:ceiling(sqrt(n))){
    if ((n%%i)==0){return(T)}
    else{return(F)}
  }
}
for (j in 523:731){}
#2

q=c()
#Q2
#1
data("swiss")

model=lm( Fertility~.,data=swiss)
model$coefficients
summary(model)
Ress=7.165^2
#2
stepmodel=step(model)
#3
anova(stepmodel,model,test="F")
#4
par(mfrow=c(2,2))
plot(stepmodel)
#Q3
#1,
data=data.frame(counts=c(574,378,282,207,207,237,176,108,66,46),
           time=c(5.00,7.00,8.00,10.00,11.00,4.52,6.03,7.53,9.55,11.57),
           sample=c(1,1,1,1,1,2,2,2,2,2))
data$sample=as.factor(data$sample)
model2=glm(counts~time*sample,data=data,family = poisson(link = "log"))
summary(model2)
#2
cooks.distance(model2)
#3
model2=glm(counts~time+I(time^2)+sample,data=data,family = poisson(link = "log"))
summary(model2)
#4
newdata=data.frame(time=7,sample=as.factor(2))
predict(model2,newdata,type = 'response')
