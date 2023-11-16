data=read.csv("/Users/guanmuhan/Desktop/gpa.csv")
head(data)
#Q2.(b)
dim(data)
y=data$GPA
x=cbind(1,data$HSM,data$HSS,data$HSE)
b=solve(t(x)%*%x,t(x)%*%y)
cat("estimated parameters:\n")
print(b)
#            [,1]
#[1,] -0.30174744
#[2,]  0.18826042
#[3,]  0.08457661
#[4,]  0.09377269
e=y-x%*%b
SSRes=sum(e^2)
s2=(SSRes/(100-4))
cat("estimated variance:", s2, "\n")
#estimated variance: 0.5390297 
#Q2.(c)
model1=lm(GPA~HSM+HSS+HSE,data = data)
summary(model1)

#Q2.(d)
e[15,]
#0.6139107
H=x%*%solve(t(x)%*%x)%*%t(x)

z_15th=e[15,]/(sqrt(s2*(1-H[15,15])))
cooks_distance_15th=(1/(3+1))*(z_15th^2)*(H[15,15]/(1-H[15,15]))
cat("leverage of the 15th observation:", H[15,15], "\n")
cat("standardised residual of the 15th observation:", z_15th, "\n")
cat("Cook's distance of the 15th observation:", cooks_distance_15th, "\n")

#> cat("leverage of the 15th observation:", H[15,15], "\n")
#leverage of the 15th observation: 0.01965834 
#> cat("standardised residual of the 15th observation:", z_15th, "\n")
#standardised residual of the 15th observation: 0.8445203 
#> cat("Cook's distance of the 15th observation:", cooks_distance_15th, "\n")
#Cook's distance of the 15th observation: 0.003575442 

#Q2.(e)
new_gpa = predict(
  model1, newdata = data.frame(HSM = 8, HSS = 9, HSE = 7))
new_gpa
#2.621934 

#Q2.(f)
# Matrix way
s=sqrt(s2)
xst=c(1,8,9,7)
y_hat=xst%*%b
ta=qt(0.995,df=100-4)
value1=y_hat-ta*s*sqrt(t(xst)%*%solve(t(x)%*%x)%*%xst)
value2=y_hat+ta*s*sqrt(t(xst)%*%solve(t(x)%*%x)%*%xst)
cat("99% CI:", sprintf("[%f, %f]", value1, value2), "\n")
#99% CI: [2.175524, 3.068345] 
#R function
interval_99_CI=predict(
  model1,
  newdata = data.frame(HSM = 8, HSS = 9, HSE = 7),
  interval = 'confidence',
  level = 0.99
)
interval_99_CI
#fit      lwr      upr
#1 2.621934 2.175524 3.068345
#Q2.(g)
# Matrix way
ta=qt(0.95,df=100-4)
value3=y_hat-ta*s*sqrt(1+t(xst)%*%solve(t(x)%*%x)%*%xst)
value4=y_hat+ta*s*sqrt(1+t(xst)%*%solve(t(x)%*%x)%*%xst)
cat("90% PI:", sprintf("[%f, %f]", value3, value4), "\n")
#90% PI: [1.370326, 3.873543] 
#R function
interval_90_PI=predict(
  model1,
  newdata = data.frame(HSM = 8, HSS = 9, HSE = 7),
  interval = 'prediction',
  level = 0.90
)
interval_90_PI
#fit      lwr      upr
#1 2.621934 1.370326 3.873543

#Q2.(h)
#1.F-test

x2=x[,-3]
Rg2=t(y)%*%x2%*%solve(t(x2)%*%x2)%*%t(x2)%*%y
SStotal=sum(y^2)
SSReg=SStotal-SSRes
Rg1g2=SSReg-Rg2
Fstat=(Rg1g2/1)/(SSRes/(100-4))
pf(Fstat,1,96,lower=F)
#0.3047348
#the p-value is 0.3047348 even lager than 0.1 ,we do not have enough evidence to reject H0 

#2.T-test
Rb=t(y)%*%x%*%b
RbHSS=t(y)%*%x[,-3]%*%solve(t(x[,-3])%*%x[,-3],t(x[,-3])%*%y)
Rb1_HSS=Rb-RbHSS
Fstat=Rb1_HSS/s2
pf(Fstat,3,96,lower.tail = F)
#0.3678409
sqrt(Fstat)
#1.03185
pt(sqrt(Fstat),96,lower.tail = F)*2
#0.3047348
#the p-value is 0.3047348 even lager than 0.1 ,we do not have enough evidence to reject H0 

#Q3.(a)
model2=lm(GPA~HSM+HSS+HSE+SATM+SATCR+SATW,data=data)
summary(model2)
#Q3.(b)
nullmodel=lm(GPA ~ 1, data=data)
anova(nullmodel, model2)

#The p-value is 2.813e-06 ,which is small enough.we reject the null hypothesis ,the new model  is relevant.
#Q3.(c)
finalmodel=step(model2, scope = ~ .)
#we select high school grades in Mathematic(HSM),high school grades in English(HSE) and SAT Mathematics(SATM) as variables of our final model.
#Q3.(d)
library(car)
linearHypothesis(finalmodel, matrix(c(0,1,0,-1),1,4), 0)
# p-value is small enough, so that we can reject H0, the parameters of SATM and HSM are not equal.