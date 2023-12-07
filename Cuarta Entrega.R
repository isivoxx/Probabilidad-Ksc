### !!! http://pastebin.com
### !!! https://justpaste.it/

##########
### 1) Correlation analysis
### 2a) Regression analysis - linear regression
### 2b) RA - Linearised nonlinear models
### 2c) Multivariate linear regression
### 2d) Regression analysis - polynomial model
###     Quadratic polynomial
### 2e) See 5 sposobov vykreslania aproximacii in malaga_2020_Pisomka_Statistics_2.R
###     MATRIX EQUATION
### 3) Plots in corr. and reg. analysis
### 4) Nonlinear and other types of regression
###    - Gram-Schmidt orthogonalisation
###    - Nonlinear nls(...)
###    - Nonlinear optim(...)
###    - Generalized Linear Models - glm(...)
###    - Logistic growth
###    - MCMC regression
##########

##########
### 1) Correlation analysis
##########
x = c(1, 1.5, 2, 2, 2.5); x
y = c(3, 5,   6, 6, 5); y
y = c(3, 5,   6, 6, 7)
cor(x, y)
plot(x,y)
ca=cor.test(x, y); ca
attributes(ca)
ca$estimate; ca$p.value


plot(y ~ x);     ##  <=> plot(x, y);
ra=lm(y ~ x); ra;
lines(x, ra$fit, col='blue', lwd=2)  ### <=>:
abline( ra, col = "red" )

##########
### 2a) Regression analysis linear model
##########
x = c(1, 1.5, 2, 2, 2.5)
y = c(3, 5,   6, 6, 7)

ra = lm(y~x); ra
su=summary(ra); su
str(ra) # structure
attributes( ra )

su$coefficients[2,4]

ra$coef           # <=> coef( ra );      
ra$fitted.values  # <=> fitted( ra)      # 6 possibilities
ra$residuals      # <=> residuals( ra )
y-ra$fitted.values
anova(ra)         # !!!!!

### Plots
plot(y ~ x);     ##  <=> plot(x, y);
abline( ra, col = "red" )
lines(x, ra$fit, col='blue', lwd=2)  ### <=>:
points(x,ra$fit,col='blue')

##### !!!!! 
##### 6 solutions for p-value
##### 1-3 [t-test], 4-5[F-test], 6[cor-test]
### 1)
summary(ra)$coefficients[,4][2]   ### p_value_for_b <=>
### 2)
coef(summary(ra))[, "Pr(>|t|)"][2]              ### <=>
### 3)
summary(ra)[[4]][[8]]
### 4)
anova(ra)$'Pr(>F)'[1]
### 5)
pVal <- function (answ) {
  if (class(answ) != "lm") stop("Not an object of class 'lm' ")
  f = summary(answ)$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) = NULL
  return(p)
}
pVal(ra)
### 6)
cor.test(x,y)$p.value

#
# Check wether the line passes through the origo (0,0):
#
xodh = -1:3; xodh
yodh = ra$coef[1] + ra$coef[2]*xodh; yodh

plot(xodh,yodh,col='green')
points(x,y)
abline( lm(y ~ x), col = "red" )
points(0,0, cex=3, col="red") # not

points(0,ra$coefficients[1], cex=2, col="blue") # not

ra2 = lm(y ~ 0 + x)
yodh = ra2$coef[1]*xodh
lines(xodh, yodh, col='blue')
points(0,0, cex=2, col="blue") # OK


##########
### 2b) RA - Linearised nonlinear models
###          Linearizovatelne nelinearne modely
##########
x = c(  45.3,    96.5,   164.8,   260.6,  429.1,   596.6,   775.1)
y = c(8886.09, 8643.75, 8340.46, 8126.6, 7486.71, 7370.64, 7258.11)
plot(y~x)

plot(y~x)
z1=exp(-x); v1 = lm(y~z1);v1; z1 #!!??  # not good
z2=1/x;     v2 = lm(y~z2)        
z3=log(x);  v3 = lm(y~z3); 

summary(v1)  # not good
summary(v2)  # good
summary(v3)  # better

summary(v1)$coefficients[2,4]
summary(v2)$coefficients[2,4]
summary(v3)$coefficients[2,4]


# Polygons:
lines(x, v1$fit)               # not good
lines(x, v2$fit,col="green")   # good
lines(x, v3$fit,col="magenta") # better
                 

### Better lines:
xx=seq(min(x),max(x),len=101); xx
lines(xx,v1$coef[1]+v1$coef[2]*exp(-xx),col="gold")   # not good
lines(xx,v2$coef[1]+v2$coef[2]/xx,col="green")        # good
lines(xx,v3$coef[1]+v3$coef[2]*log(xx),col="magenta") # better

# CONTINUED - quadratic model
# Below more detailed 

v4a = lm(y ~ 1 + x + I(x^2)); 
lines(x, v4a$fit, col="red")   # best
#  <~~>
v4b = lm( y ~ poly(x,2) ); #v3b
lines(x, v4b$fit, col="blue")  

summary(v4a)  # best
summary(v4b)  # - <->
# However:
v4a
# <>!=
v4b # orthogonal polynomials

### Residual sum of squares - numerical criterion
### smaller and smaller values:
rs1=sum((v1$resid)^2);    formatC(rs1, format = "e", digits = 2)
rs2=sum((v2$resid)^2);    formatC(rs2, format = "e", digits = 2)
rs3=sum((v3$resid)^2);    formatC(rs3, format = "e", digits = 2)  # better
rs4a=sum((v4a$resid)^2);  formatC(rs4a, format = "e", digits = 2) # best
rs4b=sum((v4b$resid)^2);  formatC(rs4b, format = "e", digits = 2)


##########
### 2c) Multivariate linear regression
##########
### Temperatures in time t
t = c(  7,	9,	11,	13,	15,	17,	19,	21,	23)
o = c(	8.2,	10.7,	18.9,	26.7,	24.6,	13,	11.4,	10.6,	10.2)
r = c(	38.4,	38.5,	35.8,	35.8,	37,	38.4,	38.2,	37.6,	38.1)
s = c(	13.5,	13.6,	15.3,	17.2,	18.1,	17.9,	16.7,	15.8,	15.4)
i = c(	15.6,	16,	17.7,	19.7,	20.4,	19.7,	18.6,	17.9,	17.4)

df = data.frame(t=t,o=o,r=r,s=s,i=i); df
# See below further correlation plots:
cor(df)


# => we neglect t
reg = lm(i ~ o + r + s)
summary(reg)

reg2 = lm(i ~ o + s)
summary(reg2)

#=============

### MS in Excel
x1 = c(2310,2333,2356,2379,2402,2425,2448,2471,2494,2517,2540)
x2 = c(2,2,3,3,2,4,2,2,3,4,2)
x3 = c(2,2,1.5,2,3,2,1.5,2,3,4,3)
x4 = c(20,12,33,43,53,23,99,34,23,55,22)
y = c(142000,144000,151000,150000,139000,169000,126000,142900,163000,169000,149000)
r2 = lm(y ~ 1 + x1 + x2 + x3 + x4); r2$coeff
summary(r2)


##########
### 2d) Regression analysis - polynomial model 
###     5 sposobov
###     MATRIX EQUATION
##########

###
###     Quadratic polynomial
###
x=c(4.6, 4.6, 4.9, 4.9, 5.2, 5.2)
y=c(2.717,2.708,2.684,2.683,2.822,2.800)

vys2 <- lm(y ~ 1 + x + I(x^2))    
## <> lm( y ~ poly(x,2) ) # - orthogonal
plot(x,y)
lines(x, vys2$fit)
# <=>:
par = function(x, c) {
  c[1] + c[2]*x + c[3]*x^2 
}
xx = seq(min(x), max(x), len=101); xx  ## FromToBy
lines(xx, par(xx, vys2$coef), col = "blue" )

### 2e) See 5 sposobov vykreslania aproximacii 
###        in malaga_2020_Pisomka_Statistics_2.R
###        from C:\Csaba\__________Vyuka\___________Vyuka_2020_ZS\ApPrSt_vycuc\Pisomky\Pisomka_2
# A) 5 sposobov vykreslania aproximacii
#    - abline(ra)
#    - ra$coeff
#    - predict(ra,xdf)
# B) - ggplot
#    - stat_smooth

library(ggplot2)
dfx =data.frame(x=x, y=y);                  #dfx
dfxx=data.frame(x=xx,y=par(xx,vys2$coef));  #dfxx
ggplot(dfxx, aes(x=x, y=y)) + geom_line(color="blue") +
  geom_point(mapping = aes(x=x,y=y), data=dfx, color="red")

#ggplot(dfxx, aes(x = x, y = y)) + geom_line() +
#  geom_point(mapping = aes(x=x,y=y), data=data.frame(x=x,y=y))

###############

x = c(1, 1.5, 2, 2, 2.5)
y = c(3, 5,   6, 6, 7)

vys2 <- lm(y ~ 1 + x + I(x^2))    
## <> lm( y ~ poly(x,2) ) # - orthogonal
plot(x,y)
lines(x, vys2$fit)
# <=>:
par = function(x, c) {
  c[1] + c[2]*x + c[3]*x^2 
}
xx = seq(min(x), max(x), .5);  ## FromToBy
lines(xx, par(xx, vys2$coef), col = "blue" )
xx = seq(min(x), max(x), .2);  ## FromToBy
lines(xx, par(xx, vys2$coef), col = "blue" )
xx = seq(min(x), max(x), .1);  ## FromToBy
lines(xx, par(xx, vys2$coef), col = "magenta" )
#==============
  
###
###     MATRIX EQUATION
###
x = c(1, 1.5, 2, 2, 2.5)
y = c(3, 5,   6, 6, 7)
X = cbind( rep(1,length(x)), x, x^2); X
plot(x,y)
# <=>
model.matrix(~ x + I(x^2))

# Solve system:
# a %*% x = b => solve(a,b) returns the unknown x
#     X*c = Y # <=>
#  tX*X*c = tX*c
#
#   X  * c  = Y       # <=>
# X_n,3*c_3,1 = Y_n,1 # <=>

# Coeffs c inverse:     for theory
solve( t(X) %*% X) %*% t(X) %*% y # - good 
# Solve system QR: for computation
solve( t(X) %*% X, t(X) %*% y)    # - better - more stable
# <=>
lm.fit(x = X, y = y)$coeff
# <=>
r2 = lm(y ~ 1 + x + I(x^2)); r2$coeff

summary(r2)  # p-value: 0.007013

###
### Further models:
###

r3_1 = lm(y ~ 1 + I(x^2) + I(x^3)) 
summary(r3_1)  # p-value: 0.01511

r3_2 = lm(y ~ 1 + x  + I(x^3)) 
summary(r3_2)  # p-value: 0.008873

vys3 = lm(y ~ 1 + x + I(x^2) + I(x^3))
# interpolation:
summary(vys3)  # p-value: < 2.2e-16
lines(x, vys3$fit, col='green', lw=2)
# !?? interpolation is not acceptable if it has too many points

{
  set.seed(129)
  
  n <- 7 ; p <- 2
  X <- matrix(rnorm(n * p), n, p); X # no intercept!
  yy <- rnorm(n)
  w <- rnorm(n)^2
  lm.fit(x = X, y = yy)
  lm.wfit(x = X, y = yy, w = w)
  
}
{
  require(utils)
  
  set.seed(129)
  
  n <- 7 ; p <- 2
  XX <- matrix(rnorm(n * p), n, p) # no intercept!
  yy <- rnorm(n)
  w <- rnorm(n)^2
  
  str(lmw <- lm.wfit(x = XX, y = yy, w = w))
  
  str(lm. <- lm.fit (x = XX, y = yy))
  
  #install.packages("microbenchmark")
  if(require("microbenchmark")) {
    mb <- microbenchmark(lm(yy~XX), lm.fit(XX,yy), .lm.fit(XX,yy))
    print(mb)
    boxplot(mb, notch=TRUE)
  }
}

#########################
  
###
### Generated model
###
### y = a - 3*x + b*x^2 + c*sin(x)
###
(x=-50:50/30)
a=1;b=-1;c=4
set.seed(123)
y=a-3*x+b*x^2+c*sin(x)+rnorm(length(x),0,0.4)
plot(x,y)

### 2 transform:
z=x^2; u=sin(x)
set.seed(123); y=a-3*x+b*z+c*u+rnorm(length(x),0,2.4)
plot(x,y,col='gray')

# Mod 1:
vys1=lm(y~x+z+u)
lines(x, vys1$fit, col='blue', lwd=2)  ### <=>:
coef(summary(vys1))
sum(vys1$residuals^2); #anova(vys1);   
# Mod 2:
vys2=lm(y~z)
lines(x, vys2$fit, col='magenta', lwd=2)  ### <=>:
coef(summary(vys2))
sum(vys2$residuals^2); #anova(vys2);   


################
###
### 3) Plots in corr. and reg. analysis
###
### VYCUC z GGPLOT_3.R


### 10) scatterplotMatrix
set.seed(1); n=70; sd=2
feach_= rep(c(letters[16:21]),each=n/6, length=n)
dat <- data.frame(x = -((1:n)-n/2)^2 + rnorm(n,sd=sd),
                  y = 1:n + rnorm(n,sd=sd+2),
                  z = n:1 + rnorm(n,sd=sd+0.1),
                  u = 1:n + rnorm(n,sd=sd+1),
                  v = n:1 + rnorm(n,sd=sd+8),
                  w = (1:n)/2 + rnorm(n,sd=sd+16),
                  feach = feach_,
                  gtime = rep(c(letters[16:21]),times=n/6, length=n),
                  hsamp = sample(feach_, replace = TRUE, size=n))
head(round(dat[,1:6],3))
dat$feach; dat$gtime; dat$hsamp

qplot(factor(feach), x, data = dat, 
      geom = c("boxplot", "jitter"))

table(dat$feach, dat$hsamp)
table(round(dat$x,-3), dat$hsamp)
table(dat$hsamp, round(dat$y,-1))

#install.packages("car")
library(car)
scatterplotMatrix(dat[,1:3], smooth=F,
                  diagonal="histogram")
scatterplotMatrix(dat[,1:6], smooth=F,
                  diagonal="histogram")
# histogram-boxplot-density-qqplot

### 11) Correlation matrix
#install.packages("ellipse")
library(ellipse)
# Correlation table
ct = cor(dat[,1:6]); round(ct, 2); h=0.01
colorfun = colorRamp(c("#CC0000","white","#3366CC"))
plotcorr(ct, col=rgb(colorfun((ct+1)/2), maxColorValue=255), mar = c(h,h,h,h))

library(GGally)
data(flea)

ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species)) 
### !!! See also: 
###     !!!!!!!!!! https://www.r-graph-gallery.com/correlogram.html
###     6_ggplot_____.R
###     _____3Dhist.R
###     ____________________!!!!!_____ggplot.R



################

###
### 4) Nonlinear and other types of regression
###
### 0) Gram-Schmidt orthogonalisation
### 1) Nonlinear nls(...)
### 2) Nonlinear optim(...)
### 3) Generalized Linear Models - glm(...)
###    Proportion and binary data
### 4) Logistic growth


###
### 0) Gram-Schmidt orthogonalisation
###
x = c(  45.3,    96.5,   164.8,   260.6,  429.1,   596.6,   775.1)
y = c(8886.09, 8643.75, 8340.46, 8126.6, 7486.71, 7370.64, 7258.11)
plot(x,y)
vys2 = lm(y ~ 1 + x + I(x^2))   
## <> lm( y ~ poly(x,2) ) $ - orthogonal polyn. - Gram-Schmidt orthogonalisation
lines(x, vys2$fit)
vys2$coefficients == lm( y ~ poly(x,2) )$coefficients

###
### 1) Nonlinear nls(...)
###
{
  nn=18
  set.seed(5948172)
  x = sort(runif(nn,0,2))
  y = 6*x/(2+x^3)
  ye = y +rnorm(nn,0,0.06)
  df = data.frame (x=x,y=y,ye=ye); df[1:3,]
  ### Treba zadat zaciatocne/startovacie hodnoty:
  v1 = nls(ye ~ aa*x /(bb + x^3),
           start = list( aa = 7, bb = 1), data=df)
  plot(x,ye)
  a0=coef(v1)[[1]]
  b0=coef(v1)[[2]]
  lines(x, a0*x /(b0 + x^3), col=2)
  summary(v1)
}


###
### 2) Nonlinear optim(...)
###
set.seed(594817)
x = seq(from=1, by=.2, length.out=21 )
e = stats::rnorm(length(x),mean=0,sd=3)
y = (x+4)^2
y = y + 1*e

f = function(v) { 
  a = v[1]; 
  b = v[2];
  sum(abs(y - ((x + a)^b)))
}
op = optim(c(1,1), f);  op[1:2]

###
### 3) Generalized Linear Models - glm(...)
###
###    Proportion and binary data
###    http://strata.uga.edu/8370/lecturenotes/generalizedLinearModels.html

###
### 4) Logistic growth
###
###    http://strata.uga.edu/8370/lecturenotes/nonlinearRegression.html

###
### 5) MCMC regression
###    Markkov Chain Monte Carlo
###    see script file: 6b_qMCMC_Regression_!!!!!!!!!!!!!!!.R
###############################


#=======================
### QQ plot:
x=1:100
y=1-3*x + rnorm(length(x),0,.5)
plot(x,y)
mod = lm(y~x)
summary(mod)
qqnorm(resid(mod)); qqline(resid(mod))
hist(resid(mod))
plot(mod$fit,mod$res) # <=> 
points(fitted(mod),resid(mod), col="blue")

