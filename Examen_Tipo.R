library(ggplot2)
library(reshape2)


#1

x=dnorm(0.3,0,1); x

y=pnorm(0.3,0,1); y

z=qnorm(0.25,0,1); z

#4

p1=ppois(10,6) ##Es cero
p2=ppois(5,6); p2
p3=qpois(0.1,6); p3

#5
#Como es una exp de media 2 --> lambda=1/media=1/2
q1=qexp(0.8,2)

#6
a1=runif(20,0,1); a1
a2=rpois(20,6); a2
a3=rexp(20,0.5); a3
a4=rnorm(20,0,1); a4

#7

b1=rnorm(10000,0,1)
xcdf=ecdf(b1)

plot(xcdf)

#8

b2=rnorm(10000,0,1.2)

m12=list(b1=b1,b2=b2)
boxplot(m12)

#9
media=mean(a4); media
varianza=var(a4);varianza
c1=quantile(a4,0.7);c1

#one-sided

alfa=0.05
gamma=1-alfa
n=length(a4)

#media

t_gamma=qt(gamma,n-1)

md1=media-t_gamma*sqrt(varianza)/sqrt(n); md1

mh1=media+t_gamma*sqrt(varianza)/sqrt(n); mh1

#varianza

chi_gamma=qchisq(gamma,n-1)
chi_1menosgamma=qchisq(1-gamma,n-1)

sigmad1=(n-1)*varianza/chi_gamma; sigmad1
sigmah1=(n-1)*varianza/chi_1menosgamma; sigmah1

#Two sided

#media
t_gamma1medios=qt((gamma+1)/2,n-1)

md2=media-t_gamma1medios*sqrt(varianza)/sqrt(n)
mh2=media+t_gamma1medios*sqrt(varianza)/sqrt(n)

intervalo=list(inferior=md2,superior=mh2); intervalo

#varianza

chi_gamma1medios=qchisq((gamma+1)/2,n-1)
chi_gammamenos1medios=qchisq((1-gamma)/2,n-1)

sigmad2=(n-1)*varianza/chi_gamma1medios
sigmah2=(n-1)*varianza/chi_gammamenos1medios

intervalo2=list(inferior=sigmad2,superior=sigmah2); intervalo2

#10

month1 = c(8.6, 8.8, 8.4, 8.6, 8.7, 8.5, 8.5)
month2 = c(8.6, 8.9, 8.8, 8.9, 8.7, 8.6, 9, 8.5, 8.6, 8.7, 8.7)

#test para ver si sigma1=sigma2:



fun_test=function(x,y,alfa){
  F_test=var.test(x,y,alternative = "two.sided");
  if (F_test$p.value<alfa){
    print('H0 rejected, not equal dispersion')
    tt=t.test(x,y,var.equal=F,alternative = 'less')
    
    }
  else {
    #p>alfa => disperzie sa rovnaju
    T_test = t.test(x, y, var.equal = TRUE, alternative = "less");
  }
  
  if(T_test$p.value < alfa){
    print("H0 is rejected, higher sale")
  } else {
    print("H0 is not rejected, the sale is not higher")
  }
}
fun_test(month1,month2,alfa)
  

  






