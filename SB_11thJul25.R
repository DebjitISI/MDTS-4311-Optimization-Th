rm(list=ls())
a0=-5
b0=15
for(i in 1:7)
{
  alpha1=a0+0.382*(b0-a0)
  alpha2=a0+0.618*(b0-a0)
  f_alpha1=alpha1^2
  f_alpha2=alpha2^2
  if(f_alpha1<f_alpha2)
  {
    a0=a0
    b0=alpha2
  }else
  {
    a0=alpha1
    b0=b0
  }
  print(paste(a0,b0,sep=","))
}


f=function(a0,b0,i=0)
{
  alpha1=a0+0.382*(b0-a0)
  alpha2=a0+0.618*(b0-a0)
  f_alpha1=alpha1^2
  f_alpha2=alpha2^2
  i=i+1
  if (i==7){
    return(c(a0,b0))
  }
  if(f_alpha1<f_alpha2)
  {
    a0=a0
    b0=alpha2
    print(paste(a0,b0,sep=","))
    f(a0,b0,i)
  }else
  {
    a0=alpha1
    b0=b0
    print(paste(a0,b0,sep=",")) 
    f(a0,b0,i)
  }

}
f(-5,15)
minimizer=(a0+b0)/2;minimizer

################################################################################
rm(list=ls())
a0=0
b0=1
while(b0-a0>0.3)
{
  alpha1=a0+0.382*(b0-a0)
  alpha2=a0+0.618*(b0-a0)
  f_alpha1=alpha1*(alpha1-1.5)
  f_alpha2=alpha2*(alpha2-1.5)
  if(f_alpha1<f_alpha2)
  {
    a0=a0
    b0=alpha2
  }else
  {
    a0=alpha1
    b0=b0
  }
  print(paste("[",a0,',',b0,']'))
}
minimizer=(a0+b0)/2;minimizer


library(pracma)
f<-function(x) {x^2}
res<-fibsearch(f,-5,15,tol=1e-10)
print(res$xmin)
print(res$fmin)
library(cmna)
goldsectmin(f,-5,15,tol=.001,m=7)
goldsectmax(f,-5,15,tol=.001,m=7)
