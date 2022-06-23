#functional value
f<-function(x){
  
  return(exp(x)-10)
}
#curve fit
{curve(f, xlim=c(-5,5), col='blue', lwd=2, lty=2, ylab='f(x)')
  abline(h=0)
  abline(v=0)
}


#derivative of f(x)

f_deriv<-function(x)
{
  eps=2.2*10^-16
  h = sqrt(eps) * x;
  xph = x + h;
  dx = xph - x;
  slope=(f(xph) - f(x)) / dx;
  return(slope)
}


#Newton_Raphson Method 

newton_raphson<-function(x1){
  if (f(x1)==0){
    return(x1)
  }
  else{
  for (i in 1:10) {
    x2=x1-(f(x1)/f_deriv(x1))
    if (abs(x2 - x1) < 0.000001) {
     return(x1)
     break
      
    }
    x1=x2
  }
  }
  return(x2)
}


#initial guess of upper and lower boundary from graph
a=2
b=4

#code for finding root
{
fa <- f(a)
fb <- f(b)
if (fa == 0.0) {
  return(a)
}
else if (fb==0.0) {
  return(b)
  
}
else {
  newton_raphson(a)
}
}








'''
{
j=0
{if(f(0)<=0){
  while (f(j)<=0) {
    f(j)
    x=j
    j=j+1
  }}
else{
  while (f(j)>0) {
    f(j)
    j=j-1
    x=j
    
    }
    }
  }

newton_raphson(x)
}###

