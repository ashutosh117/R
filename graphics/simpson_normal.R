#to estimate the normal density and distribution function using simpsons rule
rm(list=ls())
simpson1 <- function(a,b,ftn,n=100){
  n <- max(2*(n%/%2),4)
  h <- (b-a)/n
  vec <- rep()
  vec[1] <- (h/3)*ftn(a)
  for(i in 2:n){
    if(i%%2==0){
      vec[i] <- vec[i-1] + 4*(h/3)*ftn(a + (i-1)*h)
    }
    else{
      vec[i] <- vec[i-1] + 2*(h/3)*ftn(a + (i-1)*h)
    }
  }
  vec[n+1] <- vec[n] + (h/3)*ftn(b)
  library(animation)
  #create animation and save in HTML format
  saveHTML({
    ani.options(interval=0.001,ani.width=700,ani.height=700)
    for(i in 1:length(vec)){
      x <- seq(a,a+(i-1)*h,by=h)
      plot(x,vec[1:i],type ="line",col="red",xlim = c(a,b),ylim = c(0,vec[n+1]))
      lines(x,sapply(x,ftn),xlim = c(a,b),ylim = c(0,vec[n+1]))
      legend(a,vec[n+1],c("density function","distribution function"),lty=1,col=c("black","red"))
    }
  })
  return(vec[n+1])
}

phi <- function(x){
  return(exp(-x^2/2)/sqrt(2*pi))
}

simpson1(-3,3,phi,n=500)
