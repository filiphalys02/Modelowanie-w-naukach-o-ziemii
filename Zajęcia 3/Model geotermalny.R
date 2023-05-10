library(animation)
library(rmutil)
library(doSNOW)
library(foreach)
library(plotrix)

kolka <- function(a, x_srodek, y_srodek, r) {
  x = 1:nrow(a)
  y = x
  okrag = outer(x, y, function(x,y) sqrt(((x - x_srodek)^2) + ((y - y_srodek)^2)))
  war <- ifelse((okrag < r | a==6), 6, 2)
  return(war)
}

xsrodek <- c(0,0,0)
ysrodek <- c(0,0,0)

start <- Sys.time()
N <- 200
h <- 5

Lnew <- matrix(nrow=N,ncol=N,0)
L <- matrix(nrow=N,ncol=N,0)

a <- matrix(nrow=N,ncol=N,0.002)          
a[66:132,1:(N/2)] <- rep(0.006,67*(N/2))
image(t(apply(a, 2, rev)))

for(i in 1:3) {
  x_srodek <- round(runif(1, min=50, max=150))
  xsrodek[i] <- x_srodek
  y_srodek <- round(runif(1, min=50, max=150))
  ysrodek[i] <- y_srodek
  a <- kolka(a, x_srodek, y_srodek, 50)
}

a_max <- max(a)
dt <- h^2/(4*a_max)
t <- 0
L[,1] <- rep(0,N)
L[,N] <- rep(0,N)
L[1,] <- rep(0,N)
L[N,] <- rep(50,N)
Lnew <- L 

niter <- 30000
prog_bar<-txtProgressBar(min=0,max=niter,style=3)  
saveGIF({
  stepi <- (-1)
  
  for (k in 1:niter) {
    t <- t + dt
    stepi <- stepi + 1
    setTxtProgressBar(prog_bar, stepi)
    for (i in 2:(N-1))
      for (j in 2:(N-1)) {
        Lnew[i,j] <- (1-(4*dt*a[i,j])/(h^2))*L[i,j] +dt*a[i,j]*((L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])/(h^2))
      }
    Lnew[,1] <- L[,2]
    Lnew[,N] <- L[,N-1]
    auxL <- L
    L <- Lnew
    if (k%%100==0) {
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      text(0.2,0.9,t)
      for (l in 1:3){ draw.ellipse(1-xsrodek[l]/200, (ysrodek[l]/200), a = 0.25, b = 0.25, nv = 100, lwd = 1, border = "red") }
      box()
    }
  }
},interval=0.1)
stop<-Sys.time()
stop-start

cl<-makeCluster(1)
registerDoSNOW(cl)
start<-Sys.time()

out<-foreach(i=1:10) %dopar% {
  df<-rnorm(1000)
}
stop<-Sys.time()

m<-matrix(nrow=10,ncol=1000,0)
m<-unlist(out)
stopCluster(cl)
