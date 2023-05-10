#Ćw. 3
#Założyć, że na wszystkich granicach temperatura wynosi 0 ale w środku obszaru (nie na brzegach!) porusza się losowo (zmieniając położenie co iterację, rozkład jednostajny) punkt o stałej wartości 1.

library(animation)

saveGIF({
  N <- 100
  Lnew <- matrix(nrow=N,ncol=N,0)
  L <- matrix(nrow=N,ncol=N,0)
  
  L[,1] <- rep(0,N)
  L[,N] <- rep(0,N)
  L[1,] <- rep(0,N)
  L[N,] <- rep(0,N)
  Lnew <- L
  pos <- c(0,0)
  pom <- c(0,0)
  pos[1] <- round(runif(2)*(N-2)+1)
  pos[2] <- round(runif(2)*(N-2)+1)
  L[pos[1], pos[2]] <- 1
  
  Limg <- apply(L, 2, rev)
  image(t(Limg))
  text(0.98,0.98,0)
  
  epsilon = 0.0005
  
  
  
  for (k in 1:1000) {
    pom[1] <- pos[1] + round(runif(2)*2-1)
    pom[2] <- pos[2] + round(runif(2)*2-1)
    while (pom[1]<=0 || pom[1]>=N || pom[2]<=0 || pom[2]>=N )
    {
      pom[1] <- pos[1] + round(runif(2)*2-1)
      pom[2] <- pos[2] + round(runif(2)*2-1)
    }
    pos[1] <- pom[1]
    pos[2] <- pom[2]
    L[pos[1], pos[2]] <- 1
    
    for (i in 2:99) {
      for (j in 2:99) {
        Lnew[i,j] <- 0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1]) 
      }
    }
    if (max(abs(Lnew - L)) < epsilon) {
      break 
    }
    L <- Lnew
    
    if (k %% 10 == 0 && k > 0) { 
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      text(0.98,0.98,k)
    }
  }
})
