#Rozwiązać iteracyjnie równanie Laplace'a metodą MRS w siatce o wymiarach 100x100 przy następujących warunkach brzegowych:
#krawędzie górna, lewa i prawa: temperatura 0 krawędź dolna: temperatura 1
# - Wyniki dla każdej iteracji wypisać na ekranie
# - Ustalić odpowiednie kryterium stopu dla prowadzonych obliczeń
# - Przedstawić animację rozwoju pola co 10 iteracji 
# - Ustawić ilość iteracji na 1000

library(animation)
saveGIF({
  N <- 100
  Lnew <- matrix(nrow=N,ncol=N,0)
  L <- matrix(nrow=N,ncol=N,0)
  
  L[,1] <- rep(0,N)
  L[,N] <- rep(0,N)
  L[1,] <- rep(0,N)
  L[N,] <- rep(1,N)
  Lnew <- L
  
  Limg <- apply(L, 2, rev)
  image(t(Limg))
  text(0.98,0.98,0)
  
  epsilon = 0.0005
  
  for (k in 1:1000) {
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
