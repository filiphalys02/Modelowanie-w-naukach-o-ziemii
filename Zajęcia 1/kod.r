#Rozwiązać iteracyjnie równanie Laplace'a metodą MRS w siatce o wymiarach 10x10 przy następujących warunkach brzegowych:
#krawędzie: górna, lewa i prawa: temperatura 0 krawędź dolna: temperatura 1
# - Wyniki dla każdej iteracji wypisać na ekranie
# - Ustalić odpowiednie kryterium stopu dla prowadzonych obliczeń
# P rzedstawić animację rozwoju pola co 10 iteracji 

library(animation)
saveGIF({
  Lnew <- matrix(nrow=10,ncol=10,0)
  L <- matrix(nrow=10,ncol=10,0)
  
  L[,1] <- rep(0,10)
  L[,10] <- rep(0,10)
  L[1,] <- rep(0,10)
  L[10,] <- rep(1,10)
  Lnew <- L
  
  Limg <- apply(L, 2, rev)
  image(t(Limg))
  text(0,1,0)
  
  epsilon = 0.0005
  
  for (k in 0:100) {
    for (i in 2:9) {
      for (j in 2:9) {
        Lnew[i,j] <- 0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1]) 
      }
    }
    if (max(abs(Lnew - L)) < epsilon) {
      break 
    }
    L <- Lnew
    print(L)
    if (k %% 10 == 0 && k > 0) { 
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      text(0,1,k)
    }
  }
})
