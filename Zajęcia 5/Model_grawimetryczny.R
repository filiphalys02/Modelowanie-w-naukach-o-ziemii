# Pole grawitacyjne - instrukcja i wskazówki 
# 1) W siatce obliczeniowej o wymiarach 500 x 250; 
#    odległości między węzłami 1m wymodelować wartość siły ciążenia na powierzchni modelu. Wartości te należy obliczyć dla wszystkich węzłów pomiędzy 100 a 400 metrem modelu. 
#    Obliczenia wykonać przy założeniu że model składa się z tła o gęstość 3500 kg/m3 oraz 3 losowo zlokalizowanych ciał rudnych o średnicy 100 metrów 
#    i gęstości 5500 kg/m3.
# 2)  W dowolny sposób wykonać próbę przyspieszenia kodu z wykorzystaniem obliczeń równoległych. 
#     Zadanie jest zaliczone jeśli kod równoległy jest szybszy choćby o włos (możecie spodziewać się 2-3 krotnego przyspieszenia)


#install.packages('doSNOW')
#install.packages('foreach')

library(doSNOW)
library(foreach)

#funkcja dla odległości
getR <- function(j,i,k)
{
  x = j - k
  z = i
  r = sqrt(x^2+z^2)
  return(r)
}

#funkcja dla kąta
getAngle <- function(j,i,k)
{
  if (k==j)
  {
    return (90.0)    
  }
  if (k < j)
  {
    ii = j - k
    tg = i/ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
  }
  if (k > j)
  {
    ii = k-j
    tg = i/ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
  }
}

#wymiary
N = 500
M = 250

#gęstości
rho1 <- 3500
rho2 <- 5500

#stała grawitacji
gamma <- 6.67e-11

#macierz gęstości
rho <- matrix(nrow=M,ncol=N,rho1)

#generacja modelu
for (k in 1:3)
{
  x <- round(100 + runif(1)*(N - 200))
  z <- round(50 + runif(1)*(M - 100))
  for (i in 1:M) 
  {
    for (j in 1:N)  
    {
      if (sqrt((i-z)^2 + (j-x)^2) < 50 )
        rho[i,j] <- rho2  
    } 
  }
}

#zobaczmy model
image(t(apply(rho, 2, rev)),asp=0.5)
box()

#wektor na wyniki
g <- rep(0,N)

for (k in 100:400)
{
  suma <- 0 
  for (i in 1:M)
  {
    for (j in 1:N)
    {
      beta = (i+1-i)/(j+1-j)
      r = getR(j, i, k)
      r1 = getR(j, i+1, k)
      z = r1 / r  
      logarytm = log(z)  
      a = getAngle(j, i, k)  
      a1 = getAngle(j, i+1, k)  
      suma <- suma + ( rho[i,j] * beta * (logarytm - (a-a1)) )
    }
  }
  g[k] = 2*gamma*suma
}


#stworzenie i zarejestrowanie klastra
cl <- makeCluster(10)
registerDoSNOW(cl)

#pomiar czasu
start <- Sys.time()

#egzotyczna składnia foreach zwraca LISTĘ (a konkretnie wektor list) df zostaje kolejnotam zapisane
out <- foreach(i = 1:10) %dopar% {
  df <- rnorm(1000)
}

#pomiar czasu 
stop <- Sys.time()

#wypisanie różnicy czasu
stop - start

#deklaracja macierzy
m <- matrix(nrow=10,ncol=1000,0)

#przypisanie do macierzy "rozwiniętej listy"
m <- unlist(out)

#zwolnienie klastra
stopCluster(cl)

plot(g, xlim=c(100,400))
