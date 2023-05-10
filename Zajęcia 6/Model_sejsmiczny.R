# Modelowanie sejsmicznego pola falowego
# W siatce o wymiarach 500x500 węzłów odległych o 1 m wymodelować w rozchodzenie się fal akustycznych wokół źródła danego funkcją Rickera
# a zlokalizowanego w połowie  długości modelu na głębokości 25 metrów. 
# Ośrodek składa się z dwóch warstw: górnej o prędkości 1000 m/s i sięgającej do połowy modelu oraz dolej o prędkości 2000 m/s.
# Wynikiem zadania mają być:
# 1) Sejsmogram będący zapisem fal dochodzących do górnej krawędzi modelu w funkcji czasu i zapisywanego z krokiem 0.002 sekundy 
# 2) Animacja przedstawiająca rozchodzenie się fal z krokiem 0.02


library(animation)

#parametry modelu
nz <- 500     #rozpietosc pionowa
nx <- 500     #rozpietosc pozioma
fpeak <- 30.0
dt <- 0.002   #krok czasowy
et <- 0.5     #calkowity czas
ds <- 1.0     #odległość pomiędzy węzłami w których liczymy ciśnienie

#polożenie źródła
xs <- nx/2.0  #w samym środku składowej poziomej
zs <- 25      #na wysokości zs

#ile będzie kroków czasowych dla sejsmogramu
nt <- et/dt+1;

#model
V <- matrix(nrow=nz,ncol=nx,2000) #uzupełnia model wartością 2000

for (i in 1:nz/2)                 #uzupełnia połowę modelu wartością 1000
{
  for (j in 1:nx) 
  {
    V[i,j] <- 1000 
  }
}

#macierze dla pól ciśnień w czasie t+1, t i t-1
p <- matrix(nrow=nz,ncol=nx,0)
pm <- matrix(nrow=nz,ncol=nx,0)
pp <- matrix(nrow=nz,ncol=nx,0)

#trzeba znaleźć vmax do warunku stabilności
vmax <- 2000  #maksymalna prędkość fali

#dtr - realny krok próbkowania który jest dzielnikiem dt
dtr <- ds/(2.0*vmax)

w2 <- 0

while (1)
{
  w2 <- w2+1
  w1 <- dt/w2
  if (w1 <= dtr) 
  {
    dtr <- w1 
    break 
  }
}

#inicjalizacja paska postępu
niter = et/dtr+1
prog_bar <- txtProgressBar(min=0,max=niter,style=3)

k <- 1 #ilosc dtr
kk <- 1 #ilosc dtr na jedna dt
kkk <- 0 #ilosc dt
seis<-matrix(nrow=nt,ncol=nx,0)
x <- 1

saveGIF({
  while (1)
  {
    k <- k+1    #wraz ze wzrostem dtr
    kk <- kk+1  #wzrasta ilość dtr na jedna dt
    t <- k*dtr  #a czas wynosi (długość dtr*ilość dtr)
  
    #pasek postępu
    setTxtProgressBar(prog_bar, k)
  
    for (i in 2:(nz-1))
    {
      for (j in 2:(nx-1))
      {
        pp[i,j] = 2.0*p[i,j]-pm[i,j] + ((dtr*dtr)/(ds*ds))*V[i,j]*V[i,j]* 
        (p[i+1,j]+p[i-1,j]+p[i,j+1]+p[i,j-1]-4.0*p[i,j]) 
      }
    }

    #dodanie źródła Rickera 
    pp[zs,xs] <- pp[zs,xs]+exp(-(((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak))))))*(1.0-2.0*((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak)))))

    #trasparent lewa - warunek brzegowy dla lewej krawędzi modelu
    for (i in 1:nz)
    {
      pp[i,1] = p[i,1] + p[i,2] - pm[i,2] + V[i,1]*(dtr/ds)*(p[i,2]-p[i,1]-(pm[i,3]-pm[i,2]))
    }

    #trasparent prawa - warunek brzegowy dla prawej krawędzi modelu
    for (i in 1:nz)
    {
      pp[i,500] = p[i,500] + p[i,499] - pm[i,499] + V[i,500]*(dtr/ds)*(p[i,499]-p[i,500]-(pm[i,498]-pm[i,499]))
    }

    #trasparent gora - warunek brzegowy dla gornej krawędzi modelu
    for (i in 1:nz)
    {
      pp[1,i] = p[1,i] + p[2,i] - pm[2,i] + V[1,i]*(dtr/ds)*(p[2,i]-p[1,i]-(pm[3,i]-pm[2,i]))
    }

    #trasparent dol - warunek brzegowy dla dolnej krawędzi modelu
    for (i in 1:nz)
    {
      pp[500,i] = p[500,i] + p[499,i] - pm[499,i] + V[500,i]*(dtr/ds)*(p[499,i]-p[500,i]-(pm[498,i]-pm[499,i]))
    }

    #przejście o krok do przodu z macierzami
    pm <- p
    p <- pp
  
    #warunek do zapisania próbki sejsmogramu 
    if ( kk*dtr+dtr/10.0 >= dt )
    {
      kk <-0 
      kkk <- kkk+1
      #dodanie próbek do seismogramu
      # zapisywanie wartości z górnej krawędzi modelu do sejsmogramu
      for(z in 1:nx)
      {
        seis[x, z] <- pp[1, z]
      }
      x=x+1
    
      # zapisanie animcaji 
      image(t(apply(pp, 2, rev)))
      text(0.2, 0.9, kkk * dt)
      
      #przerwanie po czasie
      if (kkk*dt > et) 
        break
    }
  }
})
  
#wyrysowanie pola po obliczeniach
image(t(apply(p, 2, rev)))
text(0.2,0.9,kkk*dt)



