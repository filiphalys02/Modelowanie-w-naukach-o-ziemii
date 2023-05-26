#Proste mcmc za 
#Florian Hartig, A simple Metropolis-Hastings MCMC in R, Theoretical Ecology


#dane które muszą być wcześniej to wektory x i y !!!

x <- seq(1,100)
y <- 3*x+4+rnorm(100,0,3)

plot(x,y)

#funkcja wiarygodności 
likelihood <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# to jest ważne - rozkłada a priori -> co przypuszczamy o parametrach
# tu trzeba będzie zmienić zakres w rozkładach (min i max)
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dunif(b, min=-5, max=5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

#prawd. a posteriori 
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

######## Metropolis algorithm ################

#funkcja propozycji tu ustalamy szerokość przeszukiwania
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.05,0.05,0.05)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab <- exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

#to jest ważne: parametry startowe dla a,b i sd 
startvalue = c(1,1,1)
#tutaj ważna jest ilość iteracji: 10000 to minimum 
chain = run_metropolis_MCMC(startvalue, 10000)

#ilość elementów które będziemy pomijać na początku każdego łańcucha  
burnIn = 5000
#procent przyjętych rozwiązań 
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

#czy wsp. akceptacji jest dobry?
acceptance

#a jak to widzi zwykły model liniowy 
summary(lm(y~x))

#jakie sa rozklady naszych parametrow 

#a
plot(density(chain[-(1:burnIn),1]))
mean(chain[-(1:burnIn),1])
sd(chain[-(1:burnIn),1])

#b
plot(density(chain[-(1:burnIn),2]))
mean(chain[-(1:burnIn),2])
sd(chain[-(1:burnIn),2])

#sd
plot(density(chain[-(1:burnIn),3]))
mean(chain[-(1:burnIn),3])
sd(chain[-(1:burnIn),3])

##Klimat 
#install.packages('httr')
#install.packages('jsonlite')
#install.packages('tseries')
#install.packages('segmented')
library(httr)
library(jsonlite)
library(tseries)
library(segmented)

# Ustawienie lokalizacji na angielską
Sys.setlocale("LC_ALL","English")

# Pobieranie danych pogodowych/klimatycznych dla Krakowa z API open-meteo
r <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=50.06&longitude=19.56&start_date=1950-01-01&end_date=2022-12-31&daily=temperature_2m_max,temperature_2m_min,temperature_2m_mean,precipitation_sum,precipitation_hours,winddirection_10m_dominant&timezone=Europe%2FWarsaw",
         Accept = "application/json")
jsonRespText <- content(r, as = "text")
aux <- fromJSON(jsonRespText)

# Tworzenie ramki danych dla danych pogodowych Krakowa
krakow <- data.frame(time = aux$daily$time,
                     t_2m_max = aux$daily$temperature_2m_max,
                     t_2m_min = aux$daily$temperature_2m_min,
                     t_2m_mean = aux$daily$temperature_2m_mean,
                     p_sum = aux$daily$precipitation_sum,
                     p_h = aux$daily$precipitation_hours,
                     w_d = aux$daily$winddirection_10m_dominant)

krakow$time <- as.Date(krakow$time)

summary(krakow)


#Wyodrębnienie dnia, miesiąca i roku z daty
day <- format(krakow$time, format = "%d")
month <- format(krakow$time, format = "%m")
year <- format(krakow$time, format = "%Y")

# Konwersja dnia, miesiąca i roku na liczbę
krakow$day <- as.numeric(day)
krakow$month <- as.numeric(month)
krakow$year <- as.numeric(year)

#policzmy średnie miesięczne dla opadów i temperatury średniej dla krakowa 
t <- aggregate( t_2m_mean ~ month+ year , krakow , mean )
p <- aggregate( p_sum ~ month+ year , krakow , mean )

#R sortuje alfabetycznie dlatego musimy go przed tym powstrzymać
krakow_m <- merge(t,p,by=c("year","month"),sort=FALSE)

summary(krakow_m)
head(krakow_m)

#wezmy lipiec w krakowie
test_k <- krakow_m[krakow_m$month==7,]

plot(test_k$year,test_k$t_2m_mean)

# Wykonanie analizy segmentowanej
#install.packages('segmented')
library(segmented)

# Tworzenie modelu liniowego
fit_lm = lm(t_2m_mean ~ year + 1, data = test_k)
plot(test_k$year,test_k$t_2m_mean)
lines(test_k$year,predict(fit_lm))

# Wykonanie analizy segmentowanej dla jednego punktu
fit_segmented = segmented(fit_lm, seg.Z = ~ year, npsi = 1)

# Tworzenie wykresu z analizą segmentowaną
plot(test_k$year,test_k$t_2m_mean)
plot(fit_segmented,add=TRUE)
lines.segmented(fit_segmented)
points.segmented(fit_segmented)

# który to rok?
print(fit_segmented$psi)


#zbadajmy teraz ten trend w konwencji MCMC

x <- test_k$year[test_k$year>=1978]
y <- test_k$t_2m_mean[test_k$year>=1978]
plot(x,y)

#lepiej nam tak będzie zadawać priori
x <- seq(1,length(x))
summary(lm(y~x)) #temperatura zmienia sie rocznie o 0.07405 stopnia

prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=-1, max=1, log = T)
  bprior = dunif(b, min=0, max=20, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.05,0.05,0.05)))
}

startvalue = c(0,16,1)

chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
acceptance

#a - wsp. kierunkowy prostej
plot(density(chain[-(1:burnIn),1]))
mean(chain[-(1:burnIn),1])
sd(chain[-(1:burnIn),1])

#b - wyraz wolny w rownaniu prostej
plot(density(chain[-(1:burnIn),2]))
mean(chain[-(1:burnIn),2])
sd(chain[-(1:burnIn),2])

#sd
plot(density(chain[-(1:burnIn),3]))
mean(chain[-(1:burnIn),3])
sd(chain[-(1:burnIn),3])


#A jak było dawniej?
x <- test_k$year[test_k$year<=1978]
y <- test_k$t_2m_mean[test_k$year<=1978]
plot(x,y)

#lepiej nam tak będzie zadawać priori
x <- seq(1,length(x))
summary(lm(y~x))

prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=-1, max=1, log = T)
  bprior = dunif(b, min=0, max=20, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.05,0.05,0.05)))
}

startvalue = c(0,18,1)
chain = run_metropolis_MCMC(startvalue, 10000)
burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))
acceptance

#a
plot(density(chain[-(1:burnIn),1]))
mean(chain[-(1:burnIn),1])
sd(chain[-(1:burnIn),1])

#b
plot(density(chain[-(1:burnIn),2]))
mean(chain[-(1:burnIn),2])
sd(chain[-(1:burnIn),2])

#sd
plot(density(chain[-(1:burnIn),3]))
mean(chain[-(1:burnIn),3])
sd(chain[-(1:burnIn),3])