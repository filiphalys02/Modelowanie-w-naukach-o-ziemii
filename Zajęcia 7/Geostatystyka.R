#install.packages("sf")

library(sf)

# wyczytanie danych z pliku shape
dzielnice <- st_read("dzielnice_Krakowa.shp")

# transformacja do WGS
dzielniceWGS <- st_transform(dzielnice,crs = 4326) 

# nie chcemy podziału na dzielnice więc je wszystkie łączymy ze sobą 
krakowWGS <- st_union(st_geometry(dzielniceWGS))

data <- read.csv("Wyniki_em_2019.csv",header=TRUE,encoding="UTF-8")

# SZEROKOSC
#usunięcie "\" z końca stringa z szerokości geograficznej (ostatni znak)
pom0 <- substr(data$Szerokość.geograficzna,1,nchar(data$Szerokość.geograficzna)-1)

# podział na stopnie minuty sekundy 
pom1 <- strsplit(pom0, "°" )
pom2 <- strsplit(unlist(pom1), "' " )

# wszystko po kolei
lats <- as.numeric(unlist(pom2))

# pierwsze wartości stopnie, drugie minuty itp.
d <- lats[seq(1, length(lats), 3)]
m <- lats[seq(2, length(lats), 3)]
s <- lats[seq(3, length(lats), 3)]

# do decimal
lat <- d + (m/60) + (s/3600)
class(lat)

# DLUGOSC 
# usunięcie "\" z końca stringa z szerokości geograficznej (ostatni znak)
pom0 <- substr(data$Długość.geograficzna,1,nchar(data$Długość.geograficzna)-1)

# podział na stopnie minuty sekundy 
pom1 <- strsplit(pom0, "°" )
pom2 <- strsplit(unlist(pom1), "' " )

# wszystko po kolei
lons <- as.numeric(unlist(pom2))

# pierwsze wartości stopnie, drugie minuty itp.
d <- lons[seq(1, length(lats), 3)]
m <- lons[seq(2, length(lats), 3)]
s <- lons[seq(3, length(lats), 3)]

# do decimal
lon <- d + (m/60) + (s/3600)

# od razu wartości pola
value <- as.numeric(data$Wynik.pomiaru..V.m.)

# budujemy ramki danych dla Polski
dataP <- data.frame(longitude=lon,latitude=lat)
dataP$value <- value

# statystyki przestrzenne
#install.packages("spatstat")
#install.packages("rgdal")
library(sp)
library(rgdal)

# trzeba przejść ze sfery (lat lon) na płaską mapę w ukladzie utm (małopolska jest w 34N)
krakowUTM <- st_transform(krakowWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))

# to samo z naszymi danymi o czujnikach
data_spat <- data.frame(lon=dataP$longitude, lat=dataP$latitude, value=dataP$value)
coordinates(data_spat) <- ~lon+lat
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84")
#data_spat #Gotowe do konwersji

#konwersja
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34 +datum=WGS84"))

library(spatstat)

# stworzenie obiektu ppp 2D
dataP_ppp <- ppp(x = data_UTM$lon, y = data_UTM $ lat, window = as.owin(krakowUTM))

# stworzenie ppp z marks, czyli z danymi w punktach (tu wartość pola)
dataP_ppp_v <- ppp(x = data_UTM$lon, y = data_UTM$lat, marks = data_UTM$value, window = as.owin(krakowUTM))

# MAPA
#install.packages('maptools')
#install.packages('automap')
#install.packages('raster')
library(maptools)
library(automap)
library(raster)

# znowu konwersja
dataP_spdf <- as.SpatialPointsDataFrame.ppp(dataP_ppp_v)
spplot(dataP_spdf)
coordinates(dataP_spdf)

# Ordinary Kriging
elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,model = "Mat")
plot(elev_auto)

# plot tylko dla estymacji parametru 
plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch = "*", col = "Green")
plot(Window(dataP_ppp_v), add=TRUE)



# WOJEWODZTWA
wojewodztwa <- st_read("Wojewodztwa.shp")

# transformacja do WGS
wojewodztwaWGS <- st_transform(wojewodztwa, crs = 4326)
# Zmiana nazwy na bardziej przystepna i zrozumiala
names(wojewodztwaWGS)[names(wojewodztwaWGS) == "JPT_NAZWA_"] <- "Nazwa"
# Usuniecie niepotrzebnej kolumny
wojewodztwaWGS$JPT_KOD_JE <- NULL

wojewodztwaUTM <- st_transform(wojewodztwaWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(wojewodztwaUTM)

podkarpackie <- wojewodztwaWGS[ wojewodztwaWGS$Nazwa == "podkarpackie", ]
plot(podkarpackie)
malopolskie <- wojewodztwaWGS[ wojewodztwaWGS$Nazwa == "małopolskie", ]
plot(malopolskie)
swietokrzyskie <- wojewodztwaWGS[ wojewodztwaWGS$Nazwa == "świętokrzyskie", ]
plot(swietokrzyskie)
mazowieckie <- wojewodztwaWGS[ wojewodztwaWGS$Nazwa == "mazowieckie", ]
plot(mazowieckie)


# PODKARPACKIE
podkarpackieWGS <- st_transform(podkarpackie, crs = 4326) 
podkarpackieUTM <- st_transform(podkarpackieWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(podkarpackieUTM)

dataP_ppp_podkarpackie <- ppp(x = data_UTM$lon, y = data_UTM$lat, window = as.owin(podkarpackieUTM))
plot(dataP_ppp_podkarpackie)

dataP_ppp_v_podkarpackie <- ppp(x = data_UTM$lon, y = data_UTM$lat, marks = data_UTM$value, window = as.owin(podkarpackieUTM))
plot(dataP_ppp_v_podkarpackie)

dataP_spdf_podkarpackie <- as.SpatialPointsDataFrame.ppp(dataP_ppp_v_podkarpackie)
spplot(dataP_spdf_podkarpackie)
coordinates(dataP_spdf_podkarpackie)

elev_auto_podkarpackie <- autoKrige(marks ~ 1, input_data = dataP_spdf_podkarpackie, model = "Mat")
plot(elev_auto_podkarpackie)

plot(elev_auto_podkarpackie$krige_output[1])
points(dataP_ppp_v_podkarpackie, pch = "*", col = "Green")
plot(Window(dataP_ppp_v_podkarpackie), add=TRUE)


# MALOPOLSKIE
malopolskieWGS <- st_transform(malopolskie, crs = 4326) 
malopolskieUTM <- st_transform(malopolskieWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(malopolskieUTM)

dataP_ppp_malopolskie <- ppp(x = data_UTM$lon, y = data_UTM$lat, window = as.owin(malopolskieUTM))
plot(dataP_ppp_malopolskie)

dataP_ppp_v_malopolskie <- ppp(x = data_UTM$lon, y = data_UTM$lat, marks = data_UTM$value, window = as.owin(malopolskieUTM))
plot(dataP_ppp_v_malopolskie)

dataP_spdf_malopolskie <- as.SpatialPointsDataFrame.ppp(dataP_ppp_v_malopolskie)
spplot(dataP_spdf_malopolskie)
coordinates(dataP_spdf_malopolskie)

elev_auto_malopolskie <- autoKrige(marks ~ 1, input_data = dataP_spdf_malopolskie, model = "Lin")
plot(elev_auto_malopolskie)

plot(elev_auto_malopolskie$krige_output[1])
points(dataP_ppp_v_malopolskie, pch = "*", col = "Green")
plot(Window(dataP_ppp_v_malopolskie), add=TRUE)

# SWIETOKRZYSKIE
swietokrzyskieWGS <- st_transform(swietokrzyskie, crs = 4326) 
swietokrzyskieUTM <- st_transform(swietokrzyskieWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(swietokrzyskieUTM)

dataP_ppp_swietokrzyskie <- ppp(x = data_UTM$lon, y = data_UTM$lat, window = as.owin(swietokrzyskieUTM))
plot(dataP_ppp_swietokrzyskie)

dataP_ppp_v_swietokrzyskie <- ppp(x = data_UTM$lon, y = data_UTM$lat, marks = data_UTM$value, window = as.owin(swietokrzyskieUTM))
plot(dataP_ppp_v_swietokrzyskie)

dataP_spdf_swietokrzyskie <- as.SpatialPointsDataFrame.ppp(dataP_ppp_v_swietokrzyskie)
spplot(dataP_spdf_swietokrzyskie)
coordinates(dataP_spdf_swietokrzyskie)

elev_auto_swietokrzyskie <- autoKrige(marks ~ 1, input_data = dataP_spdf_swietokrzyskie, model = "Mat")
plot(elev_auto_swietokrzyskie)

plot(elev_auto_swietokrzyskie$krige_output[1])
points(dataP_ppp_v_swietokrzyskie, pch = "*", col = "Green")
plot(Window(dataP_ppp_v_swietokrzyskie), add=TRUE)


# MAZOWIECKIE
mazowieckieWGS <- st_transform(mazowieckie, crs = 4326) 
mazowieckieUTM <- st_transform(mazowieckieWGS, CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(mazowieckieUTM)

dataP_ppp_mazowieckie <- ppp(x = data_UTM$lon, y = data_UTM$lat, window = as.owin(mazowieckieUTM))
plot(dataP_ppp_mazowieckie)
 
dataP_ppp_v_mazowieckie <- ppp(x = data_UTM$lon, y = data_UTM$lat, marks = data_UTM$value, window = as.owin(mazowieckieUTM))
plot(dataP_ppp_v_mazowieckie)

dataP_spdf_mazowieckie <- as.SpatialPointsDataFrame.ppp(dataP_ppp_v_mazowieckie)
spplot(dataP_spdf_mazowieckie)
coordinates(dataP_spdf_mazowieckie)

elev_auto_mazowieckie <- autoKrige(marks ~ 1, input_data = dataP_spdf_mazowieckie, model = "Mat")
plot(elev_auto_mazowieckie)

plot(elev_auto_mazowieckie$krige_output[1])
points(dataP_ppp_v_mazowieckie, pch = "*", col = "Green")
plot(Window(dataP_ppp_v_mazowieckie), add=TRUE)


