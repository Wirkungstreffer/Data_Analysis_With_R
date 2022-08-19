if(!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}

if(!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

if(!require(magrittr)) {
  install.packages("magrittr")
  require(magrittr)
}

## Aufgabe 1
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Logistikverzug_file <- list.files(path = "Data/Logistikverzug/")
Logistikverzug_path <- file.path("Data/Logistikverzug", Logistikverzug_file)

Komponente_K7 <- read_csv2(Logistikverzug_path[1], col_names = TRUE, col_types = cols())
str(Komponente_K7)

Logistikverzug_K7 <- read_csv2(Logistikverzug_path[2], col_names = TRUE, col_types = cols())
str(Logistikverzug_K7)

anyNA(Komponente_K7)
anyNA(Logistikverzug_K7)

Gesamt_Logistikjoin <- full_join(Komponente_K7,Logistikverzug_K7,by = "IDNummer")

str(Gesamt_Logistikjoin)
anyNA(Gesamt_Logistikjoin)

#### Aufgabe a und d
Logistikverzug <- select(Gesamt_Logistikjoin,X1.x,IDNummer,Produktionsdatum,Wareneingang)
colnames(Logistikverzug) <- c( "X","IDNummer", "Produktionsdatum", "Wareneingang")


Logistikverzug$Produktionsdatum = as.Date(Logistikverzug$Produktionsdatum, "%Y-%m-%d")
Logistikverzug$Wareneingang = as.Date(Logistikverzug$Wareneingang, "%d.%m.%Y")

Logistikverzug <- mutate(Logistikverzug, Verzug = round(as.double (Wareneingang - Produktionsdatum),0))


ggplot(Logistikverzug, aes(x = Verzug)) +
  theme_bw() +
  geom_histogram(binwidth = 0.5, colour = "white", fill = "cornflowerblue", size = 0.1) +
  scale_x_continuous(breaks = seq(0, 15, 1)) + 
  xlab("Logistikverzug(Tag/en)") +
  ylab("Warenanzahl") +
  geom_text(aes(label=as.character(..count..)),stat="bin", binwidth = 1, vjust=-0.5) 

  
m=mean(Logistikverzug$Verzug)
#5.080437
sd=sd(Logistikverzug$Verzug)
#1.012302

ggplot(Logistikverzug) + 
  geom_histogram(aes(x = Verzug, y = ..density..),fill = "cornsilk", colour = "grey60",binwidth = 1) +
  geom_density(aes(colour="Real logistics delay distributed",x = Verzug, y = ..density..),adjust = 6,lwd=1) +
  stat_function(aes(colour="Normal distribution N(m,sd)"),fun = dnorm, args = list(m, sd), lwd=1)+
  scale_colour_manual("Groups", values = c("red", "blue"))+
  scale_x_continuous(breaks = seq(0, 15, 1)) + 
  ggtitle("Wahrscheinlichkeitverteilung") +
  xlab("Logistikverzug(Tag/en)") +
  ylab("Wahrscheinlichkeit")


#### Aufgabe b
Minimal_Logistikverzug <- min(Logistikverzug$Verzug)
Minimal_Logistikverzug
Maximal_Logistikverzug <- max(Logistikverzug$Verzug)
Maximal_Logistikverzug

#### Aufgabe c
Durchschnitt_Logistikverzug <- mean(Logistikverzug$Verzug)
Durchschnitt_Logistikverzug

## Aufgabe 2

## Aufgabe 3
Fahrzeug_file <- list.files(path = "Data/Fahrzeug/")
Fahrzeug_path <- file.path("Data/Fahrzeug", Fahrzeug_file)

Zulassungen_file <- list.files(path = "Data/Zulassungen/")
Zulassungen_path <- file.path("Data/Zulassungen", Zulassungen_file)

Bestandteile_Fahrzeuge_OEM2_Typ22 <- read_csv2(Fahrzeug_path[4], col_names = TRUE, col_types = cols())
Zulassungen_alle_Fahrzeuge <- read_csv2(Zulassungen_path[1], col_names = TRUE, col_types = cols())


str(Bestandteile_Fahrzeuge_OEM2_Typ22)
str(Zulassungen_alle_Fahrzeuge)

Zulassungen_alle_Fahrzeuge_New <- dplyr::rename(Zulassungen_alle_Fahrzeuge, "ID_Fahrzeug" = "IDNummer")

Gesamt_ID <- inner_join(Zulassungen_alle_Fahrzeuge_New, Bestandteile_Fahrzeuge_OEM2_Typ22, by = "ID_Fahrzeug")

ID_K7 <- table(Gesamt_ID$Gemeinden)
ID_K7[names(ID_K7) == "DORTMUND"]


## Aufgabe 4
str(Gesamt_ID)

## Aufgabe 5

## Aufgabe 6
Bestandteile_Fahrzeuge_OEM2_Typ21 <- read_csv2(Fahrzeug_path[3], col_names = TRUE, col_types = cols())

Gesucht_ID1 <- filter(Bestandteile_Fahrzeuge_OEM2_Typ21, ID_Motor == "K1BE2-104-1041-32050")
Gesucht_ID2 <- filter(Bestandteile_Fahrzeuge_OEM2_Typ22, ID_Motor == "K1BE2-104-1041-32050")
str(Gesucht_ID1)
str(Gesucht_ID2)

Unfall_ID <- merge(Zulassungen_alle_Fahrzeuge_New, Gesucht_ID1, by = "ID_Fahrzeug")
Unfall_ID[names(Unfall_ID) == "Gemeinden"]






