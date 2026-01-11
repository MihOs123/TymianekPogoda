library(ggplot2)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(lubridate)
library(stringr)
library(ggridges)
options(scipen=999)

###Obczajanie zbioru ziół
Ziola <- read.csv("C://Users//Iza//OneDrive//Pulpit//Pogoda//Ziola.csv")

#ggplot(Ziola, aes(x=Waga, y=Olej)) +
#  geom_point()
#ggplot(Ziola, aes(x=Waga, y=Wilg)) +
#  geom_point()
#ggplot(Ziola, aes(x=Wilg, y=Olej)) +
#  geom_point()
#ggplot(Ziola, aes(x=Miesiac))+
#  geom_histogram()

Ziola %>%
  group_by(Miesiac) %>%
  summarise(Laczna_Waga = sum(Waga)) %>%
  select(Miesiac, Laczna_Waga)

#ggplot(Ziola, aes(x=Miesiac, y=Olej)) +
#  geom_point()

count(Ziola, Miesiac)
#8, moze tez i 7 miesiac ma malo obserwacji wiec nie beda uwzgledniane w analizie
Ziola_Szczuple <- Ziola %>%
  filter(Miesiac %in% c(1,2,3,4,5,6,9,10,11,12))
#Zmorfuje razem też dane ziołowe w każdym miesiącu bo i tak dane pogodowe są per miesiąc
Ziola_Szczuple <- Ziola_Szczuple %>%
  group_by(DataKod) %>%
  summarise(Rok = mean(Rok),
            Miesiac = mean(Miesiac),
            Rok_Upraw = max(Rok_Prod),
            Waga = sum(Waga),
            Wilg = mean(Wilg),
            Olej = mean(Olej))

#######################################################################################

###Obrobienie zbioru pogody
#Dane rzekowe
Wieprz <- read.csv("C://Users//Iza//OneDrive//Pulpit//Pogoda//Wieprz_Combo.csv")
Wieprz <- Wieprz %>%
  filter(Ekst == 2) %>%
  mutate(DataKod = paste(Rok, str_pad(Mies_Kalendarz, 2, pad = "0"), sep = "-")) %>%
  select(-Ekst, -Rzeka)

#Dane pogodowe
Pogoda <- read.csv("C://Users//Iza//OneDrive//Pulpit//Pogoda//Pogoda_Combo.csv")

Pogoda <- Pogoda |>
  mutate(DataKod = paste(Rok, str_pad(Mies, 2, pad = "0"), sep = "-"))

Pogoda <- Pogoda %>%
  group_by(DataKod) %>% 
  summarise(Rok = mean(Rok, na.rm=T),
            Mies = mean(Mies, na.rm=T),
            Sred_Temp = mean(Sred_Temp, na.rm=T),
            Max_Temp = mean(Abs_Temp_Max, na.rm=T),
            Min_Temp = mean(Abs_Temp_Min, na.rm=T),
            Min_Temp_Grunt = mean(Min_Temp_Grunt, na.rm=T),
            Srd_Wiatr = mean(Wiatr, na.rm=T),
            Srd_Wilgot = mean(Sred_Wilgot, na.rm=T),
            Srd_Cisn = mean(Sred_Cisn, na.rm=T),
            Suma_Slonce = mean(Suma_Slonce, na.rm=T),
            Zachmurz = mean(Zachmurz, na.rm=T),
            Suma_Opad = mean(Suma_Opad, na.rm=T),
            Maks_Dob_Sum_Opad = mean(Maks_Dob_Sum_Opad, na.rm=T),
            Suma_Opadu_Dzien = mean(Suma_Opadu_Dzien, na.rm=T),
            Suma_Opadu_Noc = mean(Suma_Opadu_Noc, na.rm=T),
            Dni_Z_Deszcz = mean(Dni_Z_Deszcz, na.rm=T),
            Dni_Wiatr = mean(Dni_Wiatr, na.rm=T),
            Dni_Rosa = mean(Dni_Rosa, na.rm=T))

#Połączenie danych
Main <- left_join(
  Ziola_Szczuple,
  Pogoda %>% select(-Rok, -Mies),
  by = "DataKod"
)
Main <- left_join(
  Main,
  Wieprz %>% select(-Rok, -Mies_Kalendarz),
  by = "DataKod"
)
#Trza pamiętac o tym że część danych została wypełniona średnią.
#Średni olej = 2.2057
#Średnia wilgoć = 6.2719

#Dane żeby były ładnie pokazane w miesiacach wzrostu i zbiorow
Pogoda_Rosnie <- Main %>%
  filter(Miesiac %in% c(5, 6, 7, 8)) %>%
  group_by(Rok) %>%
  summarise(Rok_Upraw = mean(Rok),
            Sred_Temp = mean(Sred_Temp, na.rm=T),
            Max_Temp = max(Max_Temp, na.rm=T),
            Min_Temp = min(Min_Temp, na.rm=T),
            Min_Temp_Grunt = min(Min_Temp_Grunt, na.rm=T),
            Srd_Wiatr = mean(Srd_Wiatr, na.rm=T),
            Srd_Wilgot = mean(Srd_Wilgot, na.rm=T),
            Srd_Cisn = mean(Srd_Cisn, na.rm=T),
            Suma_Slonce = sum(Suma_Slonce, na.rm=T),
            Zachmurz = sum(Zachmurz, na.rm=T),
            Suma_Opad = sum(Suma_Opad, na.rm=T),
            Maks_Dob_Sum_Opad = max(Maks_Dob_Sum_Opad, na.rm=T),
            Suma_Opadu_Dzien = sum(Suma_Opadu_Dzien, na.rm=T),
            Suma_Opadu_Noc = sum(Suma_Opadu_Noc, na.rm=T),
            Dni_Z_Deszcz = sum(Dni_Z_Deszcz, na.rm=T),
            Dni_Wiatr = sum(Dni_Wiatr, na.rm=T),
            Dni_Rosa = sum(Dni_Rosa, na.rm=T)) %>%
  select(-Rok)
Main_GdyRosnie <- left_join(Ziola_Szczuple, Pogoda_Rosnie, by = "Rok_Upraw")
Main_GdyRosnie <- Main_GdyRosnie %>%
  filter(Rok_Upraw %in% c(19,20,21,22,23))

Pogoda_Zbiory <- Main %>%
  filter(Miesiac %in% c(9, 10, 11)) %>%
  group_by(Rok) %>%
  summarise(Rok_Upraw = mean(Rok),
            Sred_Temp = mean(Sred_Temp, na.rm=T),
            Max_Temp = max(Max_Temp, na.rm=T),
            Min_Temp = min(Min_Temp, na.rm=T),
            Min_Temp_Grunt = min(Min_Temp_Grunt, na.rm=T),
            Srd_Wiatr = mean(Srd_Wiatr, na.rm=T),
            Srd_Wilgot = mean(Srd_Wilgot, na.rm=T),
            Srd_Cisn = mean(Srd_Cisn, na.rm=T),
            Suma_Slonce = sum(Suma_Slonce, na.rm=T),
            Zachmurz = sum(Zachmurz, na.rm=T),
            Suma_Opad = sum(Suma_Opad, na.rm=T),
            Maks_Dob_Sum_Opad = max(Maks_Dob_Sum_Opad, na.rm=T),
            Suma_Opadu_Dzien = sum(Suma_Opadu_Dzien, na.rm=T),
            Suma_Opadu_Noc = sum(Suma_Opadu_Noc, na.rm=T),
            Dni_Z_Deszcz = sum(Dni_Z_Deszcz, na.rm=T),
            Dni_Wiatr = sum(Dni_Wiatr, na.rm=T),
            Dni_Rosa = sum(Dni_Rosa, na.rm=T)) %>%
  select(-Rok)
Main_GdyZbiory <- left_join(Ziola_Szczuple, Pogoda_Zbiory, by = "Rok_Upraw")
Main_GdyZbiory <- Main_GdyZbiory %>%
  filter(Rok_Upraw %in% c(19,20,21,22,23))
rm(Pogoda_Rosnie, Pogoda_Zbiory, Wieprz, Ziola, Pogoda, Ziola_Szczuple)

###################################################################
######### ANALIZA ANALIZA ANALIZA ANALIZA ANALIZA ANALIZA #########
###################################################################
###H1 Stan wody w najbliższej mierzonej rzece nie ma zauważalnego wpływu na tymianek
ggplot(Main, aes(x=Stan_Wody, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()

ggplot(Main, aes(x=Stan_Wody, y=Wilg)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#Jak widać coś tu sie dzieje, ale raczej jest to przez wspolmy czynnik - opady
#######################################################################################
###H2 Średnia wilgotność w miesiącu przyjęcia ziół ma wpływ na wilgoć mierzoną w tymianku oraz na ilość olejków
ggplot(Main, aes(x=Srd_Wilgot, y=Wilg)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#Im mokrzejszy tymianek tym bardziej się go suszy (wilgotność tymianku podana jest po wysuszeniu)

ggplot(Main, aes(x=Srd_Wilgot, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H3 Opady w miesiącach zbiorów tymianku negatywnie wpływają na jego jakość
ggplot(Main_GdyZbiory, aes(x=Suma_Opad, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H4 Opady w miesiącach wzrostu tymianku pozytywnie wpływają na jego ilość
ggplot(Main_GdyRosnie, aes(x=Suma_Opad, y=Waga)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H1 Minimalna temperatura gruntu w miesiącach wzrostu tymianku ma wpływ na jego ilość w okresie zbiorów.
ggplot(Main_GdyRosnie, aes(x=Min_Temp_Grunt, y=Waga)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H2 Ilość dni z zaobserwowaną rosą w miesiącach wzrostu tymianku ma wpływ na jego ilość w okresie zbiorów.
Main_GdyRosnie %>%
  filter(Dni_Rosa != 0) %>%
ggplot(aes(x=Dni_Rosa, y=Waga)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#Mało danych co do dni z rosa smh
#######################################################################################
###H1 Duże nasłonecznienie w miesiącach zbiorów tymianku pozytywnie wpływają na jego jakość.
Main_GdyZbiory %>%
  filter(Suma_Slonce != 0) %>%
ggplot(aes(x=Suma_Slonce, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H2 Duże nasłonecznienie w miesiącach sadzenia tymianku negatywnie wpływają na jego ilość podczas zbiorów.
Main_GdyRosnie %>%
  filter(Suma_Slonce != 0) %>%
  ggplot(aes(x=Suma_Slonce, y=Waga)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H3 Ciśnienie atmosferyczne nie wpływa znacznie na przyjmowany tymianek.
ggplot(Main, aes(x=Srd_Cisn, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
ggplot(Main_GdyRosnie, aes(x=Srd_Cisn, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
ggplot(Main_GdyZbiory, aes(x=Srd_Cisn, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################
###H4 Prędkość wiatru nie wpływa znacznie na przyjmowany tymianek.
ggplot(Main, aes(x=Srd_Wiatr, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
ggplot(Main_GdyRosnie, aes(x=Srd_Wiatr, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
ggplot(Main_GdyZbiory, aes(x=Srd_Wiatr, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#######################################################################################

