
library(ggplot2)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(lubridate)
library(stringr)

#####Przyjęcie i obróbka danych ogólnych

ZiolaOgol <- read.csv("D://Pulpit//Pogoda//ZiolaCalyOgol.csv")
ZiolaOgol$Data <- as.Date.character(ZiolaOgol$Data, tryFormats = c("%d.%m.%Y"))
ZiolaOgol$Kilo <- chartr(",", ".", ZiolaOgol$Kilo)
ZiolaOgol$Kilo <- as.numeric(ZiolaOgol$Kilo)
ZiolaOgol <- ZiolaOgol |>
  group_by(Data) |> 
  summarise(Kilo = sum(Kilo, na.rm=T))
ZiolaOgol <- ZiolaOgol |> 
  mutate(Rok = year(Data),
         Miesiac = month(Data),
         Dzien = day(Data))

######Przyjęcie i obróbka danych szczególnych

ZiolaSzczeg <- read.csv("D://Pulpit//Pogoda//ZiolaCalySzczegol.csv")
ZiolaSzczeg$Rok <- substr(ZiolaSzczeg$Kod, 2, 3)
ZiolaSzczeg$Miesiac <- substr(ZiolaSzczeg$Kod, 4, 5)
ZiolaSzczeg$Dzien <- substr(ZiolaSzczeg$Kod, 6, 7)
ZiolaSzczeg$Kod <- substr(ZiolaSzczeg$Kod, 2, 7)

ZiolaSzczeg$Waga <- chartr(",", ".", ZiolaSzczeg$Waga)
ZiolaSzczeg$Waga_Pz <- chartr(",", ".", ZiolaSzczeg$Waga_Pz)
ZiolaSzczeg$Wilg <- chartr(",", ".", ZiolaSzczeg$Wilg)
ZiolaSzczeg$Piach <- chartr(",", ".", ZiolaSzczeg$Piach)
ZiolaSzczeg$Olej <- chartr(",", ".", ZiolaSzczeg$Olej)

ZiolaSzczeg$Waga <- as.numeric(ZiolaSzczeg$Waga)
ZiolaSzczeg$Waga_Pz <- as.numeric(ZiolaSzczeg$Waga_Pz)
ZiolaSzczeg$Wilg <- as.numeric(ZiolaSzczeg$Wilg)
ZiolaSzczeg$Piach <- as.numeric(ZiolaSzczeg$Piach)
ZiolaSzczeg$Olej <- as.numeric(ZiolaSzczeg$Olej)
ZiolaSzczeg$Rok <- as.numeric(ZiolaSzczeg$Rok)
ZiolaSzczeg$Miesiac <- as.numeric(ZiolaSzczeg$Miesiac)
ZiolaSzczeg$Dzien <- as.numeric(ZiolaSzczeg$Dzien)

ZiolaSzczeg <- ZiolaSzczeg |>
  group_by(Kod) |> 
  summarise(Waga = sum(Waga, na.rm=T),
            Waga_Pz = sum(Waga_Pz, na.rm=T),
            Wilg = mean(Wilg, na.rm=T),
            Piach = mean(Piach, na.rm=T),
            Olej = mean(Olej, na.rm=T),
            Rok = mean(Rok, na.rm=T),
            Miesiac = mean(Miesiac, na.rm=T),
            Dzien = mean(Dzien, na.rm=T),
            DataKod = paste(Rok, str_pad(Miesiac, 2, pad = "0"), sep = "-"))
ZiolaSzczeg$DataKod <- paste0("20", ZiolaSzczeg$DataKod)

############Wypełnienie wartości 0 i null

SredWilg <- mean(ZiolaSzczeg$Wilg, na.rm = TRUE)
SredPiach <- mean(ZiolaSzczeg$Piach, na.rm = TRUE)
SredOlej <- mean(ZiolaSzczeg$Olej, na.rm = TRUE)

ZiolaSzczeg$Wilg[is.na(ZiolaSzczeg$Wilg)] <- SredWilg
ZiolaSzczeg$Piach[is.na(ZiolaSzczeg$Piach)] <- SredPiach
ZiolaSzczeg$Olej[is.na(ZiolaSzczeg$Olej)] <- SredOlej
ZiolaSzczeg$Waga_Pz[ZiolaSzczeg$Waga_Pz == 0] <- ZiolaSzczeg$Waga

###################

  ggplot(ZiolaSzczeg[ZiolaSzczeg$Rok == 20,], aes(x=Kod, y=Waga)) + 
  geom_bar(stat = "identity")

##################Ogarniecie danych rzekowych

Wieprz <- read.csv("D://Pulpit//Pogoda//Wieprz_Combo.csv")
Wieprz <- Wieprz %>%
  filter(Ekst == 2) %>%
  mutate(DataKod = paste(Rok, str_pad(Mies_Kalendarz, 2, pad = "0"), sep = "-")) %>%
  select(-Ekst, -Rzeka)


###################Ogarniecie danych pogodowych

Pogoda <- read.csv("D://Pulpit//Pogoda//Pogoda_Combo.csv")

Pogoda <- Pogoda |>
  mutate(DataKod = paste(Rok, str_pad(Mies, 2, pad = "0"), sep = "-"))

Pogoda <- Pogoda |>
  group_by(DataKod) |> 
  summarise(Rok = mean(Rok, na.rm=T),
            Mies = mean(Mies, na.rm=T),
            Sred_Temp = mean(Sred_Temp, na.rm=T),
            Abs_Temp_Max = max(Abs_Temp_Max, na.rm=T),
            Srd_Temp_Max = max(Srd_Temp_Max, na.rm=T),
            Abs_Temp_Min = min(Abs_Temp_Min, na.rm=T),
            Srd_Temp_Min = min(Sred_Temp_Min, na.rm=T),
            Min_Temp_Grunt = min(Min_Temp_Grunt, na.rm=T),
            Srd_Wiatr = mean(Wiatr, na.rm=T),
            Srd_Wilgot = mean(Sred_Wilgot, na.rm=T),
            Srd_Cisnienie_Pary = mean(Sred_Cisnienie_Pary, na.rm=T),
            Srd_Cisn = mean(Sred_Cisn, na.rm=T),
            Srd_Cisn_PzMorza = mean(Sred_Cisn_PoziomMorza, na.rm=T),
            Suma_Slonce = mean(Suma_Slonce, na.rm=T),
            Zachmurz = mean(Zachmurz, na.rm=T),
            Suma_Opad = mean(Suma_Opad, na.rm=T),
            Maks_Dob_Sum_Opad = max(Maks_Dob_Sum_Opad, na.rm=T),
            Suma_Opadu_Dzien = mean(Suma_Opadu_Dzien, na.rm=T),
            Suma_Opadu_Noc = mean(Suma_Opadu_Noc, na.rm=T),
            Maks_Pokryw_Snieg = mean(Maks_Pokryw_Snieg, na.rm=T),
            Dni_z_Snieg = mean(Dni_z_Snieg, na.rm=T),
            Dni_Z_Deszcz = mean(Dni_Z_Deszcz, na.rm=T),
            Dni_DeszczISnieg = mean(Dni_DeszczISnieg, na.rm=T),
            Dni_Grad = mean(Dni_Grad, na.rm=T),
            Dni_Mgla = mean(Dni_Mgla, na.rm=T),
            Dni_Zamglen = mean(Dni_Zamglen, na.rm=T),
            Dni_Sadz = mean(Dni_Sadz, na.rm=T),
            Dni_Gololedz = mean(Dni_Gololedz, na.rm=T),
            Dni_Zamiec_Niska = mean(Dni_Zamiec_Niska, na.rm=T),
            Dni_Zamiec_Wysoka = mean(Dni_Zamiec_Wysoka, na.rm=T),
            Dni_Zmetnienie = mean(Dni_Zmetnienie, na.rm=T),
            Dni_Wiatr = mean(Dni_Wiatr, na.rm=T),
            Dni_Big_Waitr = mean(Dni_Big_Waitr, na.rm=T),
            Dni_Burza = mean(Dni_Burza, na.rm=T),
            Dni_Rosa = mean(Dni_Rosa, na.rm=T),
            Dni_Szron = mean(Dni_Szron, na.rm=T))

########Tworzenie Main tablicy

Main <- left_join(
  ZiolaSzczeg,
  Pogoda %>% select(-Rok, -Mies),
  by = "DataKod"
)
Main <- left_join(
  Main,
  Wieprz %>% select(-Rok, -Mies_Kalendarz),
  by = "DataKod"
)

#########################Testowanierzeczy idk

rm(ZiolaSzczeg_WRK, ZiolaSzczeg_WRK2)

ZiolaSzczeg_WRK <- ZiolaSzczeg |>
  mutate(DataKod = paste(Rok, str_pad(Miesiac, 2, pad = "0"), sep = "-"))
ZiolaSzczeg_WRK$DataKod <- paste0("20", ZiolaSzczeg_WRK$DataKod)

ZiolaSzczeg_WRK <- left_join(
  ZiolaSzczeg_WRK,
  Pogoda %>% select(DataKod, Min_Temp_Grunt),
  by = "DataKod"
)



ZiolaSzczeg_WRK2 <- ZiolaSzczeg_WRK %>%
  filter() %>%
  group_by(DataKod) %>%
  summarise(Wilg = mean(Wilg, na.rm = TRUE)-4,
            Piach = mean(Piach),
            Olej = mean(Olej, na.rm = TRUE),
            Min_Temp_Grunt = min(Min_Temp_Grunt)) %>%
  select(DataKod, Olej, Wilg) %>%
  pivot_longer(
    cols = c(Olej, Wilg),
    names_to = "Zmienna",
    values_to = "Wartosc"
  )

ZiolaSzczeg_WRK2 %>%
ggplot( aes(x = DataKod, y = Wartosc, color = Zmienna, group = Zmienna)) +
  geom_line() +
  geom_point() +
  theme_minimal()

###################

MainWRK <- Main %>%
  filter(as.numeric(Kod) > 200600, as.numeric(Kod) < 201000) %>%
  group_by(DataKod) %>%
  summarise(Wilg = (mean(Wilg, na.rm = TRUE)*10)-60,
            Min_Temp_Grunt = min(Min_Temp_Grunt, na.rm = TRUE)) %>%
  select(DataKod, Wilg, Min_Temp_Grunt) %>%
  pivot_longer(
    cols = c(Wilg, Min_Temp_Grunt),
    names_to = "Zmienna",
    values_to = "Wartosc"
  ) 

MainWRK %>%
  ggplot( aes(x = DataKod, y = Wartosc, color = Zmienna, group = Zmienna)) +
  geom_line() +
  geom_point() +
  theme_minimal()



Main %>%
  ggplot( aes(x = DataKod, y = Wartosc, color = Zmienna, group = Zmienna)) +
  geom_line() +
  geom_point() +
  theme_minimal()

Main2 <- Main %>%
  filter(Rok == 22) %>%
  group_by(DataKod) %>%
    summarize(Waga = sum(Waga))
Main2 %>%
  ggplot(aes(x = DataKod, y = Waga, group = 1))+
  geom_line()+
  geom_vline(xintercept=4, color = "green")+
  geom_vline(xintercept=9, color = "orange")


ggplot(ZiolaSzczeg_WRK2, aes(x=Zmienna, y=Wartosc)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

ggplot(ZiolaSzczeg_WRK2, aes(x=Zmienna, y=Wartosc, fill=Zmienna)) + 
  geom_violin()

quantile()
################################################################################################
################################################################################################
################################Rzeczy do juz finalu ig
####Stan wody w najbliższej mierzonej rzece nie ma zauważalnego wpływu na uprawę tymianku

ggplot(Main, aes(x=Stan_Wody, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()

ggplot(Main, aes(x=Stan_Wody, y=Waga)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#cos jest ale mozna zalozyc ze to przez deszcz

####Średnia wilgotność ma wpływ na wilgoć mierzoną w tymianku oraz na ilość olejków
ggplot(Main, aes(x=Srd_Wilgot, y=Olej)) + 
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()

ggplot(Main, aes(x=Srd_Wilgot, y=Wilg)) +
  geom_smooth(method=lm , color="red", se=FALSE) +
  geom_point()
#czemu wilgoc spada gdy wilgotnosc rosnie? moze dlatego ze miara wilgoci tymianku jest po suszeniu i ta miara jest mierna do tego zestawienia czy cos

