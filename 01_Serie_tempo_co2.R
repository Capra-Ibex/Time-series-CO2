##########################################
####
#### TITLE : Cariation CO2 Hawaï
####
#### Auteur(s) : Pablo raguet
####
#### Date de création : 2020-07-28
####  
#### Date de modification : 2020-07-31
####
#### Commentaires : commentaire
####
##########################################

##########################################
#### ESPACE DE TRAVAIL ####
##########################################
# dir <- "directory"
# setwd(dir)
getwd()

##########################################
#### PACKAGES ####
##########################################
library(tidyverse)
library(lubridate)
library(forecast)

##########################################
#### IMPORTATION DES DONNEES ####
##########################################
rm(list = ls())
hawai <- read_csv("Data/hawai.csv")

##########################################
##########################################
##########################################


##########################################
#### Exploration des données ####
##########################################
summary(hawai)
hawai$time # semble être en année décimales
hawai$CO2 # semble être en ppm

##########################################
#### Représentation ####
##########################################
### CO2 par année ####

hawai %>% 
  mutate(Time = date_decimal(time)) %>% 
  select(Time, CO2) %>% 
  ggplot(aes(x = Time, y = CO2)) +
  geom_line() +
  labs(x = "Année", y = expression(Concentration~en~CO[2]~(ppm))) +
  theme_light()

ggsave("Figures/CO2_an.pdf")

### Moyenne par mois de la teneur en CO2 ####

hawai %>% 
  mutate(Time = date_decimal(time)) %>% 
  select(Time, CO2) %>% 
  mutate(Month = Time %>% month()) %>% 
  group_by(Month) %>% 
  summarise(mean_month = mean(CO2, na.rm = T)) %>% 
  ggplot(aes(x = Month, y = mean_month)) +
  geom_line() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Mois (numéraire)", y = expression(Concentration~moyenne~en~CO[2]~(ppm))) +
  theme_light()

ggsave("Figures/CO2_mois.pdf")

### Transformation des données ####
## nombre de mesure par année ####

hawai %>% 
  mutate(Time = date_decimal(time)) %>% 
  select(Time, CO2) %>%  
  mutate(year = Time %>% year()) %>% 
  group_by(year) %>%
  summarise(length(Time))

## Transformation des données en format ts ####

hawai_ts <- ts(hawai %>% dplyr::select(-time),
               start = hawai$time[1],
               frequency = 12)
# il y a 12 observations (mois) par ans
# cependant il y a généralement 2 mesures en janvier
# la seconde compte pour février au vu de la date (31-01)

### Autocorrelation et tendances sur 1958-2001 ####

# autoplot(hawai_ts) +
#   labs(x = "Année", y = expression(Concentration~en~CO[2]~(ppm))) +
#   theme_light()
# la teneur en CO2 augmente avec les années

ggAcf(hawai_ts) +
  ggtitle("") +
  theme_light()
ggsave("Figures/CO2_auto.pdf")
# il y a une autocorelation significative entre les mesures

gglagplot(hawai_ts) +
  theme_light()
ggsave("Figures/CO2_lag.pdf", width = 5, height = 5)
# les points sont près de la diagonnale :
# il y a une forte tendance dans les mesures

## test de Ljung-Box ####
Box.test(hawai_ts, lag = 27, type = "Ljung-Box")
# les données ne proviennent probablement pas d'un bruit blanc

### Varaition temporelle de l'augmentation de CO2 ####

autoplot(diff(hawai_ts)) +
  theme_light()
ggsave("Figures/diff_CO2.pdf")

ggAcf(diff(hawai_ts))  +
  ggtitle("") +
  theme_light()
ggsave("Figures/diff_CO2_auto.pdf")
# il y a une autocorrelation des données
# avec un cycle de 12 mois

gglagplot(diff(hawai_ts)) +
  theme_light()
ggsave("Figures/diff_CO2_lag.pdf", width = 5, height = 5)
# la fluctuation est sinusoïdale

## test de Ljung-Box ####
Box.test(diff(hawai_ts), lag = 27, type = "Ljung-Box")
# les données ne proviennent probablement pas d'un bruit blanc

##########################################
#### modélisation prévisionnel ####
##########################################
###  selection des dates d'entrainement et de test (70/30 %) ####
lim <- round(length(hawai$time)*0.7, 0)
# détermination de la date vers 70% du jeu de donnée
# la sélection n'est pas aléatoire
# elle concerne la fin du jeu de données

## données d'entrainement
train <- hawai[1:lim, ]
train_hawai_ts <-ts(train %>% dplyr::select(-time),
                    start = train$time[1],
                    frequency = 12)
train_hawai_ts

## données de validation
test <- hawai[(lim+1):length(hawai$time), ]
test_hawai_ts <- ts(test %>% dplyr::select(-time),
                    start = test$time[1],
                    frequency = 12)
test_hawai_ts

### Modèles ####
## Méthode Naïve ####
hawai_naive <- naive(train_hawai_ts, h = 159)
autoplot(hawai_naive)+
  autolayer(fitted(hawai_naive)) +
  autolayer(test_hawai_ts, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Années", y = expression(Concentration~moyenne~en~CO[2]~(ppm)))
ggsave("Figures/naiv.pdf")

summary(hawai_naive)

checkresiduals(hawai_naive)

## Méthode SES ####
hawai_hw <- hw(train_hawai_ts, damped = F, h = 159,
              seasonal = "additive")
autoplot(hawai_hw) + 
  autolayer(fitted(hawai_hw)) +
  autolayer(test_hawai_ts, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Années", y = expression(Concentration~moyenne~en~CO[2]~(ppm)))
ggsave("Figures/ses.pdf")

summary(hawai_hw)

checkresiduals(hawai_hw)

## Méthode ETS (SES auto, même résultats) ####
hawai_ets <- ets(train_hawai_ts)

autoplot(hawai_ets)
ggsave("Figures/ets_components.pdf", width = 5, height = 5)

hawai_ets_f <- hawai_ets %>% forecast(h = 159)
autoplot(hawai_ets_f) + 
  autolayer(fitted(hawai_ets_f)) +
  autolayer(test_hawai_ts, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Années", y = expression(Concentration~moyenne~en~CO[2]~(ppm)))
ggsave("Figures/ets.pdf")

summary(hawai_ets_f)

checkresiduals(hawai_ets_f)

## Méthode ARIMA ####
hawai_arima <- train_hawai_ts %>% auto.arima()
hawai_arima_f <- hawai_arima %>% forecast(h = 159)

autoplot(hawai_arima_f) + 
  autolayer(fitted(hawai_arima_f)) +
  autolayer(test_hawai_ts, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Années", y = expression(Concentration~moyenne~en~CO[2]~(ppm)))
ggsave("Figures/arima.pdf")

summary(hawai_arima_f)

checkresiduals(hawai_arima_f)

## Méthode TBATS ####
hawai_ts_tbats <- train_hawai_ts %>% tbats()
hawai_ts_tbats_f <- hawai_ts_tbats %>% forecast(h = 159)

autoplot(hawai_ts_tbats_f) +
  autolayer(fitted(hawai_ts_tbats_f)) +
  autolayer(test_hawai_ts, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Années", y = expression(Concentration~moyenne~en~CO[2]~(ppm)))
ggsave("Figures/tabts.pdf")

summary(hawai_ts_tbats_f)

checkresiduals(hawai_ts_tbats_f)

##########################################