# projet de diagramme ombrothermique par année 10/25
# Axel PAJOT avec aide de forum et ChatGPT...

# install.packages(c("tidyverse", "lubridate"))
library(tidyverse)
library(lubridate)
library(ggplot2)

# Import des donnees
setwd("")     #selectionner le chemin d'acces a son fichier
data <- read.csv2("", dec = ".")   #selectionner son fichier

# Recherche des donnees de Noirmoutier
Stationmeteo <- data[which(data$NOM_USUEL== ""),]

annee_choisie <- 2022  # choie de l'année !   Pour choisir une période : mettre l'année minimale

meteo <- Stationmeteo[which(Stationmeteo$ANNEE == annee_choisie),]    # pour une période : changer == en > 
attach(meteo)     # permet de mettre le nom d'une colonne directement plutôt que tableau$colonne
# Conversion au format date si nécessaire
meteo$AAAAMMJJ <- as.Date(as.character(meteo$AAAAMMJJ), format = "%Y%m%d")

# Ajout des colonnes mois et année
meteo <- meteo %>%
  mutate(
    year = year(AAAAMMJJ),
    month = month(AAAAMMJJ, label = TRUE, abbr = TRUE)  # abrégé ex: Jan, Feb...
  )

# Agrégation mensuelle
meteo_mensuel <- meteo %>%
  group_by(year, month) %>%    # supprimer "year" pour faire un diagramme sur une période
  summarise(
    P = sum(RR, na.rm = TRUE),        # cumul mensuel des précipitations
    TN = mean(TN, na.rm = TRUE),        # moyenne mensuelle des températures
    TX = mean(TX, na.rm = TRUE),        # moyenne mensuelle des températures maximales
    TM = mean(TM,na.rm = TRUE)          # moyenne mensuelle des températures moyenne
  ) %>%
  ungroup()

# Passage en format long pour les températures
meteo_long <- meteo_mensuel %>%
  pivot_longer(cols = c(TN, TM, TX), names_to = "Type", values_to = "Temp") %>%
  mutate(Temp = Temp * 2)  # on multiplie ici pour l’échelle secondaire

# Diagramme avec ggplot2
ggplot() +
  geom_col(data = meteo_mensuel, aes(x = month, y = P), fill = "skyblue", width = 0.6) +
  geom_line(data = meteo_long, aes(x = month, y = Temp, color = Type, group = Type), linewidth = 1.2) +
  geom_point(data = meteo_long, aes(x = month, y = Temp, color = Type), size = 2) +
  scale_color_manual(
    name = "Températures",
    values = c("TN" = "yellow", "TM" = "orange", "TX" = "red"),
    labels = c("TN" = "T° Minimale", "TM" = "T° Moyenne", "TX" = "T° Maximale")
  ) +
  scale_y_continuous(
    name = "Précipitations mensuelles (mm)",
    sec.axis = sec_axis(~ . / 2, name = "Températures (°C)")
  ) +
  labs(
    title = str_glue("Diagramme ombrothermique de l'année {annee_choisie}"),
    x = "Mois"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    legend.position = "right"
  )
