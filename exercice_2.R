# Charger les bibliothèques nécessaires
library(tidyverse)   # Chargement des bibliothèques pour les opérations de manipulation de données
library(vroom)       # Chargement de la bibliothèque pour la lecture rapide des fichiers CSV
library(ggplot2)    # Chargement de la bibliothèque pour la création de graphiques


# Définir le nom du fichier à lire
nom_fichier <- "fleaux_de_la_societe.csv"

# Lire le fichier CSV
df_delinquance <- vroom(nom_fichier)

# Sélectionner les variables d'intérêt
code_commune <- "Code.département"
atteinte <- "classe"
annee <- "annee"

# Commune spécifique
code_commune_specifique <- "59"

# Filtrer les données pour la commune spécifique
df_commune_specifique <- df_delinquance %>%
  filter({{ code_commune }} == code_commune_specifique) %>%
  group_by({{ atteinte }}, {{ annee }}) %>%
  summarize(nombre_atteintes = n(), .groups = 'drop')

# Autre commune spécifique
code_autre_commune_specifique <- "62"

# Filtrer les données pour l'autre commune spécifique
df_autre_commune_specifique <- df_delinquance %>%
  filter({{ code_commune }} == code_autre_commune_specifique) %>%
  group_by({{ atteinte }}, {{ annee }}) %>%
  summarize(nombre_atteintes = n(), .groups = 'drop')

# Créer un graphique de l'évolution de la délinquance dans les deux communes spécifiques
ggplot() +
  geom_line(data = df_commune_specifique, aes(x = {{ annee }}, y = nombre_atteintes, color = "Délinquance dans la commune 59")) +
  geom_line(data = df_autre_commune_specifique, aes(x = {{ annee }}, y = nombre_atteintes, color = "Délinquance dans la commune 62")) +
  labs(title = "Évolution de la délinquance dans les communes 59 et 62",
       x = "Année",
       y = "Nombre d'atteintes") +
  scale_color_manual(values = c("Délinquance dans la commune 59" = "blue", "Délinquance dans la commune 62" = "red")) +
  theme_minimal()


