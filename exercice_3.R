
              
# Charger les bibliothèques nécessaires
library(tidyverse)   # Pour les opérations de manipulation de données
library(vroom)       # Pour lire le fichier CSV de manière rapide et efficace
library(ggplot2)    # Pour créer des graphiques

# Définir le nom du fichier à lire
nom_fichier <- "fleaux_de_la_societe.csv"

# Lire le fichier CSV contenant les données de délinquance
df_delinquance <- vroom(nom_fichier)

# Charger la table de passage de l'Insee pour la conversion de la géographie
table_passage <- vroom("Données_communes.csv")

# Année spécifique à convertir
annee_specifique <- "22"

# Fonction pour convertir la géographie d'une année spécifique
convertir_geographie <- function(annee) {
  # Filtrer les données pour l'année spécifique
  df_annee <- df_delinquance %>%
    filter(annee == annee_specifique)
  
  # Joindre avec la table de passage pour obtenir les nouvelles géographies
  df_converted <- df_annee %>%
    left_join(table_passage, by = c("Code.département" = "Code INSEE"))
  
  # Réorganiser les colonnes pour les données converties
  df_converted <- df_converted %>%
    select(Nom.de.la.commune, Code.département, POP, LOG, POP_2023, LOG_2023)
  
  return(df_converted)
}

# Convertir les données pour chaque année disponible dans df_delinquance
annees_disponibles <- unique(df_delinquance$annee)
df_converted <- purrr::map_dfr(annees_disponibles, convertir_geographie)

# Contrôles de cohérence avant et après le changement de géographie
nb_communes_avant <- nrow(df_delinquance)
nb_communes_apres <- nrow(df_converted)

total_population_avant <- sum(df_delinquance$POP)
total_population_apres <- sum(df_converted$POP_2023)

total_logements_avant <- sum(df_delinquance$LOG)
total_logements_apres <- sum(df_converted$LOG_2023)

# Affichage des résultats des contrôles de cohérence
cat("Contrôles de cohérence avant et après le changement de géographie :\n")
cat("Nombre de communes avant :", nb_communes_avant, "\n")
cat("Nombre de communes après :", nb_communes_apres, "\n\n")

cat("Totaux départementaux de la population avant :", total_population_avant, "\n")
cat("Totaux départementaux de la population après :", total_population_apres, "\n\n")

cat("Totaux départementaux des logements avant :", total_logements_avant, "\n")
cat("Totaux départementaux des logements après :", total_logements_apres, "\n")

# Visualisation des données converties avec un graphique
ggplot(df_converted, aes(x = POP_2023, y = LOG_2023)) +
  geom_point() +
  labs(title = "Conversion de la géographie communale 2022 vers 2023",
       x = "POP 2023",
       y = "LOG 2023") +
  theme_minimal()
