# Charger les bibliothèques nécessaires
library(tidyverse)    # Chargement des bibliothèques pour les opérations de manipulation de données
library(sf)           # Chargement de la bibliothèque pour les données géographiques
library(RColorBrewer) # Chargement de la bibliothèque pour les palettes de couleurs
library(tmap)         # Chargement de la bibliothèque pour la création de cartes
library(vroom)        # Chargement de la bibliothèque pour la lecture rapide des fichiers CSV
library(magrittr)     # Chargement de la bibliothèque pour les opérations de manipulation de données


# Définir le nom du fichier à lire
nom_fichier <- vroom("fleaux_de_la_societe.csv")

# Charger le fichier csv en tant que dataframe
df_delinquance <- nom_fichier

# Sélectionner les variables d'intérêt
df_delinquance_select <- df_delinquance %>%
  select(classe, annee, Code.département, Code.région, Code.région)

# Filtrer les données pour l'année et la classe de votre choix
AN <- 19
atteinte <- "Vols violents sans arme"
df_filtré <- df_delinquance_select %>%
  filter(annee == AN, classe == atteinte)

# Créer un répertoire "departements" s'il n'existe pas encore
if (!file.exists("C:/Users/maxim/Documents/New projet R/departements")) {
  dir.create("C:/Users/maxim/Documents/New projet R/departements")
}

# Charger la carte des départements de France
setwd("C:/Users/maxim/Documents/New projet R/departements")
departements <- st_read("departements/departements-20180101.shp")

# Fusionner les données de la délinquance avec les données des départements
departements_delinquance <- left_join(departements, df_filtré, by = c("code_insee" = "DEP"))

# Préparer les couleurs pour la carte
nb_classes <- 5
palette_couleurs <- brewer.pal(nb_classes, "Reds")
palette_couleurs <- rev(palette_couleurs)
couleur_nulle <- "#f7f7f7"
palettes_couleurs <- colorRampPalette(c(couleur_nulle, palette_couleurs))

# Définir les classes pour la carte
classes <- tmaptools::classify(departements_delinquance$ATTEINTE, method = "quantile", n = nb_classes)

# Dessiner la carte
tm_shape(departements_delinquance) +
  tm_polygons("ATTEINTE",
              style = "jenks",
              palette = palettes_couleurs(nb_classes),
              breaks = classes$brks,
              title = paste("Nombre de", atteinte, "en", annee),
              legend.hist = TRUE,
              legend.is.portrait = FALSE,
              border.col = "gray70") +
  tm_borders(col = "gray80") +
  tm_layout(title = "Carte départementale de la délinquance en France") +
  tm_compass(type = "arrow", position = c("right","bottom"))


