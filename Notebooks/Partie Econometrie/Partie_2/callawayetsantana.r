# 1. CHARGEMENT DES LIBRAIRIES
if (!require("did")) install.packages("did")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(did)
library(dplyr)
library(ggplot2)

# 2. FONCTION DE PRÉPARATION DES DONNÉES AVEC FILTRAGE INITIAL
prepare_data_cs <- function(path, election_type) {
  # Chargement des données brutes
  df_raw <- read.csv(path)
  
  # Définition des années cibles en premier
  if (election_type == "pres") {
    target_years <- c(1995, 2002, 2007, 2012, 2017, 2022)
  } else {
    target_years <- c(1997, 2002, 2007, 2012, 2017, 2022)
  }
  
  # 1. Calcul du seuil cliquet sur l'année 1998 (donnée de référence)
  seuil_df <- df_raw %>%
    filter(Annee == 1998) %>%
    mutate(seuil = total_equipements * 0.6) %>%
    select(codecommune, seuil)
  
  # 2. FILTRAGE DES ANNÉES EN PREMIER
  # On ne garde que les années électorales pour la suite des calculs
  df <- df_raw %>% 
    filter(Annee %in% target_years) %>%
    as.data.frame()
  
  # Identifiant numérique pour 'did'
  df$id_numeric <- as.numeric(as.factor(df$codecommune))
  
  # 3. Application de la logique de traitement sur les données filtrées
  df <- df %>%
    left_join(seuil_df, by = "codecommune") %>%
    mutate(is_treated_now = ifelse(total_equipements < seuil, 1, 0)) %>%
    arrange(codecommune, Annee) %>%
    group_by(codecommune) %>%
    mutate(traitée = cummax(is_treated_now)) %>% # Effet cliquet
    ungroup()
    
  # 4. Définition de l'année de premier traitement alignée sur les élections[cite: 1]
  first_treated_df <- df %>%
    filter(traitée == 1) %>%
    group_by(codecommune) %>%
    summarise(first_treated = min(Annee))
    
  df <- df %>%
    left_join(first_treated_df, by = "codecommune") %>%
    mutate(first_treated = ifelse(is.na(first_treated), 0, first_treated)) %>%
    ungroup()
    
  return(df)
}

# 3. FONCTION D'ESTIMATION
run_did_cs <- function(df, election, parti, data_name) {
  
  outcome_var <- paste0("vote_", parti, "_", election)
  
  # Liste des contrôles[cite: 1]
  ctrls <- c("pop", "propf", "prop014", "prop1539", "prop60p", "petranger", 
             "pouem", "pchom", "paind", "pbac", "psup", "revmoy")
  formula_ctrls <- as.formula(paste("~", paste(ctrls, collapse = " + ")))
  
  # Estimation ATT(g,t)[cite: 1]
  # On utilise id_numeric et first_treated calculés sur les années filtrées
  out <- att_gt(yname = outcome_var,
                tname = "Annee",
                idname = "id_numeric",
                gname = "first_treated",
                xformla = formula_ctrls,
                data = df,
                control_group = "notyettreated",
                est_method = "dr",
                clustervars = "id_numeric")
                
  # Agrégation Event Study pour les effets dynamiques[cite: 1]
  es <- aggte(out, type = "dynamic")
  
  # Affichage des résultats
  cat("\n--- Résultats C&S (Années filtrées) :", data_name, "|", outcome_var, "---\n")
  print(summary(es))
  
  # Génération du graphique
  p <- ggdid(es) + ggtitle(paste("C&S Event Study:", data_name, outcome_var))
  print(p)
  
  return(es)
}

# 4. CONFIGURATION ET EXÉCUTION
files <- list(
  rnp = "C:/Users/yancr/Documents/ENSAE_V2/STATAPP/V2/STATAPP_V2/Données/Partie Econometrie/communes_social_rnp.csv",
  rp  = "C:/Users/yancr/Documents/ENSAE_V2/STATAPP/V2/STATAPP_V2/Données/Partie Econometrie/communes_social_rp.csv",
  ui  = "C:/Users/yancr/Documents/ENSAE_V2/STATAPP/V2/STATAPP_V2/Données/Partie Econometrie/communes_social_ui.csv",
  ud  = "C:/Users/yancr/Documents/ENSAE_V2/STATAPP/V2/STATAPP_V2/Données/Partie Econometrie/communes_social_d.csv"
)

for (name in names(files)) {
  tryCatch({
    print(paste("Traitement du fichier :", files[[name]]))
    
    # 1. Traitement pour les Présidentielles[cite: 1]
    data_pres <- prepare_data_cs(files[[name]], "pres")
    run_did_cs(data_pres, "pres", "RN", name)
    run_did_cs(data_pres, "pres", "PS", name)
    
    # 2. Traitement pour les Législatives[cite: 1]
    data_leg <- prepare_data_cs(files[[name]], "leg")
    run_did_cs(data_leg, "leg", "RN", name)
    run_did_cs(data_leg, "leg", "PS", name)
    
  }, error = function(e) {
    message(paste("Erreur lors du traitement de", name, ":", e$message))
  })
}