# ============================================================
# INSTALLATION DES PACKAGES SI NÉCESSAIRE
# ============================================================
install_if_missing <- function(pkgs){
  to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(to_install)) install.packages(to_install)
}

install_if_missing(c("readxl", "tidyverse", "rstatix", "car", "ggpubr"))

library(readxl)
library(tidyverse)
library(rstatix)
library(car)
library(ggpubr)

# ============================================================
# IMPORTATION DES DONNÉES
# ============================================================

data <- read_excel("batch1_5espèces.xlsx")

# Vérification
print("Aperçu des données :")
print(head(data))

# Nettoyage noms de colonnes (sécurité)
names(data) <- names(data) %>%
  str_replace_all("\\s+", "_") %>%
  str_replace_all("[^0-9A-Za-z_]", "")

# Forcer Neighbour en minuscules (kin / stranger)
data$Neighbour <- tolower(data$Neighbour)

# ============================================================
# FONCTION D’ANALYSE POUR 1 VARIABLE DANS 1 ESPÈCE
# ============================================================

analyse_species_var <- function(data, species, var){
  
  cat("\n---------------------------------------------------------\n")
  cat("Espèce :", species, " | Variable :", var, "\n")
  cat("---------------------------------------------------------\n")
  
  d <- data %>%
    filter(ID_Focal == species) %>%
    filter(!is.na(.data[[var]]))
  
  if(length(unique(d$Neighbour)) < 2){
    cat("❌ Pas assez de groupes (kin / stranger) pour analyser.\n")
    return(NULL)
  }
  
  # -------------------------
  # Modèle linéaire
  # -------------------------
  mod <- lm(as.formula(paste(var, "~ Neighbour")), data=d)
  
  # -------------------------
  # SHAPIRO TEST (ultra sécurisé)
  # -------------------------
  shapiro_res <- tryCatch(
    shapiro_test(residuals(mod)),
    error = function(e) NULL
  )
  
  # Valeur par défaut
  normal_ok <- FALSE
  
  if (!is.null(shapiro_res)) {
    # vérifier si la colonne p existe et est numérique
    if ("p" %in% names(shapiro_res) &&
        length(shapiro_res$p) == 1 &&
        is.numeric(shapiro_res$p) &&
        !is.na(shapiro_res$p)) {
      
      print(shapiro_res)
      normal_ok <- shapiro_res$p > 0.05
      
    } else {
      cat("⚠ Shapiro a échoué ou p-value invalide\n")
    }
  } else {
    cat("⚠ Shapiro test impossible (NULL)\n")
  }
  
  # -------------------------
  # LEVENE TEST (sécurisé)
  # -------------------------
  levene_raw <- tryCatch(
    leveneTest(as.formula(paste(var, "~ Neighbour")), data=d),
    error = function(e) NULL
  )
  
  var_equal <- FALSE
  
  if (!is.null(levene_raw)) {
    print(levene_raw)
    
    # extraire la p-value en évitant erreurs
    p_lev <- tryCatch(
      levene_raw$`Pr(>F)`[1],
      error = function(e) NA
    )
    
    if (!is.na(p_lev)) {
      var_equal <- p_lev > 0.05
    } else {
      cat("⚠ Levene p-value invalide\n")
    }
    
  } else {
    cat("⚠ Levene test impossible\n")
  }
  
  # -------------------------
  # SÉLECTION DU TEST
  # -------------------------
  if (isTRUE(normal_ok) && isTRUE(var_equal)) {
    cat("\n✔ ANOVA\n")
    print(anova(mod))
  } else {
    cat("\n⚠ Test non paramétrique (Wilcoxon)\n")
    print(wilcox.test(as.formula(paste(var, "~ Neighbour")), data=d))
  }
  
  # -------------------------
  # FOLD CHANGE
  # -------------------------
  means <- d %>%
    group_by(Neighbour) %>%
    summarise(m = mean(.data[[var]], na.rm=TRUE))
  
  if(all(c("kin","stranger") %in% means$Neighbour)){
    FC <- means$m[means$Neighbour=="stranger"] /
      means$m[means$Neighbour=="kin"]
    cat("\nFold change Stranger/Kin =", FC, "\n")
  } else {
    cat("\n⚠ Impossible de calculer le fold change (groupes incomplets)\n")
  }
}

# ============================================================
# ANALYSES POUR TOUTES LES ESPÈCES ET VARIABLES
# ============================================================

variables <- c("Length_Focal", "Tillers", "NBI")
species_list <- unique(data$ID_Focal)

for(sp in species_list){
  cat("\n==========================================\n")
  cat("       Analyse pour l'espèce :", sp, "\n")
  cat("==========================================\n")
  
  for(v in variables){
    analyse_species_var(data, sp, v)
  }
}

# ============================================================
# FONCTION STATISTIQUE (pour annoter les graphiques)
# ============================================================

compute_stats <- function(df, variable){
  
  kin  <- df %>% filter(Neighbour == "kin") %>% pull(variable)
  stranger <- df %>% filter(Neighbour == "stranger") %>% pull(variable)
  
  if(length(kin) < 2 | length(stranger) < 2){
    return(list(pval=NA, fold=NA))
  }
  
  # Wilcoxon (toujours pour les graphiques)
  wtest <- wilcox.test(kin, stranger, exact = FALSE)
  
  fold <- mean(stranger, na.rm=TRUE) / mean(kin, na.rm=TRUE)
  
  return(list(
    pval = wtest$p.value,
    fold = fold
  ))
}

# ============================================================
# FONCTION GRAPHIQUE
# ============================================================

plot_species_var <- function(data, species, variable){
  
  df <- data %>% filter(ID_Focal == species)
  stats <- compute_stats(df, variable)
  
  ggplot(df, aes(x = Neighbour, y = .data[[variable]], fill = Neighbour)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.1) +
    theme_bw() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = paste("Specie", species, "-", variable),
      subtitle = paste0(
        "Wilcoxon p = ", signif(stats$pval, 3),
        " | Fold change = ", round(stats$fold, 2)
      ),
      x = "Neighbour",
      y = variable
    )
}

# ============================================================
# GÉNÉRATION DES 5 FIGURES
# ============================================================

for(sp in species_list){
  
  df_sp <- data %>% filter(ID_Focal == sp)
  
  p1 <- plot_species_var(data, sp, "Length_Focal")
  p2 <- plot_species_var(data, sp, "Tillers")
  p3 <- plot_species_var(data, sp, "NBI")
  
  fig <- ggarrange(p1, p2, p3, ncol=3, nrow=1,
                   common.legend=TRUE, legend="bottom")
  
  ggsave(filename = paste0("fig_species_", sp, ".png"),
         plot = fig, width = 12, height = 4, dpi = 300)
  
  print(paste("Figure sauvegardée :", paste0("fig_species_", sp, ".png")))
}
