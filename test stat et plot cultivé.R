### --- Packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggpubr)

### --- Import des données ---
df <- read_excel("Batch_2NA.xlsx")

### --- Détection correcte des colonnes --- 
id_cols <- c("ID_Focal","ID_focal","ID focal","ID", "ID_FOCAL")
id_col <- names(df)[tolower(names(df)) %in% tolower(id_cols)]
if(length(id_col) == 0) stop("❌ Impossible de trouver la colonne ID_focal dans le fichier.")
id_col <- id_col[1]
df <- df %>% rename(ID_focal = all_of(id_col))

neigh_cols <- c("Neighbour","neighbour","Neighbor","neighbor","Treatment")
neigh_col <- names(df)[tolower(names(df)) %in% tolower(neigh_cols)]
if(length(neigh_col) == 0) stop("❌ Impossible de trouver la colonne Neighbour.")
neigh_col <- neigh_col[1]
df <- df %>% rename(Neighbour = all_of(neigh_col))

### --- Nettoyage ---
df$ID_focal  <- trimws(as.character(df$ID_focal))
df$Neighbour <- tolower(trimws(as.character(df$Neighbour)))

### --- Normalisation des labels neighbour ---
df$Neighbour <- case_when(
  grepl("kin", df$Neighbour) ~ "kin",
  grepl("stranger1", df$Neighbour) ~ "stranger1",
  grepl("stranger2", df$Neighbour) ~ "stranger2",
  grepl("stranger", df$Neighbour) ~ "stranger",
  TRUE ~ df$Neighbour
)

### --- Liste des espèces focales (à adapter si besoin) ---
species_list <- c("ATO", "SUR", "EPO 072", "VEN", "TIT", "CAL", "MEM", "PLA", "KWS")
multi_stranger <- c("MEM","PLA","KWS")
vars <- c("Length_Focal","Tillers","NBI")

dir.create("species_plots", showWarnings = FALSE)

###############################################################################
### Fonction run_test corrigée et robuste
###############################################################################
run_test <- function(df_sub, var, levels){
  # Vérifications initiales
  if(!(var %in% colnames(df_sub))){
    return(list(p = NA, method = "variable_missing"))
  }
  # garder uniquement niveaux demandés
  df_sub <- df_sub %>% filter(Neighbour %in% levels)
  if(nrow(df_sub) == 0){
    return(list(p = NA, method = "no_data"))
  }
  # combien de groupes effectivement présents ?
  present_groups <- unique(df_sub$Neighbour)
  if(length(present_groups) < 2){
    return(list(p = NA, method = "not_enough_groups"))
  }
  # Shapiro par groupe : utiliser cur_data() + pull pour être sûr
  sh <- df_sub %>%
    group_by(Neighbour) %>%
    summarise(
      sh_p = {
        x <- pull(cur_data(), var)
        x_non_na <- x[!is.na(x)]
        if(length(x_non_na) < 3 || length(unique(x_non_na)) <= 1){
          NA_real_
        } else {
          tryCatch(shapiro.test(x_non_na)$p.value, error = function(e) NA_real_)
        }
      },
      .groups = "drop"
    )
  normal <- all(sh$sh_p > 0.05, na.rm = TRUE)
  
  # si seulement 2 groupes présents (réels), on utilise t-test / wilcoxon
  if(length(present_groups) == 2){
    g1 <- df_sub %>% filter(Neighbour == present_groups[1]) %>% pull(var)
    g2 <- df_sub %>% filter(Neighbour == present_groups[2]) %>% pull(var)
    # Si tous NA dans un groupe -> cannot test
    if(all(is.na(g1)) || all(is.na(g2))){
      return(list(p = NA, method = "all_NA_in_group"))
    }
    if(normal){
      out <- try(t.test(g1, g2), silent = TRUE)
      if(inherits(out, "try-error")) return(list(p=NA, method="t-test error"))
      return(list(p = out$p.value, method = "t-test"))
    } else {
      out <- try(wilcox.test(g1, g2), silent = TRUE)
      if(inherits(out, "try-error")) return(list(p=NA, method="wilcoxon error"))
      return(list(p = out$p.value, method = "wilcoxon"))
    }
  }
  
  # si >=3 groupes (ex: kin, stranger1, stranger2) : ANOVA/Kruskal
  # construire formule propre
  formula <- as.formula(paste(var, "~ Neighbour"))
  if(normal){
    fit <- try(aov(formula, data = df_sub), silent = TRUE)
    if(inherits(fit, "try-error")){
      kw <- try(kruskal.test(formula, data = df_sub), silent = TRUE)
      if(inherits(kw, "try-error")) return(list(p = NA, method="kruskal error"))
      return(list(p = kw$p.value, method = "kruskal"))
    }
    p <- summary(fit)[[1]][["Pr(>F)"]][1]
    return(list(p = p, method = "anova"))
  } else {
    kw <- try(kruskal.test(formula, data = df_sub), silent = TRUE)
    if(inherits(kw, "try-error")) return(list(p = NA, method="kruskal error"))
    return(list(p = kw$p.value, method = "kruskal"))
  }
}

###############################################################################
### Fold change robuste
###############################################################################
compute_fc <- function(df_sub, var, levels){
  if(!(var %in% colnames(df_sub))) return(list(fc = setNames(rep(NA, sum(levels != "kin")), levels[levels!="kin"])))
  kin_vals <- df_sub %>% filter(Neighbour == "kin") %>% pull(var)
  kin_mean <- mean(kin_vals, na.rm = TRUE)
  fcs <- list()
  for(l in levels){
    if(l == "kin") next
    mean_l <- df_sub %>% filter(Neighbour == l) %>% pull(var) %>% mean(na.rm = TRUE)
    fcs[[l]] <- if(is.na(kin_mean) || kin_mean == 0) NA else mean_l / kin_mean
  }
  return(list(fc = fcs))
}

###############################################################################
### Boucle sur espèces
###############################################################################
for(sp in species_list){
  cat("\n=== Espèce :", sp, "===\n")
  df_sp <- df %>% filter(ID_focal == sp)
  if(nrow(df_sp) == 0){
    cat(" -> aucune donnée pour cette espèce (ID_focal)\n")
    next
  }
  levels_use <- if(sp %in% multi_stranger) c("kin","stranger1","stranger2") else c("kin","stranger")
  plot_list <- list()
  for(var in vars){
    df_sub <- df_sp %>% filter(Neighbour %in% levels_use)
    test <- run_test(df_sub, var, levels_use)
    fc   <- compute_fc(df_sub, var, levels_use)
    # build annotation text safely
    p_txt <- ifelse(is.na(test$p), "p=NA", paste0("p=", signif(test$p,3)))
    fc_txt <- paste0(names(fc$fc), "=", round(unlist(fc$fc), 3), collapse = " | ")
    label_txt <- paste0(p_txt, "\nFC: ", fc_txt, "\n(method: ", test$method, ")")
    # safe plotting (if no rows, create empty plot with message)
    if(nrow(df_sub) == 0){
      g <- ggplot() + theme_void() + ggtitle(paste(sp, "-", var)) +
        annotate("text", x=0.5, y=0.5, label="no data for this variable / neighbours", size=4)
    } else {
      g <- ggplot(df_sub, aes(x = factor(Neighbour, levels = levels_use), y = .data[[var]], fill = Neighbour)) +
        geom_boxplot() +
        theme_bw() +
        scale_fill_brewer(palette = "Set2") +
        ggtitle(paste(sp, "-", var)) +
        annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.5, label=label_txt, size=3)
    }
    plot_list[[var]] <- g
  }
  fig <- ggpubr::ggarrange(plotlist = plot_list, ncol=3)
  ggsave(paste0("species_plots/", gsub(" ", "_", sp), "_boxplots.png"), fig, width=16, height=5)
}

cat("\n✔ Analyse terminée. Figures dans species_plots/\n")
