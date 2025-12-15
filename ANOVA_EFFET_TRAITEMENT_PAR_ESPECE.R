# ===============================
# PACKAGES
# ===============================
library(readxl)
library(tidyverse)
library(car)
library(emmeans)

# ===============================
# 1) Import
# ===============================
df <- read_excel("C:/Users/febvre/Documents/DOMINANTE 4/PROJECT/Stats 4 déc/Valeurs_BATCH1.xlsx")

# ===============================
# 2) Nettoyage colonnes
# ===============================
names(df) <- names(df) %>%
  str_replace_all("\\s+", "_") %>%
  str_replace_all("[^0-9A-Za-z_]", "")

# Harmoniser Tillers_Focal si ancien nom
if ("Density_Focal" %in% names(df) && !("Tillers_Focal" %in% names(df))) {
  df <- df %>% rename(Tillers_Focal = Density_Focal)
}

# ===============================
# 3) Supprimer éventuelle ligne vide
# ===============================
empty_row <- which(rowSums(is.na(df)) == ncol(df))[1]
if (!is.na(empty_row)) df <- df[1:(empty_row - 1), ]

# ===============================
# 4) Nettoyage des valeurs (CRUCIAL)
# ===============================
# - Treatment : trim, normalise, recode variantes en "NT"/"T" avant factor()
# - Numériques : remplacer virgule décimale -> point puis as.numeric

df <- df %>%
  mutate(
    # ID_Focal en caractère pour filtrage robuste ensuite
    ID_Focal = as.character(ID_Focal),
    
    # Nettoyage + normalisation du traitement
    Treatment = trimws(as.character(Treatment)),
    Treatment = tolower(Treatment),
    Treatment = case_when(
      Treatment %in% c("nt","non traite","non traité","non-traite","non-traité","non_t","non_treat","no_treat") ~ "nt",
      Treatment %in% c("t","traite","traité","traité","treat","treated") ~ "t",
      TRUE ~ Treatment
    ),
    Treatment = toupper(Treatment),
    Treatment = factor(Treatment, levels = c("NT","T"))
  )

num_cols <- intersect(c("Length_Focal","Tillers_Focal","NBI"), names(df))
df <- df %>%
  mutate(across(all_of(num_cols), ~ as.numeric(str_replace(as.character(.), ",", "."))))

# Reco factor propre
df <- df %>%
  mutate(ID_Focal = factor(ID_Focal))

# ===============================
# 5) Variables
# ===============================
vars <- c("Length_Focal", "Tillers_Focal", "NBI")
vars <- vars[vars %in% names(df)]

# ===============================
# 6) Graphiques + annotation p-value & FC(T/NT) pour ID 65
# ===============================
for (v in vars) {
  
  # Sous-ensemble robuste ID 65
  df65 <- df %>%
    mutate(ID_Focal_num = suppressWarnings(as.numeric(as.character(ID_Focal)))) %>%
    filter(ID_Focal_num == 65)
  
  # Sécurité : s'il n'y a pas les 2 modalités, on l'indique
  lvls65 <- unique(na.omit(df65$Treatment))
  if (!all(c("NT","T") %in% lvls65)) {
    warning(paste("ID 65 n'a pas les deux niveaux T/NT pour", v, "-> FC sera NA"))
  }
  
  # Test Wilcoxon NT vs T (robuste)
  wtest <- wilcox.test(as.formula(paste(v, "~ Treatment")), data = df65, exact = FALSE)
  pval <- signif(wtest$p.value, 3)
  
  # Fold-change T/NT — sans drop_na avant, avec na.rm = TRUE
  mean_NT <- mean(df65[df65$Treatment == "NT", v, drop = TRUE], na.rm = TRUE)
  mean_T  <- mean(df65[df65$Treatment == "T",  v, drop = TRUE], na.rm = TRUE)
  FC <- if (is.na(mean_NT) || mean_NT == 0) NA_real_ else mean_T / mean_NT
  FC_lab <- ifelse(is.na(FC), "NA", signif(FC, 3))
  
  annotation_text <- paste0("ID 65 :  p = ", pval, "   |   FC (T/NT) = ", FC_lab)
  
  g <- ggplot(df, aes(
    x = as.factor(ID_Focal),
    y = .data[[v]],
    fill = Treatment
  )) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2),
                size = 1, alpha = 0.5) +
    theme_bw(base_size = 14) +
    labs(
      title = paste("Treatment effect on", v),
      x = "Species (ID_Focal)",
      y = v
    ) +
    scale_fill_brewer(palette = "Set2") +
    annotate(
      "text",
      x = 1,
      y = max(df[[v]], na.rm = TRUE) * 1.05,
      label = annotation_text,
      hjust = 0,
      size = 4
    ) +
    expand_limits(y = max(df[[v]], na.rm = TRUE) * 1.1)
  
  print(g)
}

# ===============================
# 7) ANOVA + post-hoc (inchangé)
# ===============================
anova_results <- list()
posthoc_results <- list()

for (v in vars) {
  cat("\n###########", v, "###########\n")
  f <- as.formula(paste(v, "~ Treatment * ID_Focal"))
  model <- lm(f, data = df)
  a <- Anova(model, type = 2)
  print(a)
  anova_results[[v]] <- a
  
  if ("Treatment:ID_Focal" %in% rownames(a) &&
      a["Treatment:ID_Focal", "Pr(>F)"] < 0.05) {
    em <- emmeans(model, ~ Treatment | ID_Focal)
    tuk <- pairs(em, adjust = "tukey")
    print(tuk)
    posthoc_results[[v]] <- tuk
  }
}
