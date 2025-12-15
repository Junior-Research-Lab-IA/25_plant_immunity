library(readxl)
library(dplyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(stringr)

# ---- Import ----
dat <- read_excel("Tableau_modifié_batch1_2.xlsx")

# Vérifier les noms réels
print(colnames(dat))

# ---- Si ta colonne s'appelle bien "ID Neighbour" avec un espace, alors on renomme uniquement celle-là ----
if ("ID Neighbour" %in% colnames(dat)) {
  dat <- dat %>% rename(ID_Neighbour = `ID Neighbour`)
}

# ---- Garder lignes complètes pour ACP ----
dat_acp <- dat %>% 
  drop_na(Length_Focal, Tillers, NBI) %>%
  mutate(
    Length_Focal = as.numeric(Length_Focal),
    Density_Focal = as.numeric(Tillers),
    NBI = as.numeric(NBI)
  )

# ---- ACP ----
res_pca <- PCA(dat_acp[, c("Length_Focal", "Tillers", "NBI")],
               scale.unit = TRUE,
               graph = FALSE)

# ---- Variance expliquée ----
eig <- res_pca$eig
print(eig)

# Graphique des variances expliquées (screeplot)
fviz_eig(res_pca,
         addlabels = TRUE,
         ylim = c(0, 100),
         barfill = "steelblue",
         barcolor = "black",
         title = "Percentage of variance explained by the axes")


# Biplot général
fviz_pca_biplot(res_pca,
                geom.ind = "point",
                #fill.ind = as.factor(dat_acp$Neighbour), # choix: Neighbour, Treatment, Ecological
                col.ind = "black",       # contour points
                pointshape = 21,
                pointsize = 3,
                alpha.ind = 0.9,
                
                col.var = "firebrick",
                repel = TRUE,
                
                title = "PCA results - Axes 1 and 2"
) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank()
  )


# -----------------------------
# 1) Coloration par Neighbour
# -----------------------------
fviz_pca_ind(res_pca,
             geom = "point",
             habillage = as.factor(dat_acp$Neighbour),
             addEllipses = TRUE,
             title = "ACP - Group by Neighbour")

# -----------------------------
# 2) Coloration par Treatment
# -----------------------------
fviz_pca_ind(res_pca,
             geom = "point",
             habillage = as.factor(dat_acp$Treatment),
             addEllipses = TRUE,
             title = "ACP - Group by Treatment")

# -----------------------------
# 3) Coloration par Ecological
# -----------------------------
#fviz_pca_ind(res_pca,
             #geom = "point",
             #habillage = as.factor(dat_acp$Ecological),
             #addEllipses = TRUE,
             #title = "ACP - Par Ecological")

# -----------------------------
# 4) Coloration par ID_Focal = espèce
# -----------------------------
fviz_pca_ind(res_pca,
             geom = "point",
             habillage = as.factor(dat_acp$ID_Focal),
             addEllipses = FALSE,
             title = "ACP - Par espèce (ID_Focal)")

library(Hmisc)

res <- rcorr(as.matrix(dat_acp[, c("Length_Focal", "Tillers", "NBI")]))

# Matrice de corrélation
res$r

# p-values associées
res$P

library(ggcorrplot)

cor_mat <- cor(dat_acp[, c("Length_Focal", "Tillers", "NBI")],
               use = "complete.obs")

ggcorrplot(cor_mat,
           lab = TRUE,
           type = "lower",
           outline.col = "black",
           colors = c("steelblue", "white", "firebrick"),
           title = "Correlation between Length_Focal, Tillers et NBI")
