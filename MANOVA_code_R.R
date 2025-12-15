# Librairie nécéssaire
library(readxl)   
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(car)
library(ggplot2)
library(GGally)

# Traitement de tableau pour la manipulation
dat_full <- read_excel("data_im.xlsx")
dat <- dat_full[1:184, ]
dat <- dat[, colSums(!is.na(dat)) > 0]
dat<-na.omit(dat)
str(dat)
dat$Density_Neighbour<-as.numeric(dat$Density_Neighbour)
vars <- c("Length_Focal",
          "Length_Neighbour",
          "Density_Focal",
          "Density_Neighbour",
          "NBI")

dat[vars] <- scale(dat[vars])
View(dat)

# Boxplot des données 
p1 <- ggboxplot(
 dat, x = "Treatment", y = c("Length_Focal","Length_Neighbour","Density_Focal","Density_Neighbour","NBI"), 
  merge = TRUE, palette = "jco", title= "Traitement"
)

p2 <- ggboxplot(
  dat, x = "Neighbour", y = c("Length_Focal","Length_Neighbour","Density_Focal","Density_Neighbour","NBI"), 
  merge = TRUE, palette = "jco", title= "Voisin"
)

ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Statistiques descriptives 
dat %>%
  group_by(Treatment) %>%
  get_summary_stats(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI, type = "mean_sd")

dat %>%
  group_by(Neighbour) %>%
  get_summary_stats(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI, type = "mean_sd")

# Vérification de la taille de l'échantillon 
dat %>%
  group_by(Treatment) %>%
  summarise(N = n())

dat %>%
  group_by(Neighbour) %>%
  summarise(N = n())
# Valeurs abberantes 
#Notez que, dans le cas où vous avez des valeurs extrêmes aberrantes, cela peut être dû à : 1) erreurs de saisie de données, erreurs de mesure ou valeurs inhabituelles.
#Vous pouvez de toute façon inclure la valeur aberrante dans l’analyse si vous ne croyez pas que le résultat sera affecté de façon substantielle. Ceci peut être évalué en comparant le résultat du MANOVA avec et sans la valeur aberrante.

# Valeurs abberantes traitement 
dat %>%
  group_by(Treatment) %>%
  identify_outliers(Length_Focal)

dat %>%
  group_by(Treatment) %>%
  identify_outliers(Length_Neighbour)

dat %>%
  group_by(Treatment) %>%
  identify_outliers(Density_Focal)

dat %>%
  group_by(Treatment) %>%
  identify_outliers(Density_Neighbour)

dat %>%
  group_by(Treatment) %>%
  identify_outliers(NBI)

# Valeurs abberantes voisin 
dat %>%
  group_by(Neighbour) %>%
  identify_outliers(Length_Focal)

dat %>%
  group_by(Neighbour) %>%
  identify_outliers(Length_Neighbour)

dat %>%
  group_by(Neighbour) %>%
  identify_outliers(Density_Focal)

dat %>%
  group_by(Neighbour) %>%
  identify_outliers(Density_Neighbour)

dat %>%
  group_by(Neighbour) %>%
  identify_outliers(NBI)

# Valeurs aberrantes multivariées
#Si vous avez des valeurs aberrantes multivariées, vous pouvez envisager d’exécuter MANOVA avant et après avoir supprimé les valeurs aberrantes pour vérifier si leur présence modifie ou non les résultats.
# Valeurs abberantes traitement
dat %>%
  group_by(Treatment) %>%
  mahalanobis_distance(c(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI)) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# Valeurs abberantes voisin
dat %>%
  group_by(Neighbour) %>%
  mahalanobis_distance(c(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI)) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# Test shapiro pour la normalité univariée (treatment)
dat %>%
  group_by(Treatment) %>%
  shapiro_test(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) %>%
  arrange(variable)

p1 <- ggqqplot(dat, "Length_Focal", facet.by = "Treatment",
               ylab = "Length_Focal", ggtheme = theme_bw())

p2 <- ggqqplot(dat, "Length_Neighbour", facet.by = "Treatment",
               ylab = "Length_Neighbour", ggtheme = theme_bw())

p3 <- ggqqplot(dat, "Density_Focal", facet.by = "Treatment",
               ylab = "Density_Focal", ggtheme = theme_bw())

p4 <- ggqqplot(dat, "Density_Neighbour", facet.by = "Treatment",
               ylab = "Density_Neighbour", ggtheme = theme_bw())

p5 <- ggqqplot(dat, "NBI", facet.by = "Treatment",
               ylab = "NBI", ggtheme = theme_bw())

ggarrange(p1, p2, p3, p4, p5,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "bottom")

# Test shapiro pour la normalité univariée (neighbour)
dat %>%
  group_by(Neighbour) %>%
  shapiro_test(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) %>%
  arrange(variable)

p1 <- ggqqplot(dat, "Length_Focal", facet.by = "Neighbour",
               ylab = "Length_Focal", ggtheme = theme_bw())

p2 <- ggqqplot(dat, "Length_Neighbour", facet.by = "Neighbour",
               ylab = "Length_Neighbour", ggtheme = theme_bw())

p3 <- ggqqplot(dat, "Density_Focal", facet.by = "Neighbour",
               ylab = "Density_Focal", ggtheme = theme_bw())

p4 <- ggqqplot(dat, "Density_Neighbour", facet.by = "Neighbour",
               ylab = "Density_Neighbour", ggtheme = theme_bw())

p5 <- ggqqplot(dat, "NBI", facet.by = "Neighbour",
               ylab = "NBI", ggtheme = theme_bw())

ggarrange(p1, p2, p3, p4, p5,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "bottom")


# Normalité à plusieurs variables 
dat %>%
  select(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) %>%
  mshapiro_test()

# Identifier la multicollinéarité
# Idéalement, la corrélation entre les variables-réponses devrait être modérée, pas trop élevée. Une corrélation supérieure à 0,9 est une indication de la multicollinéarité, ce qui est problématique pour MANOVA, ceci n'étant pas le cas ici.
correlation <- dat %>% cor_mat(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI)
View(correlation)


# Hypothèse de linéarité
#Dans le cas où vous détectez des relations non linéaires, vous pouvez: exécuter l’analyse de toute façon. Vous perdrez un peu de puissance.

results <- dat %>%
  select(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI,Treatment) %>%
  group_by(Treatment) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

results$plots


results <- dat %>%
  select(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI, Neighbour) %>%
  group_by(Neighbour) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

results$plots

# Hypothèse d’homogénéité des covariances
#Le test est statistiquement significatif (p < 0,001), donc les données ont violé l’hypothèse de l’homogénéité des matrices de variance-covariance.
#Notez que, si vous avez un plan d’échantillonnage équilibré (c.-à-d. des groupes de taille similaire), vous n’avez pas à vous soucier trop de la violation de l’homogénéité des matrices de variances-covariances et vous pouvez continuer votre analyse, c'est notre cas ici.

box_m(dat[, c("Length_Focal","Length_Neighbour","Density_Focal","Density_Neighbour","NBI")], dat$Treatment)
box_m(dat[, c("Length_Focal","Length_Neighbour","Density_Focal","Density_Neighbour","NBI")], dat$Neighbour)

# Hypothèse d’homogénéité de la variance

dat %>% 
  gather(key = "variable", value = "value", Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)

dat %>% 
  gather(key = "variable", value = "value", Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) %>%
  group_by(variable) %>%
  levene_test(value ~ Neighbour)

# MANOVA

model <- lm(cbind(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) ~ Treatment, dat)
Manova(model, test.statistic = "Wilks")

model1 <- lm(cbind(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) ~ Neighbour, dat)
Manova(model1, test.statistic = "Pillai")

model2 <- lm(cbind(Length_Focal,Length_Neighbour,Density_Focal,Density_Neighbour,NBI) ~ Treatment*Neighbour, dat)
Manova(model2, test.statistic = "Pillai")












