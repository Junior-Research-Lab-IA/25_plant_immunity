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
dat_full <- read_excel("batch1.xlsx")

dat <- dat_full %>%
  slice(1:184) %>% 
  drop_na(Length_Focal, Density_Focal)

dat$Treatment  <- as.factor(dat$Treatment)
dat$Neighbour  <- as.factor(dat$Neighbour)

# Vérification de sécurité
colnames(dat)
table(dat$Treatment)
table(dat$Neighbour)

vars <- c("Length_Focal",
          "Density_Focal"
          )

dat[vars] <- scale(dat[vars])
View(dat)

# Boxplot des données 
p1 <- ggboxplot(
  dat, x = "Treatment", y = c("Length_Focal","Density_Focal"), 
  merge = TRUE, palette = "jco", title= "Traitement"
)

p2 <- ggboxplot(
  dat, x = "Neighbour", y = c("Length_Focal","Density_Focal"), 
  merge = TRUE, palette = "jco", title= "Voisin"
)

ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Statistiques descriptives 
dat %>%
  group_by(Treatment) %>%
  get_summary_stats(Length_Focal, Density_Focal, type = "mean_sd")

dat %>%
  group_by(Neighbour) %>%
  get_summary_stats(Length_Focal,Density_Focal, type = "mean_sd")

# Vérification de la taille de l'échantillon 
dat %>%
  group_by(Treatment) %>%
  summarise(N = n())

dat %>%
  group_by(Neighbour) %>%
  summarise(N = n())



# Test shapiro pour la normalité univariée (treatment)
dat %>%
  group_by(Treatment) %>%
  shapiro_test(Length_Focal,Density_Focal) %>%
  arrange(variable)

p1 <- ggqqplot(dat, "Length_Focal", facet.by = "Treatment",
               ylab = "Length_Focal", ggtheme = theme_bw())

p2 <- ggqqplot(dat, "Density_Focal", facet.by = "Treatment",
               ylab = "Density_Focal", ggtheme = theme_bw())

ggarrange(p1, p2,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "bottom")

# Test shapiro pour la normalité univariée (neighbour)
dat %>%
  group_by(Neighbour) %>%
  shapiro_test(Length_Focal,Density_Focal) %>%
  arrange(variable)

p1 <- ggqqplot(dat, "Length_Focal", facet.by = "Neighbour",
               ylab = "Length_Focal", ggtheme = theme_bw())

p2 <- ggqqplot(dat, "Density_Focal", facet.by = "Neighbour",
               ylab = "Density_Focal", ggtheme = theme_bw())


ggarrange(p1, p2,
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "bottom")


# Normalité à plusieurs variables 
dat %>%
  select(Length_Focal,Density_Focal) %>%
  mshapiro_test()

# Identifier la multicollinéarité
# Idéalement, la corrélation entre les variables-réponses devrait être modérée, pas trop élevée. Une corrélation supérieure à 0,9 est une indication de la multicollinéarité, ce qui est problématique pour MANOVA, ceci n'étant pas le cas ici.
#correlation <- dat %>% cor_mat(Length_Focal,Density_Focal)
#View(correlation)


# Hypothèse de linéarité
#Dans le cas où vous détectez des relations non linéaires, vous pouvez: exécuter l’analyse de toute façon. Vous perdrez un peu de puissance.

results <- dat %>%
  group_by(Treatment) %>%
  select(Length_Focal,Density_Focal) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

results$plots


results <- dat %>%
  select(Length_Focal,Density_Focal, Neighbour) %>%
  group_by(Neighbour) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

results$plots

# Hypothèse d’homogénéité des covariances
#Le test est statistiquement significatif (p < 0,001), donc les données ont violé l’hypothèse de l’homogénéité des matrices de variance-covariance.
#Notez que, si vous avez un plan d’échantillonnage équilibré (c.-à-d. des groupes de taille similaire), vous n’avez pas à vous soucier trop de la violation de l’homogénéité des matrices de variances-covariances et vous pouvez continuer votre analyse, c'est notre cas ici.

box_m(dat[, c("Length_Focal","Density_Focal")], dat$Treatment)
box_m(dat[, c("Length_Focal","Density_Focal")], dat$Neighbour)

# Hypothèse d’homogénéité de la variance

dat %>% 
  gather(key = "variable", value = "value", Length_Focal,Density_Focal) %>%
  group_by(variable) %>%
  levene_test(value ~ Treatment)

dat %>% 
  gather(key = "variable", value = "value", Length_Focal,Density_Focal) %>%
  group_by(variable) %>%
  levene_test(value ~ Neighbour)

# MANOVA

model <- lm(cbind(Length_Focal,Density_Focal) ~ Treatment, dat)
Manova(model, test.statistic = "Wilks")

model1 <- lm(cbind(Length_Focal, Density_Focal) ~ Neighbour, dat)
Manova(model1, test.statistic = "Pillai")

model2 <- lm(cbind(Length_Focal,Density_Focal) ~ Treatment*Neighbour, dat)
Manova(model2, test.statistic = "Pillai")












