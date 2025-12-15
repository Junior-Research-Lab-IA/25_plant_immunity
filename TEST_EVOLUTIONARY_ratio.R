# ============================
# Ratio Stranger/Kin par trait et Evolutionary (0/1)
# ============================
# Prérequis: df2 avec colonnes Neighbour (Kin/Stranger), Evolutionary (0/1),
#            Length_Focal, Tillers_Focal, NBI

library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(42)

# Si besoin de (re)créer df2 :
# df2 <- df %>%
#   filter(Neighbour %in% c("Kin","Stranger")) %>%
#   mutate(
#     Neighbour   = factor(Neighbour, levels = c("Kin","Stranger")),
#     Evolutionary = factor(Evolutionary, levels = c(0,1))
#   ) %>%
#   tidyr::drop_na(Length_Focal, Tillers_Focal, NBI)

# 1) Long format
long <- df2 %>%
  pivot_longer(c(Length_Focal, Tillers_Focal, NBI),
               names_to = "Trait", values_to = "Value") %>%
  drop_na(Value)

# 2) Fonction bootstrap du ratio des MOYENNES (Stranger/Kin)
boot_ratio_means <- function(x_str, x_kin, B = 2000) {
  # ratio point estimate
  rhat <- mean(x_str) / mean(x_kin)
  # bootstrap percentile CI
  nS <- length(x_str); nK <- length(x_kin)
  rb <- replicate(B, {
    ms <- mean(sample(x_str, nS, replace = TRUE))
    mk <- mean(sample(x_kin, nK, replace = TRUE))
    if (mk == 0) NA_real_ else ms / mk
  })
  rb <- rb[is.finite(rb)]
  ci <- quantile(rb, c(0.025, 0.975), na.rm = TRUE)
  list(ratio = rhat, lwr = ci[[1]], upr = ci[[2]])
}

# 3) Calcul ratio S/K par Trait × Evolutionary
ratios <- long %>%
  group_by(Trait, Evolutionary) %>%
  summarise(
    ratio = {
      vS <- Value[Neighbour == "Stranger"]
      vK <- Value[Neighbour == "Kin"]
      tmp <- boot_ratio_means(vS, vK, B = 2000)
      tmp$ratio
    },
    lwr = {
      vS <- Value[Neighbour == "Stranger"]
      vK <- Value[Neighbour == "Kin"]
      boot_ratio_means(vS, vK, B = 2000)$lwr
    },
    upr = {
      vS <- Value[Neighbour == "Stranger"]
      vK <- Value[Neighbour == "Kin"]
      boot_ratio_means(vS, vK, B = 2000)$upr
    },
    .groups = "drop"
  )

# 4) Plot : points = ratio S/K, barres = IC95%, ligne = 1 (égalité)
ggplot(ratios, aes(Evolutionary, ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
                width = 0.15, position = position_dodge(width = 0.6)) +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "Stranger/Kin ratio (means) ± 95% CI",
       x = "Evolutionary (0 = wild / 1 = cultivated)",
       y = "Ratio S/K (mean(Stranger) / mean(Kin))") +
  theme_minimal()
