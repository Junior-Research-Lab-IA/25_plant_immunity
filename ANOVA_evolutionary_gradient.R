library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Mise au format long ----
long <- df2 %>%
  pivot_longer(
    c(Length_Focal, Tillers_Focal, NBI),
    names_to = "Trait",
    values_to = "Value"
  )

# ---- Ajouter "(cm)" seulement pour le trait Length_Focal ----
long <- long %>%
  mutate(
    Trait_label = case_when(
      Trait == "Length_Focal" ~ "Length_Focal (cm)",
      TRUE ~ Trait
    )
  )

# ---- Calcul moyennes & SE ----
sumdat <- long %>%
  group_by(Trait, Trait_label, Evolutionary, Neighbour) %>%
  summarise(
    n = sum(!is.na(Value)),
    mean = mean(Value, na.rm = TRUE),
    se = sd(Value, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# ---- Graphique ----
ggplot(sumdat, aes(x = Evolutionary, y = mean, color = Neighbour)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.15,
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ Trait_label, scales = "free_y") +
  labs(
    title = "Trait means ± SE by Evolutionary status (0 wild / 1 cultivated) and Neighbour",
    x = "Evolutionary category (0 = wild / 1 = cultivated)",
    y = "Mean ± SE",
    color = "Neighbour"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 13),
    strip.text = element_text(face = "bold")
  )

