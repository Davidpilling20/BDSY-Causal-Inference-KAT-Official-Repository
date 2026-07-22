library(tidyverse)

df <- read.csv("Desktop/table3_labeled.csv", stringsAsFactors = FALSE)
d <- df %>% mutate(System = ifelse(hospital == 1, "Hopkins", "Yale"))

lab_map <- c(
  hispanic="Hispanic", age="Age",
  elx_comorbidity_score="Elixhauser score", baseline_creatinine7="Baseline creatinine",
  bun_at_randomization="BUN", potassium_at_randomization="Potassium",
  hemoglobin_at_randomization="Hemoglobin",
  antibiotic_at_randomization="Antibiotic use", nsaid_at_randomization="NSAID use",
  total_recommended_count="Recommendations (n)",
  icu="ICU"
)
vars <- names(lab_map)

smd <- map_dfr(vars, function(v) {
  x1 <- d[[v]][d$System == "Hopkins"]; x0 <- d[[v]][d$System == "Yale"]
  sp <- sqrt((var(x1, na.rm=TRUE) + var(x0, na.rm=TRUE)) / 2)
  tibble(var = v, smd = (mean(x1, na.rm=TRUE) - mean(x0, na.rm=TRUE)) / sp)
}) %>%
  mutate(label = lab_map[var], abssmd = abs(smd))

p <- ggplot(smd, aes(x = smd, y = reorder(label, abssmd))) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.8) +
  geom_segment(aes(x = 0, xend = smd, yend = reorder(label, abssmd)),
               color = "#5A8FD4", linewidth = 2) +
  geom_point(fill = "#5A8FD4", color = "#2E5B94", size = 5.5, shape = 21, stroke = 0.9) +
  labs(x = "Standardized mean difference  (Hopkins − Yale)", y = NULL) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title   = element_text(face = "bold", size = 24),
    axis.text.y  = element_text(size = 18, face = "bold"),
    axis.text.x  = element_text(size = 16),
    axis.title.x = element_text(size = 18, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p)
ggsave("Desktop/smd_baseline_by_system.png", p, width = 10, height = 4, dpi = 500)
