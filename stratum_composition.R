library(tidyverse)
library(zoo)

final <- readr::read_csv("/Users/zjx/Desktop/REDO/table3_labeled.csv", show_col_types = FALSE)

df <- final %>%
  transmute(idx = row_number(), hosp = hospital,
            P_always = always_taker, P_complier = complier, P_never = never_taker)

boundary <- max(df$idx[df$hosp == 0])

k <- 51
roll <- function(x) rollapply(x, width = k, FUN = mean, align = "center", partial = TRUE)

sm <- df %>%
  group_by(hosp) %>%
  mutate(across(c(P_always, P_complier, P_never), roll)) %>%
  ungroup() %>%
  select(idx, P_always, P_complier, P_never) %>%
  pivot_longer(-idx, names_to = "stratum", values_to = "p") %>%
  mutate(stratum = factor(stratum,
                          levels = c("P_never","P_complier","P_always"),
                          labels = c("Never-taker","Complier","Always-taker")))

pal <- c("Never-taker"  = "#CDC3DB",
         "Complier"     = "#8FB4E5",
         "Always-taker" = "#A2D591")

p1 <- ggplot(sm, aes(idx, p, fill = stratum)) +
  geom_area(position = "stack", color = NA) +
  geom_vline(xintercept = boundary + 0.5, color = "grey30", linewidth = 1) +
  annotate("text", x = boundary/2, y = 0.94, label = "Yale",
           fontface = 2, size = 7, color = "grey20") +
  annotate("text", x = boundary + (max(sm$idx)-boundary)/2, y = 0.94,
           label = "Hopkins", fontface = 2, size = 7, color = "grey20") +
  scale_fill_manual(values = pal, name = NULL, guide = "none") +
  coord_cartesian(xlim = c(1, max(sm$idx)), ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent, expand = c(0, 0)) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Patient (record order)", y = "Stratum probability") +
  theme_minimal(base_size = 20) +
  theme(plot.margin = margin(t = 8, r = 25, b = 6, l = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
        axis.title = element_text(face = "bold", color = "grey25"),
        axis.text.y = element_text(face = "bold", color = "grey25"),
        axis.text.x = element_blank())

p1

dir.create("/Users/zjx/Desktop/REDO/poster plots", showWarnings = FALSE)
ggsave("/Users/zjx/Desktop/REDO/poster plots/stratum_composition.png",
       p1, width = 10, height = 6, dpi = 500)