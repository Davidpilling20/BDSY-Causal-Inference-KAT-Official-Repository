library(ggplot2)
library(dplyr)

# removed csv for public github
df <- read.csv("redacted")

# order x axis by ITTy
df <- df %>%
  mutate(row_id = row_number(),
         row_id = reorder(row_id, ITTy))

# caterpillar plot for ITTy
ggplot(df, aes(x = reorder(row_id, ITTy), y = ITTy)) +
  geom_errorbar(aes(ymin = ITTy_lo, ymax = ITTy_hi), 
                width = 0, color = "steelblue", 
                alpha = 0.6, linewidth = 0.8) +
  geom_point(size = 2, color = "#000066") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#404040") +
  labs(
    x = "Patients (sorted by ITTy)", 
    y = "Intention to Treat on Y (ITTy)", 
    title = "ITTy Caterpillar Plot with Credible Intervals") +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# order x axis by HCACE
df <- df %>%
  mutate(row_id = row_number(),
         row_id = reorder(row_id, HCACE))

# caterpillar plot for HCACE
ggplot(df, aes(x = reorder(row_id, HCACE), y = HCACE)) +
  geom_errorbar(aes(ymin = HCACE_lo, ymax = HCACE_hi), 
                width = 0, color = "darkseagreen", 
                alpha = 0.6, linewidth = 0.8) +
  geom_point(size = 2, color = "#003d00") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#404040") +
  labs(
    x = "Patients (sorted by HCACE)", 
    y = "Heterogeneous Complier Average Causal Effect (HCACE)", 
    title = "HCACE Caterpillar Plot with Credible Intervals") +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# data for pie chart
df <- data.frame(
  stratum = df$stratum
)

# create strata varibale with the sum of each estimated group
compliers <- sum(df$stratum == "complier", na.rm = TRUE)
never_takers <- sum(df$stratum == "never_taker", na.rm = TRUE)
always_takers <- sum(df$stratum == "always_taker", na.rm = TRUE)

# add new variables
df <- data.frame(
  labels = c("Compliers", "Never-takers", "Always-takers"),
  slices = c(compliers, never_takers, always_takers)
)
# calculate percentages
df$pct <- round(df$slices / sum(df$slices) * 100, 2)
df$pct_label <- paste0(df$pct, "%")

# define colors
my_colors <- c("#a2d591", "#8fb4e5", "#cdc3db")

# create pie chart
ggplot(df, aes(x = "", y = slices, fill = labels)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors, name = "Strata") +
  labs(title = "Proportions of Estimated Stratum Assignments") +
  ## removed percentage labels to add them manually to control font location
  theme_void(base_size = 14, base_family = "Times New Roman") +  
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -5),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 16)
  )
