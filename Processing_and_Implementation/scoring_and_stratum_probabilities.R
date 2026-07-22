library(BART)
set.seed(2026)

# ---- 1. Read table 1 ----
df <- read.csv("REDACTED", stringsAsFactors = FALSE)

# ---- 2. Pull columns BY NAME (never by position) ----
Y <- df$Y
Z <- df$Z
W <- df$W
X <- df[, setdiff(names(df), c("Y", "Z", "W"))]   # 21 covariates
n <- nrow(df)
cat("n =", n, " | #covariates in X =", ncol(X), "\n")
cat("X columns:\n"); print(names(X))

# ---- 3. T-learner helper: fit on each arm, predict counterfactually on everyone ----
tlearner_pbart <- function(outcome, X, Z) {
  fit1 <- pbart(x.train = X[Z == 1, ], y.train = outcome[Z == 1],
                x.test = X, ntree = 200, nskip = 500, ndpost = 1000)
  fit0 <- pbart(x.train = X[Z == 0, ], y.train = outcome[Z == 0],
                x.test = X, ntree = 200, nskip = 500, ndpost = 1000)
  list(p1 = fit1$prob.test, p0 = fit0$prob.test)   # each ndpost x n
}

# ---- 4. ITTw: T-learner on W ----
cat("\n== Fitting BART for W (ITTw) ==\n")
w_fit <- tlearner_pbart(W, X, Z)
p1_draws <- w_fit$p1
p0_draws <- w_fit$p0
ittw_draws <- p1_draws - p0_draws

p1   <- colMeans(p1_draws)
p0   <- colMeans(p0_draws)
ittw <- colMeans(ittw_draws)

# ---- 5. ITTy: T-learner on Y ----
cat("\n== Fitting BART for Y (ITTy) ==\n")
y_fit <- tlearner_pbart(Y, X, Z)
itty_draws <- y_fit$p1 - y_fit$p0
itty <- colMeans(itty_draws)

# ---- 6. Principal stratum probabilities (per derivation figure) ----
always_taker <- p0            # e11 = P(W=1 | Z=0)
complier     <- ittw          # e01 = p1 - p0
never_taker  <- 1 - p1        # e00 = P(W=0 | Z=1)

# ---- 7. Adjusted ITTw (truncate small/negative to 0.05) and ratio = hCACE ----
adjusted_ittw <- pmax(ittw, 0.05)
ratio <- itty / adjusted_ittw

# ---- 8. Attach all new columns ----
df$p0            <- p0
df$p1            <- p1
df$ITTy          <- itty
df$ITTw          <- ittw
df$adjusted_ITTw <- adjusted_ittw
df$always_taker  <- always_taker
df$complier      <- complier
df$never_taker   <- never_taker
df$ratio         <- ratio

# ---- 9. Sanity checks (these catch p0/p1 mistakes) ----
cat("\n---- SANITY CHECKS ----\n")
cat("Three strata sum to 1? range of (always+complier+never):",
    round(range(always_taker + complier + never_taker), 4), "\n")
cat("p1 mean (should be ~ observed P(W=1|Z=1) =", round(mean(W[Z==1]), 3), "):",
    round(mean(p1), 3), "\n")
cat("p0 mean (should be ~ observed P(W=1|Z=0) =", round(mean(W[Z==0]), 3), "):",
    round(mean(p0), 3), "\n")
cat("complier (ITTw) mean:", round(mean(complier), 3),
    " | fraction negative:", round(mean(complier < 0), 3), "\n")
cat("always_taker mean:", round(mean(always_taker), 3),
    " | never_taker mean:", round(mean(never_taker), 3), "\n")
cat("ITTy mean:", round(mean(itty), 4), "\n")

# ---- 10. Monotonicity check: how many with ITTw upper CI < 0 (defiers) ----
ittw_ci <- t(apply(ittw_draws, 2, quantile, probs = c(0.025, 0.975)))
cat("Defiers (ITTw upper < 0):", sum(ittw_ci[,2] < 0), "\n")

# ---- 11. Save table 2 (+ keep posterior draws for credible intervals in script 4) ----
write.csv(df, "Desktop/table2_scored.csv", row.names = FALSE)
saveRDS(list(itty_draws = itty_draws, ittw_draws = ittw_draws,
             adjusted_ittw = adjusted_ittw),
        "Desktop/posterior_draws.rds")
cat("\nSaved table2_scored.csv and posterior_draws.rds to Desktop\n")
