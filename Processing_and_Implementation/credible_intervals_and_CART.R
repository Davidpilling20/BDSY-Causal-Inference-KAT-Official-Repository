library(rpart)

# ---- Read table 3 and posterior draws ----
df   <- read.csv("REDACTED", stringsAsFactors = FALSE)
post <- readRDS("REDACTED")
itty_draws <- post$itty_draws
ittw_draws <- post$ittw_draws
df$row_id <- seq_len(nrow(df))

# ---- Predictors ----
predictors_full <- c("race", "hispanic", "age", "femalesex", "elx_comorbidity_score",
                     "baseline_creatinine7", "hospital",
                     "bun_at_randomization", "potassium_at_randomization",
                     "bicarbonate_at_randomization", "wbcc_at_randomization",
                     "hemoglobin_at_randomization", "plateletcount_at_randomization",
                     "antibiotic_at_randomization", "nsaid_at_randomization",
                     "sus_hypotension_pre_randomization",
                     "vasopressor_at_randomization", "total_recommended_count",
                     "gen_med_floor", "icu", "surgical_floor")

ancestors <- function(n) {
  path <- n
  while (n > 1) { n <- n %/% 2; path <- c(path, n) }
  sort(unique(path))
}

# ---- Function: build tree + per-node credible intervals ----
run_tree <- function(comp, predictors, title) {
  fit <- rpart(ratio ~ ., data = comp[, c("ratio", predictors)],
               method = "anova", maxdepth = 2, cp = 0.01, minsplit = 20)
  
  cat("\n\n=====================================================\n")
  cat("===", title, "  (n =", nrow(comp), ") ===\n")
  cat("=====================================================\n")
  print(fit)
  
  comp$node_leaf <- as.integer(rownames(fit$frame))[fit$where]
  all_nodes <- as.integer(rownames(fit$frame))
  
  node_ci <- function(node) {
    under <- sapply(comp$node_leaf, function(lf) node %in% ancestors(lf))
    idx   <- comp$row_id[under]
    num   <- rowMeans(itty_draws[, idx, drop = FALSE])
    den   <- pmax(rowMeans(ittw_draws[, idx, drop = FALSE]), 0.05)
    cd    <- num / den
    data.frame(node = node,
               is_leaf = fit$frame$var[match(node, all_nodes)] == "<leaf>",
               n = length(idx),
               hcace = round(mean(cd), 4),
               lower95 = round(quantile(cd, 0.025), 4),
               upper95 = round(quantile(cd, 0.975), 4),
               p_pos = round(mean(cd > 0), 3))
  }
  tab <- do.call(rbind, lapply(all_nodes, node_ci))
  rownames(tab) <- NULL
  cat("\n--- Node-level hCACE with 95% credible intervals ---\n")
  print(tab, row.names = FALSE)
  
  root <- tab[tab$node == 1, ]
  cat("\nOverall CACE:", root$hcace,
      " | 95% CrI: [", root$lower95, ",", root$upper95, "]",
      " | P(hCACE>0):", root$p_pos, "\n")
  
  invisible(list(fit = fit, table = tab))
}

# ---- Tree 1: ALL compliers ----
comp_all <- df[df$stratum == "complier", ]
res_all <- run_tree(comp_all, predictors_full, "CART on ALL compliers")

# ---- Tree 2: YALE compliers only (drop hospital: constant within Yale) ----
comp_yale <- df[df$stratum == "complier" & df$hospital == 0, ]
predictors_yale <- setdiff(predictors_full, "hospital")
res_yale <- run_tree(comp_yale, predictors_yale, "CART on YALE compliers only")



# ===== Frequentist CI + p-value per node, for BOTH trees =====

freq_nodes <- function(fit, comp, title) {
  frame         <- fit$frame
  node_ids      <- as.numeric(rownames(frame))
  leaf_node_ids <- node_ids[frame$var == "<leaf>"]
  comp$leaf_node <- as.integer(rownames(frame))[fit$where]
  
  desc_leaves <- function(node) {
    out <- c()
    for (L in leaf_node_ids) {
      x <- L
      while (x >= 1) { if (x == node) { out <- c(out, L); break }; x <- x %/% 2 }
    }
    out
  }
  
  n_all <- nrow(comp)
  tab <- do.call(rbind, lapply(node_ids, function(nd) {
    idx <- comp$leaf_node %in% desc_leaves(nd)
    r   <- comp$ratio[idx]
    nn  <- length(r)
    est <- mean(r); se <- sd(r) / sqrt(nn)
    pval <- if (nn > 1) t.test(r, mu = 0)$p.value else NA
    data.frame(node = nd,
               is_leaf = frame$var[node_ids == nd] == "<leaf>",
               n = nn, share = round(nn / n_all, 3),
               estimate = round(est, 4),
               ci_lower = round(est - 1.96 * se, 4),
               ci_upper = round(est + 1.96 * se, 4),
               p_value  = round(pval, 4))
  }))
  rownames(tab) <- NULL
  cat("\n\n=== Frequentist CI + p-value:", title, "===\n")
  print(tab, row.names = FALSE)
  invisible(tab)
}

freq_all  <- freq_nodes(res_all$fit,  comp_all,  "ALL compliers")
freq_yale <- freq_nodes(res_yale$fit, comp_yale, "YALE compliers only")

