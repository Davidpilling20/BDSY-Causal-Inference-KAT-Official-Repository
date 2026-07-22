# ---- 1. Read table 2 ----
dat <- read.csv("/Users/zjx/Desktop/Redo/table2_scored.csv", stringsAsFactors = FALSE)

# ---- 2. Define the four sets by (Z, W) under monotonicity ----
# M0 = {Z=0, W=0}: never-taker + complier (mixed)
# M1 = {Z=1, W=0}: pure never-taker
# M2 = {Z=1, W=1}: always-taker + complier (mixed)
# M3 = {Z=0, W=1}: pure always-taker
M0 <- which(dat$Z == 0 & dat$W == 0)
M1 <- which(dat$Z == 1 & dat$W == 0)
M2 <- which(dat$Z == 1 & dat$W == 1)
M3 <- which(dat$Z == 0 & dat$W == 1)

n_M0 <- length(M0); n_M1 <- length(M1)
n_M2 <- length(M2); n_M3 <- length(M3)

cat("---- Set counts ----\n")
print(data.frame(
  set  = c("M0","M1","M2","M3"),
  type = c("never+complier (mixed)","pure never","always+complier (mixed)","pure always"),
  n    = c(n_M0, n_M1, n_M2, n_M3)
), row.names = FALSE)
cat("\nSum of four sets:", n_M0+n_M1+n_M2+n_M3, " | total rows:", nrow(dat), "\n")

# ---- 3. Principal scores (computed here, not in script 2) ----
# t_score = p0 / p1            in M2, high t  -> always-taker
# r_score = p_never / (1 - p0) in M0, high r  -> never-taker
dat$t_score <- dat$p0 / dat$p1
dat$r_score <- dat$never_taker / (1 - dat$p0)

# ---- 4. Peel off pure types by score; the rest are compliers ----
# In M2 (always+complier): the n_M3 highest t_score are the always-takers
hatM2 <- M2[order(dat$t_score[M2], decreasing = TRUE)][seq_len(n_M3)]
hatC2 <- setdiff(M2, hatM2)                                   # compliers from M2

# In M0 (never+complier): the n_M1 highest r_score are the never-takers
hatM0 <- M0[order(dat$r_score[M0], decreasing = TRUE)][seq_len(n_M1)]
hatC1 <- setdiff(M0, hatM0)                                   # compliers from M0

C <- union(hatC1, hatC2)

# ---- 5. Label every row: stratum ----
dat$stratum <- NA_character_
dat$stratum[c(M1, hatM0)]    <- "never_taker"    # pure never + peeled never
dat$stratum[c(M3, hatM2)]    <- "always_taker"   # pure always + peeled always
dat$stratum[c(hatC1, hatC2)] <- "complier"

# ---- 6. Tag complier source ----
dat$complier_source <- NA_character_
dat$complier_source[hatC1] <- "C1"   # complier identified from M0
dat$complier_source[hatC2] <- "C2"   # complier identified from M2

# ---- 7. Report ----
cat("\nhatC1 (compliers from M0) =", length(hatC1),
    " | hatC2 (compliers from M2) =", length(hatC2),
    " | total compliers =", length(C), "\n\n")
print(table(dat$stratum, useNA = "ifany"))
cat("\ncomplier_source:\n"); print(table(dat$complier_source, useNA = "ifany"))

# ---- 8. Save table 3 ----
write.csv(dat, "/Users/zjx/Desktop/Redo/table3_labeled.csv", row.names = FALSE)
cat("\nSaved table3_labeled.csv to Desktop/Redo\n")