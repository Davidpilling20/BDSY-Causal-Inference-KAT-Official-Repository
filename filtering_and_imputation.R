library(tidyverse)
library(VIM)   # KNN imputation

# ---- 1. Read raw all-sites file ----
raw <- readr::read_csv(
  "/Users/zjx/Desktop/Yale Causal inference 2026/data/Real Data/KAT AKI/KAT_AKI_full_allsites_4.26.26.csv",
  show_col_types = FALSE)

# ---- 2. Build table 1: extract + map to final_dat's 26-column schema ----
tab1 <- raw %>%
  transmute(
    # core: outcome / instrument / treatment
    Y = primary_outcome,
    Z = as.integer(randomization == 1),
    W = as.integer(total_implemented24_count >= 1),
    
    # demographics
    race       = as.integer(race == "White"),        # White vs non-White
    hispanic   = hispanic,
    age        = age,
    femalesex  = femalesex,
    
    # comorbidity / labs / severity
    elx_comorbidity_score          = elx_comorbidity_score,
    baseline_creatinine7           = baseline_creatinine7,
    bun_at_randomization           = bun_at_randomization,
    potassium_at_randomization     = potassium_at_randomization,
    bicarbonate_at_randomization   = bicarbonate_at_randomization,
    wbcc_at_randomization          = wbcc_at_randomization,
    hemoglobin_at_randomization    = hemoglobin_at_randomization,
    plateletcount_at_randomization = plateletcount_at_randomization,
    
    # meds / events
    antibiotic_at_randomization       = antibiotic_at_randomization,
    nsaid_at_randomization            = nsaid_at_randomization,
    sus_hypotension_pre_randomization = sus_hypotension_pre_randomization,
    vasopressor_at_randomization      = vasopressor_at_randomization,
    
    total_recommended_count = total_recommended_count,
    
    # service_type -> 3 dummies (Med Specialist Floor = reference)
    gen_med_floor = as.integer(service_type == "Gen Medical Floors"),
    icu           = as.integer(service_type == "ICU"),
    surgical_floor= as.integer(service_type == "Surgical Floor"),
    
    # hospital: 1-6 -> 0/1 (0 = Yale sites 1-5, 1 = Hopkins site 6)
    hospital = as.integer(hospital == 6)
  )

# ---- 3. Sanity checks BEFORE imputation ----
cat("Rows:", nrow(tab1), "  Cols:", ncol(tab1), "\n")
cat("\nhospital table (expect 0=3200, 1=803):\n"); print(table(tab1$hospital))
cat("\nZ table:\n"); print(table(tab1$Z, useNA="ifany"))
cat("\nW table:\n"); print(table(tab1$W, useNA="ifany"))
cat("\nY table:\n"); print(table(tab1$Y, useNA="ifany"))
cat("\nMissingness per column (%):\n")
print(round(sapply(tab1, function(x) mean(is.na(x))*100), 1))

# ---- 4. Whole-table KNN imputation (merged, not by system) ----
tab1_imp <- VIM::kNN(tab1, k = 5, imp_var = FALSE)

cat("\nRemaining NAs after imputation:", sum(is.na(tab1_imp)), "\n")

# ---- 5. Save to Desktop/Redo ----
dir.create("/Users/zjx/Desktop/Redo", showWarnings = FALSE)
readr::write_csv(tab1_imp, "/Users/zjx/Desktop/Redo/table1_imputed.csv")
cat("\nSaved to /Users/zjx/Desktop/Redo/table1_imputed.csv\n")