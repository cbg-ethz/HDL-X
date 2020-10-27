#' Add LLoQ per feature, log2 transform, and left_join dictionary.
#' 
#' @param tidyDF A tidy tibble with columns `HDL No`, `key`, and `value`.
#' @return A spread data frame with `HDL No` as a column.
log_and_add_covariates <- function(tidyDF, join.by = "HDL No") {
  tidyDF %>%
    group_by(key) %>%
    mutate(LLOQ = ifelse(any(value == 0),
                         min(setdiff(value, c(0, 1)), na.rm = TRUE), 0)) %>%
    ungroup() %>%
    # Add a pseudo-count to allow for logarithmic transformation
    mutate(value = log2(value + LLOQ)) %>%
    select(-LLOQ) %>%
    # Include covariates recorded in the dictionary (round is redundant)
    left_join(covariates %>% select(-round), by = join.by) %>%
    # Keep only patient samples
    filter(str_detect(`Proband ID`, "^HDLX-"))
}

#' Nest based on key, and fit linear models.
#' 
#' @param dF.aug Output from `log_and_add_covariates`.
#' @param isolated A boolean indicating whether HDL isolation was performed.
nested_lm_fits <- function(dF.aug, isolated = TRUE) {
  # If samples were isolated (for HDL), then adjust for rotor and date.
  formel <- ifelse(
    isolated,
    "value ~ Diabetes + CHD + hospital + Female + rotor + `Start isolation`",
    "value ~ Diabetes + CHD + hospital + Female")
  dF.aug %>%
    group_by(key) %>%
    nest() %>%
    mutate(linMod = map(data, function(dF) {
      lm(as.formula(formel), data = dF)
    }))
}

#' Use `broom` to extract residuals and fixed effects.
#' 
#' @param nested.lm.fits Output from `nested_lm_fits`.
lm_brooms <- function(nested.lm.fits) {
  nested.lm.fits %>%
    mutate(aug = map(linMod, broom::augment),
           tid = map(linMod, broom::tidy),
           aug.with.HDL_No = pmap(.l = list(data, aug), .f = function(x, y) {
             bind_cols(x %>% select(`HDL No`, `Proband ID`), y)}))
}

#' Unnest `lm.brooms` and adjust p-values using Benjamini-Hochberg's procedure.
#' 
#' @param lm.brooms Output from `lm_brooms`.
#' @return Unnested `lm.brooms` object with adjusted p-values.
lm_brooms_adjusted <- function(lm.brooms) {
  lm.brooms %>%
    unnest(tid) %>%
    filter(term %in% c("CHDTRUE", "DiabetesTRUE")) %>%
    group_by(key) %>%
    mutate(p.adjusted = p.adjust(p.value, method = "fdr")) %>%
    ungroup()
}

#' Adjust data by adding back fixed effects of disease.
#' 
#' @param lm.brooms Output from `lm_brooms`.
lm_brooms_normalise <- function(lm.brooms) {
  left_join(
    lm.brooms %>% unnest(aug.with.HDL_No),
    lm.brooms %>% unnest(tid) %>% select(key, term, estimate) %>%
      spread(term, estimate) %>%
      select(key, `(Intercept)`, CHDTRUE, DiabetesTRUE),
    by = "key") %>%
    mutate(value = `(Intercept)` + .resid +
             CHD * CHDTRUE + Diabetes * DiabetesTRUE) %>%
    select(`Proband ID`, `HDL No`, key, value)
}
