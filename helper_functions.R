#' Augment master data frame with column recording condition.
#' @param dataFrame A spread master data frame.
#' @return Augmented data frame.
augment <- function(dataFrame) {
  dataFrame %>%
    mutate(condition = ifelse(
      !`Diabetes` & !`CHD`, "Healthy", ifelse(
        `Diabetes` & !`CHD`, "Diabetes", ifelse(
          !`Diabetes` & `CHD`, "CHD", ifelse(
            `Diabetes` & `CHD`, "Diabetes and CHD", NA))))) %>%
    select(-`Diabetes`, -`CHD`, -`Proband ID`)
}

#' Get design matrix for a given disease condition.
#' @param disease A string denoting disease, e.g. "Diabetes and CHD".
#' @return A list containing response vector and predictor matrix.
getDesign <- function(disease) {
  design <- master.aug %>%
    filter(condition %in% c("Healthy", disease)) %>%
    # Keep only features with more than one unique value in the sub-selection.
    Filter(function(x) length(unique(x)) > 1, .) %>%
    model_matrix(formula = ~ .-1)
  resp <- design %>%
    mutate(affliction = 1 - conditionHealthy) %>%
    pull(affliction)
  preds <- design %>%
    select(-contains("condition")) %>%
    as.matrix()
  colnames(preds) <- gsub("`", "", colnames(preds))
  list(response = resp, predictors = preds)
}

#' Convert a string into an integer, for use as a seed.
#' @param string A string.
#' @return An integer.
stringToSeed <- function(string) {
  strsplit(gsub(" ", "", string), "")[[1]] %>%
    sapply(function(char) grep(char, c(LETTERS, letters))) %>%
    sum()
}
