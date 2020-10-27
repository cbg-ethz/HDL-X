require(glmnet)
require(glmnetUtils)
require(stabs)
require(parallel)

#' Logistic regression stability selection for specified disease condition.
#' @param disease A string denoting disease, e.g. "Diabetes and CHD".
#' @param use.saved.result Load result from hard-coded file path?
#' @param save.result Save result to hard-coded file path?
#' @elastic elastic net parameter alpha in glmnet.
#' @pfer Bound for expected number of false positives.
#' @ctff Cutoff parameter for stabsel.
#' @seed Random seed.
#' @return Augmented stabsel object containing title etc. for plotting.
stabDisease <- function(disease, use.saved.result = FALSE, save.result = FALSE,
                        elastic = 0.95, pfer = 2, ctff = 0.6, seed = NULL) {
  diseaseCollapse <- paste(disease, collapse = " + ")
  filepath <- paste0(paste("computations/stabs", diseaseCollapse, sep = "_"), ".rds")
  if(use.saved.result) return(readRDS(filepath))
  design <- getDesign(disease)
  set.seed(seed)
  stabby <- stabsel(design$predictors, design$response, cutoff = ctff,
                    PFER = pfer, fitfun = glmnet.lasso,
                    args.fitfun = list(family = "binomial", alpha = elastic),
                    assumption = "r-concave", sampling.type = "SS",
                    mc.cores = detectCores(logical = TRUE))
  stabby$n <- nrow(design$predictors)
  stabby$spearman <- cor(design$predictors, design$response, method="spearman")
  stabby$title <- diseaseCollapse
  if(save.result) saveRDS(stabby, filepath)
  stabby
}

#' Run cva.glmnet for logistic regression on a given disease condition.
#' @param disease A string denoting disease, e.g. "Diabetes and CHD".
#' @param seed A random seed.
#' @param alphas A sequence of values for the elastic net parameter alpha.
elastic_net_CV <- function(disease, seed = NULL, alphas = seq(0, 1, len = 101)) {
  design <- getDesign(disease)
  # set.seed(stringToSeed(disease))
  set.seed(seed)
  cl <- makePSOCKcluster(detectCores())
  cvafit <- cva.glmnet(x = design$predictors, y = design$response,
                       family = "binomial", alpha = alphas,
                       outerParallel = cl)
  stopCluster(cl)
  temp <- sapply(cvafit$modlist, function(mod) {
    mod$cvm[mod$lambda == mod[["lambda.1se"]]]
  })
  opt.idx <- which.min(temp)
  optimalAlpha <- cvafit$alpha[opt.idx]
  cvfit <- cvafit$modlist[[opt.idx]]
  glmnet.fit <- cvfit$glmnet.fit
  list(design = design, cvafit = cvafit, optimalAlpha = optimalAlpha,
       opt.idx = opt.idx, cvfit = cvfit, fit = glmnet.fit)
}
