#' This function is adapted from glmnet.lasso() in the stabs R package.
#' It implements the randomised lasso, as presented by Meinshausen and Bühlmann
#' in J. R. Statist. Soc. B (2010), 72, Part 4, pp. 417–473.
randomised_lasso <- function (x, y, q,
                              type = c("conservative", "anticonservative"),
                              weakness = 1, 
                              ...) 
{
  if (!requireNamespace("glmnet", quietly = TRUE)) 
    stop("Package ", sQuote("glmnet"), " needed but not available")
  if (is.data.frame(x)) {
    message("Note: ", sQuote("x"), " is coerced to a model matrix without intercept")
    x <- model.matrix(~. - 1, x)
  }
  if ("lambda" %in% names(list(...))) 
    stop("It is not permitted to specify the penalty parameter ", 
         sQuote("lambda"), " for lasso when used with stability selection.")
  type <- match.arg(type)
  if (type == "conservative") 
    fit <- suppressWarnings(glmnet::glmnet(x, y, pmax = q,
                                           penalty.factor = 1/runif(ncol(x),
                                                                    weakness,1),
                                           ...))
  if (type == "anticonservative") 
    fit <- glmnet::glmnet(x, y, dfmax = q - 1, ...)
  selected <- predict(fit, type = "nonzero")
  selected <- selected[[length(selected)]]
  ret <- logical(ncol(x))
  ret[selected] <- TRUE
  names(ret) <- colnames(x)
  cf <- fit$beta
  sequence <- as.matrix(cf != 0)
  return(list(selected = ret, path = sequence))
}
