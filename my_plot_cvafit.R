#' Plots mean CV error curves.
#' @param cvafit.object An object returned from glmnetUtils::cvafit.
ggplot_cvafit <- function(cvafit.object) {
  names(cvafit.object$modlist) <- cvafit.object$alpha
  lapply(cvafit.object$modlist, function(mod) {
    tibble(lambda = mod$lambda, `mean CV error` = mod$cvm)}) %>%
    bind_rows(.id = "alpha") %>%
    mutate(alpha = as.numeric(alpha)) %>%
    ggplot(aes(log(lambda), `mean CV error`, group = alpha, colour = alpha)) +
    theme_minimal() +
    labs(x = expression(log(lambda)),
         y = expression("mean" ~ "CV" ~ "error")) +
    geom_line(show.legend = FALSE) +
    scale_color_distiller(palette = "RdYlBu")
}

#' Plots mean CV error at lambda_1se.
#' @param cvafit.object An object returned from glmnetUtils::cvafit.
ggplot_minlossplot <- function(cvafit.object) {
  names(cvafit.object$modlist) <- cvafit.object$alpha
  lapply(cvafit.object$modlist, function(mod) {
    l1se.idx <- which(mod$lambda.1se == mod$lambda)
    tibble(`mean CV error at lambda.1se` = mod$cvm[l1se.idx])}) %>%
    bind_rows(.id = "alpha") %>%
    mutate(alpha = as.numeric(alpha)) %>%
    ggplot(aes(alpha, `mean CV error at lambda.1se`, colour = alpha)) +
    theme_minimal() +
    labs(x = expression(alpha),
         y = expression("mean" ~ "CV" ~ "error" ~ "at" ~ lambda["1se"])) +
    geom_point() +
    scale_color_distiller(palette = "RdYlBu") +
    scale_y_log10()
}

#' Plot above plots side by side.
#' @param enCVobject Output from elastic_net_CV().
multiplot_enCV <- function(enCVobject) {
  grid.arrange(ggplot_cvafit(enCVobject$cvafit),
               ggplot_minlossplot(enCVobject$cvafit) +
                 labs(caption =
                        bquote(Optimal ~ alpha[CV] == .(enCVobject$optimalAlpha))),
               ncol = 2)
}
