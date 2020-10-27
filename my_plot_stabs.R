plot.stabs <- function(so) {
  # Only plot features which are selected at least once
  nonTrivial <- names(which(rowSums(so$phat) > 0))
  steps <- ncol(so$phat) - 1
  stab.tidy <- rownames_to_column(as.data.frame(so$phat), var = "assay") %>%
    as_tibble %>%
    filter(assay %in% nonTrivial) %>%
    gather(key, phat, -assay) %>%
    mutate(step = steps - parse_number(key),
           assay = gsub("`", "", assay))
  
  basePlot <- ggplot(stab.tidy, aes(step, phat, group = assay)) +
    theme_classic() +
    geom_hline(yintercept = so$cutoff, colour = "darkred",
               lwd = 0.5, lty = "dotted") +
    ylim(0, 1) +
    labs(title = paste0(so$title, ": stability paths"),
         subtitle = paste("Expected number of false selections is at most",
                          so$specifiedPFER),
         caption = paste0("n = ", so$n, ", p = ", so$p),
         x = "Regularisation (step)",
         y = "Selection probability",
         colour = "Selected predictors")
  if(length(so$selected) > 0) {
    basePlot <- basePlot +
      geom_line(aes(colour = ifelse(assay %in% gsub("`", "", names(so$selected)), assay, NA)),
                show.legend = TRUE)
  } else basePlot <- basePlot + geom_line(colour = "grey50")
  basePlot
}

plot.stabs.maxsel <- function(so) {
  cors <- so$spearman
  sources <- bind_cols(feature = gsub("`", "", rownames(cors)),
                       "Correlation with response" = cors) %>%
    left_join(features, by = "feature")
  so.maxSelProbs <- 
    rownames_to_column(data.frame(maxsel = so$max), var = "feature") %>%
    as_tibble %>%
    rename("Maximum selection probability" = maxsel) %>%
    right_join(sources, by = "feature") %>%
    arrange(-desc(`Maximum selection probability`)) %>%
    mutate(feature = factor(feature, levels = feature)) %>%
    rename("Data source" = source)
  
  basePlot <- ggplot(
    top_n(so.maxSelProbs, 25, `Maximum selection probability`),
    aes(x = `Maximum selection probability`, y = feature,
        colour = `Correlation with response`, shape = `Data source`)) +
    theme_linedraw() +
    xlim(c(0,1)) +
    geom_vline(aes(xintercept = so$cutoff, colour = so$cutoff),
               linetype = "dotted") +
    # geom_hline(aes(yintercept = feature)) +
    scale_colour_distiller(limits = c(-1, 1), type = "seq",
                           palette = "RdYlBu", direction = -1) +
    # scale_shape_identity() +
    geom_point(size = 3) +
    labs(title = paste0(so$title, ": maximum selection probabilities"),
         y = "Feature")
  basePlot
}
