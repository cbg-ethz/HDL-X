#' Draw volcano plot.
#' 
#' @param lm.brooms Output from `lm_brooms`.
#' @param titl A string used for the plot title.
#' @param isolated A boolean indicating whether HDL isolation was performed.
plot_volcano <- function(lm.brooms.adjusted, titl = "", isolated = TRUE) {
  subtitl <- ifelse(
    isolated,
    "Adjusted for hospital, gender, HDL isolation date and rotor",
    "Adjusted for hospital and gender")
  if(isolated) {
    temp <- lm.brooms.adjusted %>% mutate(term = paste("HDL", term))
  } else {temp <- lm.brooms.adjusted}
  temp %>%
    # filter(str_detect(term, c("CHD")) | str_detect(term, c("Diabetes"))) %>%
    mutate(term = sub("TRUE", "", term),
           term = sub("Diabetes", "T2DM", term)) %>%
    ggplot(aes(estimate, -log10(p.adjusted), shape = p.adjusted < 0.05,
               colour = p.adjusted < 0.05)) +
    theme_bw() +
    # theme(legend.position = "top") +
    labs(#title = titl, subtitle = subtitl,
         x = expression("adjusted" ~ log[2] ~ "fold change"),
         y = expression(-log[10]("FDR-adjusted p-value"))) +
    geom_vline(xintercept = 0, colour = "lightgrey") +
    geom_point(size = 0.5, show.legend = F) +
    # geom_text(aes(label = key, colour = p.adjusted < 0.05)) +
    geom_text_repel(aes(label = ifelse(p.adjusted < 0.05, key, NA)), seed = 42, cex = 2,
                    vjust = 1.5, show.legend = FALSE, segment.alpha = .1,
                    box.padding = 0.075, na.rm = TRUE) +
    scale_color_manual(values = c("grey", "black")) +
    facet_grid(term ~ .,  scales = "free_y", space = "free_y")
}

#' Print names of statistically significant omics by increasing fold change.
#' 
#' @param lm.brooms.adjusted Output from `lm_brooms_adjusted`.
print_significant_omics <- function(lm.brooms.adjusted) {
  cat("\\small{The following have FDR-adjusted $p$-values smaller than 5\\%,")
  cat(" and are listed by increasing $\\log_2$ fold change in parenthesis.")
  list_omics_by_fc(lm.brooms.adjusted, "CHDTRUE")
  list_omics_by_fc(lm.brooms.adjusted, "DiabetesTRUE")
  cat("}")
}

list_omics_by_fc <- function(lm.brooms.FDR, effect) {
  temp <- lm.brooms.FDR %>%
    filter(term == effect, p.adjusted < 0.05) %>%
    mutate(keyFC = paste0(key, " (", ifelse(estimate < 0, "$", ""),
                          signif(estimate, 2), ifelse(estimate < 0, "$", ""), ")"))
  cat("\n\n\\subsubsection*{", sub("TRUE", "", effect), "}\n", sep = "")
  temp %>%
    filter(estimate < 0) %>%
    arrange(estimate) %>%
    pull(keyFC) %>%
    cat(cat("\n\\textbf{Negative $\\log_2$ fold change (", length(.), " features)}:", sep = ""),
        ., sep = ", ")
  temp %>%
    filter(estimate > 0) %>%
    arrange(estimate) %>%
    pull(keyFC) %>%
    cat(cat(".\n\n\\textbf{Positive $\\log_2$ fold change (", length(.), " features)}:", sep = ""),
        ., sep = ", ")
  cat(".")
}
