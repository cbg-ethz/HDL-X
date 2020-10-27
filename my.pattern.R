#' This function is adapted from md.pattern() in the mice R package.
my.pattern <- function (x, plot = TRUE, abbrv = NULL) 
{
  if (!(is.matrix(x) || is.data.frame(x))) 
    stop("Data should be a matrix or dataframe")
  if (ncol(x) < 2) 
    stop("Data should have at least two columns")
  R <- is.na(x)
  nmis <- colSums(R)
  R <- matrix(R[, order(nmis)], dim(x))
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse = ""))
  sortR <- matrix(R[order(pat), ], dim(x))
  if (nrow(x) == 1) {
    mpat <- is.na(x)
  }
  else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  if (all(!is.na(x))) {
    cat(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==")
    cat("  No need for mice. This data set is completely observed.\n")
    cat(" \\  \\|/  /\n  `-----'\n\n")
    mpat <- t(as.matrix(mpat, byrow = TRUE))
    rownames(mpat) <- table(pat)
  }
  else {
    if (is.null(dim(mpat))) {
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))
  if(!is.null(abbrv)) {
    colnames(r) <- abbreviate(colnames(r), minlength = abbrv)
  }
  if (plot) {
    plot.new()
    if (is.null(dim(sortR[!duplicated(sortR), ]))) {
      R <- t(as.matrix(r[1:nrow(r) - 1, 1:ncol(r) - 1]))
    }
    else {
      if (is.null(dim(R))) {
        R <- t(as.matrix(R))
      }
      R <- r[1:nrow(r) - 1, 1:ncol(r) - 1]
    }
    par(mar = rep(0, 4))
    plot.window(xlim = c(-1, ncol(R) + 1), ylim = c(-1, nrow(R) + 
                                                      1), asp = 1)
    M <- cbind(c(row(R)), c(col(R))) - 1
    shade <- ifelse(R[nrow(R):1, ], mdc(1), mdc(2))
    rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
    for (i in 1:ncol(R)) {
      text(i - 0.5, nrow(R) + 0.3, colnames(r)[i], adj = c(0, 0), srt = 45, cex = 0.8)
      text(i - 0.5, -0.5, nmis[order(nmis)][i])
    }
    for (i in 1:nrow(R)) {
      text(ncol(R) + 0.3, i - 0.5, r[(nrow(r) - 1):1, ncol(r)][i], 
           adj = 0)
      text(-0.3, i - 0.5, rownames(r)[(nrow(r) - 1):1][i], 
           adj = 1)
    }
    text(ncol(R) + 0.3, -0.3, r[nrow(r), ncol(r)])
    return(r)
  }
  else {
    return(r)
  }
}
