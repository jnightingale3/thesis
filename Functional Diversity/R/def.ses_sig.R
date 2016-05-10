## function to calculate median and p-values of SES score

ses_sig <- function(X) {
  x <- X$SES
  med <- round(median(x), 1)
  lineup <- sort(c(x, 0), decreasing = (med < 0))
  pval <- (which(lineup == 0) - 1) / (length(x) / 2)
  if(pval > 1) {pval <- 1}
  return(c(med, pval))
}