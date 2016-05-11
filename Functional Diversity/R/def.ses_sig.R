## function to calculate median and p-values of SES score

ses_sig <- function(X, nmet) {
  x <- X$SES
  med <- round(median(x), 1)
  pval <- p.adjust(wilcox.test(x)$p.value, method='fdr', n=nmet)
  return(c(med, pval))
}