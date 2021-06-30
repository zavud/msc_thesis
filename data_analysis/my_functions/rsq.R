rsq = function(preds, actual) {
        rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
        tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
        rsq <- 1 - rss/tss
        return(rsq)
}