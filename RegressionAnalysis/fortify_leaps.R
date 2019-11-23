fortify.regsubsets <- function(model, data, ...){
  require(plyr)
  stopifnot(model$intercept)
  models <- summary(model)$which
  rownames(models) <- NULL
  model_stats <- as.data.frame(summary(model)[c("bic","cp","rss","rsq","adjr2")])
  dfs <- lapply(coef(model, 1:nrow(models)), function(x) as.data.frame(t(x)))
  model_coefs <- plyr::rbind.fill(dfs)
  model_coefs[is.na(model_coefs)] <- 0
  model_stats <- cbind(model_stats, model_coefs)
  # terms_short <- abbreviate(colnames(models))
  terms_short <- colnames(models)
  model_stats$model_words <- aaply(models, 1, function(row) paste(terms_short[row], collapse = "+"))
  model_stats$size <- rowSums(summary(model)$which)
  model_stats
}

get_model_coefs <- function(model){
  models <- summary(model)$which
  dfs <- lapply(coef(model, 1:nrow(models)), function(x) as.data.frame(t(x)))
  model_coefs <- plyr::rbind.fill(dfs)
  model_coefs[is.na(model_coefs)] <- 0
  model_coefs
}