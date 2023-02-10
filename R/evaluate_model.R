#' @title Evaluate model 
#' 
#' @description La fonction est utilisée pour évaluer un modèle donné en utilisant une mesure de qualité, RMSE.
#' 
#' @param model model that you want to train from caret package (see names(getModelInfo()))
#' @param mydata data.frame containing value and time columns
#' 
#' @return RMSE
#' 
#' @import caret
#' @import forecast
#' 
#' @examples
#' evaluate_model("rf", data)
#' 
#' @export
evaluate_model <- function(model, mydata, ntree = NULL) {
  set.seed(123)
  if (model %in% c("slm", "spm")) {
    missing_rows <- sample(1:nrow(mydata), size = round(nrow(mydata) * 0.7))
    training <- mydata
    training$value[missing_rows] <- NA
    testing <- mydata$value[missing_rows]
    if(model == "slm") pred <- zoo::na.approx(training$value)[missing_rows]
    if(model == "spm") pred <- zoo::na.spline(training$value)[missing_rows]
    acc <- round(RMSE(pred, testing, na.rm = TRUE), 3)
  } else {
    trainIndex <- createDataPartition(mydata$value, p = 0.7, list = FALSE)
    training <- mydata[ trainIndex,]
    testing <- mydata[-trainIndex,]
    if(model == "rf") fit <- train(value ~ time, data = training, method = model, ntree = ntree)
    if(!model %in% c("rf")) fit <- train(value ~ time, data = training, method = model)
    pred <- predict(fit, newdata = testing)
    acc <- round(RMSE(as.numeric(pred), testing$value), 3)
  }
  return(acc)
}