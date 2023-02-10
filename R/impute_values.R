#' @title Impute values
#'
#' @description Interpolates missing values. Can either use machine learning models or interpolation methods (spline and approximation) to impute the missing values.
#'
#' @param model a string representing the model that you want to train from the caret package (see names(getModelInfo()))
#' @param mydata a data.frame containing the columns 'value' and 'time'
#' @param ntree an optional integer argument for the number of trees to use in a random forest model
#'
#' @import caret
#' @import zoo
#'
#' @examples
#' impute_values("rf", data)
#'
#' @export
impute_values <- function(model, mydata, ntree = NULL) {
  set.seed(123)
  if (model %in% c("slm", "spm")) {
    if(model == "slm") imputed_values <- zoo::na.approx(mydata$value)
    if(model == "spm") imputed_values <- zoo::na.spline(mydata$value)
    mydata$is_imputed <- ifelse(is.na(mydata$value), 1, 0)
    mydata$value <- imputed_values
    mydata
  } else {
    if(model == "rf") fit <- train(value ~ time, data = na.omit(mydata), method = model, ntree = ntree)
    if(!model %in% c("rf")) fit <- train(value ~ time, data = na.omit(mydata), method = model)
    imputed_values <- predict(fit, newdata = mydata[which(is.na(mydata$value)), ])
    mydata$is_imputed <- ifelse(is.na(mydata$value), 1, 0)
    mydata$value[which(is.na(mydata$value))] <- imputed_values
    mydata
  }
}