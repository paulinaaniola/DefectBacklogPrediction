arima_prediction <- function(product, training_history, number_of_weeks_to_predict){
    
defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")
    
weekly_backlog_ts = ts(defect_backlog$backlog_all)
models <- c()
for(j in 1:number_of_weeks_to_predict){
    assign(paste('forecast', j, sep='_'), c())
    assign(paste('actual', j, sep='_'), c())
    assign(paste('error', j, sep='_'), c())
}

for(i in 1:nrow(weeks_to_predict)){
    week_number <- weeks_to_predict[i,1]
    if ((week_number-training_history)<0){
        models[i] = NA
        for(j in 1:number_of_weeks_to_predict){
            forecasts_j = get(paste("forecast", j, sep = "_"))
            assign(paste('forecast', j, sep='_'), c(forecasts_j, NA))

            actual_backlog_size = (defect_backlog %>% filter (number == (week_number + j -1)) %>% select (backlog_all))[[1]]
            actuals_j = get(paste("actual", j, sep = "_"))
            assign(paste('actual', j, sep='_'), c(actuals_j, actual_backlog_size))

            errors_j = get(paste("error", j, sep = "_"))
            assign(paste('error', j, sep='_'), c(errors_j, NA ))
        }
    } else {
        fit = auto.arima(subset(weekly_backlog_ts, 
                               start = week_number-training_history,
                               end = week_number))
        fc = forecast(fit, number_of_weeks_to_predict)
#         p = length(fit$model$phi)
#         d = fit$model$Delta
#         q = length(fit$model$theta)
        arma = fit$arma
        models[i] = paste(arma[1], arma[6], arma[2])
        for(j in 1:number_of_weeks_to_predict){
            forecasted_value = round(fc$mean[j])
            forecasts_j = get(paste("forecast", j, sep = "_"))
            assign(paste('forecast', j, sep='_'), c(forecasts_j, forecasted_value))

            actual_backlog_size = (defect_backlog %>% filter (number == (week_number + j -1)) %>% select (backlog_all))[[1]]
            actuals_j = get(paste("actual", j, sep = "_"))
            assign(paste('actual', j, sep='_'), c(actuals_j, actual_backlog_size))

            ae = abs(forecasted_value - actual_backlog_size)
            errors_j = get(paste("error", j, sep = "_"))
            assign(paste('error', j, sep='_'), c(errors_j, ae ))
        }
    }
}

result <- data.frame(Week = weeks_to_predict,
                      p_d_g = models)
    
for(j in 1:number_of_weeks_to_predict){
    forecasts_j = get(paste("forecast", j, sep = "_"))
    actuals_j = get(paste("actual", j, sep = "_"))
    errors_j = get(paste("error", j, sep = "_"))
    results_j = data.frame(Week = weeks_to_predict)

    if(j == 1){
        results_j[, "Actual"] = actuals_j
        results_j[, "Forecast"] = forecasts_j
        results_j[, "Error"] = errors_j
    } else {
        results_j[, paste("Actual", j-1, sep="+")] = actuals_j
        results_j[, paste("Forecast", j-1, sep="+")] = forecasts_j
        results_j[, paste("Error", j-1, sep="+")] = errors_j
    }
    result <- merge(result, results_j, all = TRUE)
} 
colnames(result)[1] <- "Week"
result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/", "arima_th:", training_history, ".csv", sep="")
write.table(result, file = result_file_path, sep=",")
}