arima_prediction <- function(training_history, product){
    
defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")
    
weekly_backlog_ts = ts(defect_backlog$backlog_all)

forecasts <- c()
actuals <- c()
errors <- c()
models <- c()

for(i in 1:nrow(weeks_to_predict)){
    week_number <- weeks_to_predict[i,1]
    print(week_number)
    actual_backlog_size = defect_backlog %>% filter (number == week_number) %>% select (backlog_all)
    actuals = c(actuals, actual_backlog_size[[1]])
    if ((week_number-training_history)<0){
        forecasts[i] = NA
        errors[i] = NA
        models[i] = NA
    } else {
        fit = auto.arima(subset(weekly_backlog_ts, 
                           start = week_number-training_history,
                           end = week_number))
        fc = forecast(fit,1)    
        forecasts[i] = round(fc$mean)
        ae <- as.integer(forecasts[i]) - as.integer(actuals[i])
        errors[i] = abs(ae)
        p = length(model1$model$phi)
        d = model1$model$Delta
        q = length(model1$model$theta)
        models[i] = paste(p, d, q)
    }
}

result <- data.frame(Week=weeks_to_predict$V1, 
                     Actual=actuals, 
                     Forecast=forecasts,
                     Absolute_Error = errors,
                     p_d_q = models)
    
result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/", "arima_th:", training_history, ".csv", sep="")
write.table(result, file = result_file_path, sep=",")
}