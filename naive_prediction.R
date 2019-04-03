naive_prediction <- function(product){
    
defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")
    
weekly_backlog_ts = ts(defect_backlog$backlog_all)

forecasts <- c()
actuals <- c()
errors <- c()

for(i in 1:nrow(weeks_to_predict)){
    week_number <- weeks_to_predict[i,1]
    actual_backlog_size = defect_backlog %>% filter (number == week_number) %>% select (backlog_all)
    actuals = c(actuals, actual_backlog_size[[1]])
    if (week_number == defect_backlog$number[1]){
        forecasts[i] = NA
        errors[i] = NA
    } else if (week_number == defect_backlog$number[2]){
        forecasts[i] = (defect_backlog %>% filter (number == defect_backlog$number[1]) %>% select (backlog_all))[[1]]
        ae <- as.integer(forecasts[i]) - as.integer(actuals[i])
        errors[i] = abs(ae)
    } else {
        model = naive(subset(weekly_backlog_ts, 
                         start = week_number-2,
                         end = week_number))
        fc = forecast(model,1)
        forecasts[i] = round(fc$mean[1])
        ae <- as.integer(forecasts[i]) - as.integer(actuals[i])
        errors[i] = abs(ae)
    }
}

result <- data.frame(Week=weeks_to_predict$V1, 
                     Actual=actuals, 
                     Forecast=forecasts,
                     Absolute_Error = errors)
    
result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/", "naive.csv", sep="")
write.table(result, file = result_file_path, sep=",")
}