ericsson_prediction <- function(product){
    
defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")
    
weekly_backlog_ts = ts(defect_backlog$backlog_all)

forecasts <- c()
actuals <- c()
errors <- c()

for(i in 1:nrow(weeks_to_predict)){
    week_number <- weeks_to_predict[i,1]
    print(week_number)
    actual_backlog_size = defect_backlog %>% filter (number == week_number) %>% select (backlog_all)
    actuals[i] = actual_backlog_size[[1]]
    if (week_number - 3 < defect_backlog$number[1]){
        forecasts[i] = NA
        errors[i] = NA
    } else {
        db_1 <- (defect_backlog %>% filter (number == week_number-1) %>% select (backlog_all))[[1]]
        di_1 <- (defect_backlog %>% filter (number == week_number-1) %>% select (inflow_all))[[1]]
        di_2 <- (defect_backlog %>% filter (number == week_number-2) %>% select (inflow_all))[[1]]
        di_3 <- (defect_backlog %>% filter (number == week_number-3) %>% select (inflow_all))[[1]]
        do_1 <- (defect_backlog %>% filter (number == week_number-1) %>% select (outflow_all))[[1]]
        do_2 <- (defect_backlog %>% filter (number == week_number-2) %>% select (outflow_all))[[1]]
        do_3 <- (defect_backlog %>% filter (number == week_number-3) %>% select (outflow_all))[[1]]
        di <- (di_1 + di_2 + di_3)/3
        do <- (do_1 + do_2 + do_3)/3
        db <- db_1 + di - do
        forecasts[i] = round(db)
        ae <- as.integer(forecasts[i]) - as.integer(actuals[i])
        errors[i] = abs(ae)
    }
}

result <- data.frame(Week=weeks_to_predict$V1, 
                     Actual=actuals, 
                     Forecast=forecasts,
                     Absolute_Error = errors)
    
result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/", "ericsson.csv", sep="")
write.table(result, file = result_file_path, sep=",")
}