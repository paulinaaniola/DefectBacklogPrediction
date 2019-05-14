evaluate_divided_backlog_errors <- function(product) {
    forecasts_path = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions", sep="")

    arima_files <- list.files(path=forecasts_path, pattern="arima*", full.names=TRUE, recursive=FALSE)
    ets_files <- list.files(path=forecasts_path, pattern="ets*", full.names=TRUE, recursive=FALSE)
    ericsson_files <- list.files(path=forecasts_path, pattern="ericsson*", full.names=TRUE, recursive=FALSE)
    naive_files <- list.files(path=forecasts_path, pattern="naive*", full.names=TRUE, recursive=FALSE)

    arima_errors <- c()
    for(i in 1:length(arima_files)) {
        predictions = read.csv(file=arima_files[i], header=TRUE, sep=",")
        arima_errors = c(arima_errors, predictions$error_1)
    }

    ets_errors <- c()
    for(i in 1:length(ets_files)) {
        predictions = read.csv(file=ets_files[i], header=TRUE, sep=",")
        ets_errors = c(ets_errors, predictions$error_1)
    }

    ericsson_errors <- c()
    for(i in 1:length(ericsson_files)) {
        predictions = read.csv(file=ericsson_files[i], header=TRUE, sep=",")
        ericsson_errors = c(ericsson_errors, predictions$error_1)
    }

    naive_errors <- c()
    for(i in 1:length(naive_files)) {
        predictions = read.csv(file=naive_files[i], header=TRUE, sep=",")
        naive_errors = c(naive_errors, predictions$error_1)
    }

    arima_mean_error = round(mean(arima_errors, na.rm=TRUE), 2)
    ets_mean_error = round(mean(ets_errors, na.rm=TRUE), 2)
    ericsson_mean_error = round(mean(ericsson_errors, na.rm=TRUE), 2)
    naive_mean_error = round(mean(naive_errors, na.rm=TRUE), 2)
    
    arima_improvement = round((1 - (arima_mean_error/naive_mean_error))*100, 2)
    ets_improvement =  round((1 - (ets_mean_error/naive_mean_error))*100, 2)
    ericsson_improvement = round((1 - (ericsson_mean_error/naive_mean_error))*100, 2)
    
    mean_errors = c(arima_mean_error, ets_mean_error, ericsson_mean_error, naive_mean_error)
    improvements = c(arima_improvement, ets_improvement, ericsson_improvement, 0)
    result <- rbind(mean_errors, improvements)
    
    rownames(result)[1] = "mean_errors"
    rownames(result)[2] = "improvements"
    colnames(result)=  c("arima_th.all", "ets.all", "ericsson", "naive")

    write.table(result, file = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/errors_evaluation.csv", sep=""), sep=",")  
}