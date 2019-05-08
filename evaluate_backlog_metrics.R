evaluate_backlog_metrics <- function (products) {
    product_names <- c()
    autocorrelation_measures <- c()
    backlog_mean_size <- c()
    mean_backlog_size_change <- c()
    for(i in 1:nrow(products)){
       product = products[i,]
       defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
       backlog_mean_size = c(backlog_mean_size, round(mean(defect_backlog$backlog_all), 2))      
       
       product_autocorrelation_measure = get_backlog_autocorrelation(product)
       autocorrelation_measures = c(autocorrelation_measures, product_autocorrelation_measure)
        
       backlog_size_change = round(get_mean_backlog_size_change(defect_backlog), 2)
       mean_backlog_size_change = c(mean_backlog_size_change, backlog_size_change )
        
       product_names = c(product_names, paste(product[1], product[2], sep = "_"))
    }
    result = data.frame(Products = product_names,
                        Percent_of_autocorrelation_that_exceeds_sign_bounds = autocorrelation_measures,
                        Mean_backlog_size = backlog_mean_size,
                        Mean_backlog_size_change = mean_backlog_size_change)
    write.table(result, file = "data/backlogs_metrics.csv", sep=",")
}    

get_backlog_autocorrelation <- function(product){
    product_path = paste("data/", product[1], "/", product[2], "/", sep="")
    defect_backlog = read.csv(file=paste(product_path, product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
    component_evaluation = read.csv(file=paste(product_path, "/predictions_evaluation.csv", sep=""), header=TRUE, sep=",")
    
    best_arima_training_history = (component_evaluation %>% filter (str_detect(Method, "arima")) 
                                   %>% filter (Error_1 ==  min(Error_1))
                                   %>% select (Method))[[1]]
        
    best_arima_predictions = read.csv(file=paste(product_path, "Predictions/", best_arima_training_history, ".csv", sep=""), header=TRUE, sep=",")
    most_frequent_p_d_q <- tail(names(sort(table(best_arima_predictions$p_d_g))), 1)
    d <- strsplit(most_frequent_p_d_q, " ", fixed=TRUE)[[1]][[2]]
    d <- strtoi(d)
    
    backlog_all <- defect_backlog$backlog_all
    number_of_diff = d
    while(number_of_diff > 0){
        backlog_all = diff(backlog_all)
        number_of_diff = number_of_diff-1
    } 
    ts_length = length(backlog_all)
    autocorelation = acf(backlog_all, lag.max=ts_length, plot = FALSE)
    acf = autocorelation$acf
    bound = 2/sqrt(length(defect_backlog$backlog_all))
    number_of_correlated_lags = 0
    for (i in 1:length(acf)){
        if (abs(acf[i]) > bound) number_of_correlated_lags = number_of_correlated_lags +1
    } 
    return(round(number_of_correlated_lags/ts_length * 100, 2))
}

get_mean_backlog_size_change <- function(defect_backlog){
    backlog_size_diff = c()
    for (i in 2:nrow(backlog)){
        previous_week = defect_backlog[i-1,"backlog_all"]
        current_week = defect_backlog[i,"backlog_all"]
        backlog_size_diff = c(backlog_size_diff, abs(previous_week-current_week) )
    }    
    return(mean(backlog_size_diff, na.rm=TRUE))
}
