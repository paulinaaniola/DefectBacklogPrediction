evaluate_autocorrelation <- function (products) {
    product_names <- c()
    autocorrelation_measures <- c()
    for(i in 1:nrow(products)){
       product = products[i,]
       product_autocorrelation_measure = check_backlog_autocorrelation(product)
       autocorrelation_measures = c(autocorrelation_measures, product_autocorrelation_measure)
       product_names = c(product_names, paste(product[1], product[2], sep = "_"))
    }
    result = data.frame(Products = product_names,
                        Percent_of_correlated_lags = autocorrelation_measures)
    write.table(result, file = "data/autocorrelation_evaluation.csv", sep=",")
}    

check_backlog_autocorrelation <- function(product){
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
    print(d)
    
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