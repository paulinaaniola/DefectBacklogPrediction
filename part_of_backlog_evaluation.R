evaluate_divided_backlog_errors <- function(product, comparison_baseline_method) {
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
    
    arima_errors <- arima_errors[!is.na(arima_errors)]  
    ets_errors <- ets_errors[!is.na(ets_errors)]   
    ericsson_errors <- ericsson_errors[!is.na(ericsson_errors)]   

    comparison_baseline_method_errors = c()
    
     if(comparison_baseline_method == "arima"){
         comparison_baseline_method_errors = arima_errors
     } else if(comparison_baseline_method == "ets"){
         comparison_baseline_method_errors = ets_errors  
     } else if (comparison_baseline_method == "naive"){
         comparison_baseline_method_errors = naive_errors  
     } else if (comparison_baseline_method == "ericsson"){
         comparison_baseline_method_errors = ericsson_errors  
     }
    
    arima_mean_error = round(mean(arima_errors, na.rm=TRUE), 2)
    ets_mean_error = round(mean(ets_errors, na.rm=TRUE), 2)
    ericsson_mean_error = round(mean(ericsson_errors, na.rm=TRUE), 2)
    naive_mean_error = round(mean(naive_errors, na.rm=TRUE), 2)
    
    arima_improvement = round((1 - (arima_mean_error/naive_mean_error))*100, 2)
    ets_improvement =  round((1 - (ets_mean_error/naive_mean_error))*100, 2)
    ericsson_improvement = round((1 - (ericsson_mean_error/naive_mean_error))*100, 2)   
    
    #cliff delta and hedges'g coefficients
    arima_cd = cliff.delta(comparison_baseline_method_errors, arima_errors)[[1]]
    arima_nnt = arima_cd^-1
    arima_hg = toString(cohen.d(comparison_baseline_method_errors, arima_errors, hedges.correction=TRUE, na.rm=TRUE)$magnitude)

    ets_cd = cliff.delta(comparison_baseline_method_errors, ets_errors)[[1]]
    ets_nnt = ets_cd^-1
    ets_hg = toString(cohen.d(comparison_baseline_method_errors, ets_errors, hedges.correction=TRUE, na.rm=TRUE)$magnitude)
    
    ericsson_cd = cliff.delta(comparison_baseline_method_errors, ericsson_errors)[[1]]
    ericsson_nnt = ericsson_cd^-1
    ericsson_hg = toString(cohen.d(comparison_baseline_method_errors, ericsson_errors, hedges.correction=TRUE, na.rm=TRUE)$magnitude)
    
    #wilcoxon test
    #In order to make wilcoxon test length of naive errors adjusted to the length of method errors
    arima_n_error = comparison_baseline_method_errors
    ets_n_error = comparison_baseline_method_errors
    ericsson_n_error = comparison_baseline_method_errors

    if(length(comparison_baseline_method_errors) > length(arima_errors)){
       arima_n_error <- tail(comparison_baseline_method_errors, n=length(arima_errors))
    } else {
       arima_errors <- tail(arima_errors, n=length(comparison_baseline_method_errors))
    }
    arima_p = wilcox.test(arima_n_error, arima_errors, paired=TRUE)$p.value
    arima_wilcoxon_test = arima_p<0.05
    
    if(length(comparison_baseline_method_errors) > length(ets_errors)){
       ets_n_error <- tail(comparison_baseline_method_errors, n=length(ets_errors))
    } else {
       ets_errors <- tail(ets_errors, n=length(comparison_baseline_method_errors))
    }    
    ets_n_error <- tail(comparison_baseline_method_errors, n=length(ets_errors))
    ericsson_n_error <- tail(comparison_baseline_method_errors, n=length(ericsson_errors))
    
    ets_p = wilcox.test(ets_n_error, ets_errors, paired=TRUE)$p.value
    ets_wilcoxon_test = ets_p<0.05
    
    if(length(comparison_baseline_method_errors) > length(ericsson_errors)){
       ericsson_n_error <- tail(comparison_baseline_method_errors, n=length(ericsson_errors))
    } else {
       ericsson_errors <- tail(ericsson_errors, n=length(comparison_baseline_method_errors))
    }  
    ericsson_p = wilcox.test(ericsson_n_error, ericsson_errors, paired=TRUE)$p.value
    ericsson_wilcoxon_test = ericsson_p<0.05
    
    mean_errors = c(arima_mean_error, ets_mean_error, ericsson_mean_error, naive_mean_error)
    improvements = c(arima_improvement, ets_improvement, ericsson_improvement, 0) 
    hedges_g = c(arima_hg, ets_hg, ericsson_hg, NA)
    cliff_delta = c(get_cd_magnitude(arima_cd), get_cd_magnitude(ets_cd), get_cd_magnitude(ericsson_cd), NA)
    nnt = c(arima_nnt, ets_nnt, ericsson_nnt, NA)
    wilcoxon_test = c(arima_wilcoxon_test, ets_wilcoxon_test, ericsson_wilcoxon_test, FALSE)
    
    result <- data.frame(Method = c("arima_th:all", "ets_th:all", "ericsson", "naive"),
                             Error_1 = mean_errors,
                             Impr_1 = improvements,
                             Cliff_delta = cliff_delta,
                             Hedges_g = hedges_g,
                             NNT = nnt,
                             Wilcoxon_test = wilcoxon_test)                            

    write.table(result, file = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/", comparison_baseline_method, "_baseline_predictions_comparison.csv", sep=""), sep=",")  
}

get_cd_magnitude <- function(cd){
    magnitude = ""
    if(cd<0.112){
       magnitude = "negligible"
    } else if(0.112<=cd & cd <0.276) {
       magnitude = "small"
    } else if(0.112<= cd & cd <0.428){
       magnitude = "medium"
    } else if (cd>=0.428){
       magnitude = "large"
    }
    return(magnitude)
}