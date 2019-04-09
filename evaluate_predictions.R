evaluate <- function (product) {
    
    forecasts_path = paste("data/", product[1], "/", product[2], "/Predictions", sep="")
    files <- list.files(path=forecasts_path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
    files = mixedsort(files)
    
    first_results = read.csv(file=files[1], header=TRUE, sep=",")
    col_names = colnames(first_results)
    last_error = tail(col_names, n=1)
    number_of_predicted_weeks = (strsplit(last_error, "_", fixed=TRUE))[[1]][2]
    number_of_predicted_weeks = as.integer(number_of_predicted_weeks)
    
    methods <- c()
    
    for(j in 1:number_of_predicted_weeks){
        assign(paste('mean_ae', j, sep='_'), c())
        assign(paste('improvement', j, sep='_'), c())
    }

    for(i in 1:length(files)) {
        x = files[i]
        end_of_file_name = strsplit(x, "/Predictions/", fixed=TRUE)
        method_name = (strsplit(end_of_file_name[[1]][2], ".", fixed=TRUE))[[1]][1]
        predictions = read.csv(file=x, header=TRUE, sep=",")           
        methods = c(methods, method_name)
        
         for(j in 1:number_of_predicted_weeks){
            mean_ae_j = get(paste("mean_ae", j, sep = "_"))

            if(j>1 && (method_name == "naive" | method_name == "ericsson")){
               assign(paste('mean_ae', j, sep='_'), c(mean_ae_j, NA))
            } else {
                error_column_name = paste("Error", j, sep="_")
                ae_j = mean(predictions[, error_column_name], na.rm=TRUE) 
                assign(paste('mean_ae', j, sep='_'), c(mean_ae_j, ae_j))
            }
        }
    }
    
    errors <- data.frame(Method = methods)
        
    for(j in 1:number_of_predicted_weeks){
        mean_ae_j = get(paste("mean_ae", j, sep = "_"))
        errors_j = data.frame(Method = methods)
        errors_j[, paste("Error", j, sep="_")] = mean_ae_j
        errors <- merge(errors, errors_j, all = TRUE)
    }  
    
    naive_ae = (errors %>% filter (Method == 'naive') %>% select (Error_1))[[1]]
    
    for(i in 1:nrow(errors)) {
        method_name = errors[i, 1]
        for(j in 1:number_of_predicted_weeks){         
            improvement_j = get(paste("improvement", j, sep = "_"))

            if(j>1 && (method_name == "naive" | method_name == "ericsson")){
               assign(paste('improvement', j, sep='_'), c(improvement_j, NA))
            } else {
                error_column_name = paste("Error", j, sep="_")
                method_mean_error = errors[i, error_column_name]
                impr_j = (1 - as.integer(method_mean_error)/as.integer(naive_ae))*100
                # remove after decision what to do with naive forecasting 2,3,4 weeks forward
                impr_j = ifelse(j>1, NA, impr_j)
                assign(paste('improvement', j, sep='_'), c(improvement_j, impr_j))
            }
         }
     }
        
     result <- data.frame(Method = methods)
   
     for(j in 1:number_of_predicted_weeks){
        mean_ae_j = get(paste("mean_ae", j, sep = "_"))
        improvement_j = get(paste("improvement", j, sep = "_"))
        result_j = data.frame(Method = methods)
        result_j[, paste("Error", j, sep="_")] = mean_ae_j
        result_j[, paste("Improvement", j, sep="_")] = improvement_j
        result <- merge(result, result_j, all = TRUE)
     }  
        
    result_file_path = paste("data/", product[1], "/", product[2], "/predictions_evaluation.csv", sep="")
    write.table(result, file = result_file_path, sep=",")
}                        