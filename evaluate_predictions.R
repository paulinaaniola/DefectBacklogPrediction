evaluate_predictions <- function (products) {
    for(i in 1:nrow(products)){
        evaluate_single_product_predictions(products[i,])
    }
    calculate_mean_assesment_measures(products)  
}    

evaluate_single_product_predictions <- function (product) {
    
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
        assign(paste('errors_autocorrelation', j, sep='_'), c())
    }

    for(i in 1:length(files)) {
        x = files[i]
        end_of_file_name = strsplit(x, "/Predictions/", fixed=TRUE)
        method_name = (strsplit(end_of_file_name[[1]][2], ".", fixed=TRUE))[[1]][1]
        predictions = read.csv(file=x, header=TRUE, sep=",")           
        methods = c(methods, method_name)
        
         for(j in 1:number_of_predicted_weeks){
            mean_ae_j = get(paste("mean_ae", j, sep = "_"))
            errors_autocorrelation_j = get(paste("errors_autocorrelation", j, sep = "_"))

            if(j>1 && (method_name == "naive" | method_name == "ericsson")){
               assign(paste('mean_ae', j, sep='_'), c(mean_ae_j, NA))
               assign(paste('errors_autocorrelation', j, sep='_'), c(errors_autocorrelation_j, NA))
            } else {
                error_column_name = paste("Error", j, sep="_")
                error_autocorrelation_column_name = paste("Error_Autocorrelation", j, sep="_")
                ae_j = mean(predictions[, error_column_name], na.rm=TRUE) 
                
                errors = predictions[, error_column_name]
                errors <- errors[!is.na(errors)]
                ljung_box_test = Box.test(predictions[, error_column_name], lag=(length(errors)-2), type="Ljung-Box")
                
                ae_j = round(ae_j, 2)
                acorr_j = ljung_box_test$p.value>0.05
                
                assign(paste('mean_ae', j, sep='_'), c(mean_ae_j, ae_j))
                assign(paste('errors_autocorrelation', j, sep='_'), c(errors_autocorrelation_j, acorr_j))
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
                impr_j = ifelse(j>1, NA, round(impr_j, 2))
                assign(paste('improvement', j, sep='_'), c(improvement_j, impr_j))
            }
         }
     }
        
     result <- data.frame(Method = methods)
   
     for(j in 1:number_of_predicted_weeks){
        mean_ae_j = get(paste("mean_ae", j, sep = "_"))
        acorr_j = get(paste('errors_autocorrelation', j, sep = "_"))
        improvement_j = get(paste("improvement", j, sep = "_"))
        result_j = data.frame(Method = methods)
        result_j[, paste("Error", j, sep="_")] = mean_ae_j
        result_j[, paste("Impr", j, sep="_")] = improvement_j
        result_j[, paste("Independent_errors", j, sep="_")] = acorr_j
        result <- merge(result, result_j, all = TRUE)
     }  
        
    result_file_path = paste("data/", product[1], "/", product[2], "/predictions_evaluation.csv", sep="")
    write.table(result, file = result_file_path, sep=",")
} 

calculate_mean_assesment_measures <- function(products){
    first_product = products[1,]
    first_product_path = paste("data", first_product[1], first_product[2], "predictions_evaluation.csv", sep = "/")
    first_component_evaluation = read.csv(file=first_product_path, header=TRUE, sep=",") 
    methods = first_component_evaluation$Method
    methods = mixedsort(methods)
    
    improvements_result = matrix(NA, 0, length(methods))
    colnames(improvements_result) <- methods

    errors_result = matrix(NA, 0, length(methods))
    colnames(errors_result) <- methods
    
    indep_errors_result = matrix(NA, 0, length(methods))
    colnames(indep_errors_result) <- methods
    
    for(i in 1:nrow(products)){
        product = products[i,]
        product_path = paste("data", product[1], product[2], "predictions_evaluation.csv", sep="/")    
        component_evaluation = read.csv(file=product_path, header=TRUE, sep=",")
        improvements = c()
        errors = c()
        indep_errors = c()
        for(j in 1:length(methods)){
            method_improvement = (component_evaluation %>% filter (Method == methods[j]) %>% select (Impr_1))[[1]]
            improvements = c(improvements, method_improvement)
            
            method_error = (component_evaluation %>% filter (Method == methods[j]) %>% select (Error_1))[[1]]
            errors = c(errors, method_error)
            
            method_indep_errors = (component_evaluation %>% filter (Method == methods[j]) %>% select (Independent_errors_1))[[1]]
            indep_errors = c(indep_errors, method_indep_errors)
        }
        improvements_result = rbind(improvements_result, improvements)
        rownames(improvements_result)[i] = paste(product[1], product[2], sep="_")

        errors_result = rbind(errors_result, errors)
        rownames(errors_result)[i] = paste(product[1], product[2], sep="_")
        
        indep_errors_result = rbind(indep_errors_result, indep_errors)
        rownames(indep_errors_result)[i] = paste(product[1], product[2], sep="_")
    }

    mean_improvement <- c()
    improvements_standard_deviation <- c()
    
    mean_error <- c()
    errors_standard_deviation <- c()
    
    number_of_independent_errors <- c()
    number_of_dependent_errors <- c()
   
    for(i in 1:ncol(improvements_result)){
        method_improvements = improvements_result[,i]
        mean_improvement = c(mean_improvement, mean(method_improvements))
        improvements_standard_deviation = c(improvements_standard_deviation, sqrt(var(method_improvements)))
        
        method_errors = errors_result[,i]
        mean_error = c(mean_error, mean(method_errors))
        errors_standard_deviation = c(errors_standard_deviation, sqrt(var(method_errors)))   
        
        method_indep_errors = indep_errors_result[,i]
        error_indep <- table(method_indep_errors)
        number_of_false = error_indep[names(error_indep)==FALSE]
        number_of_true = error_indep[names(error_indep)==TRUE]        
        number_of_false = ifelse(is.na(number_of_false[1]), 0, number_of_false[1])
        number_of_true = ifelse(is.na(number_of_true[1]), 0, number_of_true[1])
        
        number_of_independent_errors = c(number_of_independent_errors, number_of_true)
        number_of_dependent_errors = c(number_of_dependent_errors, number_of_false)

    }
    improvements_result = rbind(improvements_result, mean_improvement)
    improvements_result = rbind(improvements_result, improvements_standard_deviation)
    
    errors_result = rbind(errors_result, mean_error)
    errors_result = rbind(errors_result, errors_standard_deviation)
    
    indep_errors_result = rbind(indep_errors_result, number_of_independent_errors)
    indep_errors_result = rbind(indep_errors_result, number_of_dependent_errors)

    write.table(improvements_result, file = "data/improvements_evaluation_week_1.csv", sep=",")  
    write.table(errors_result, file = "data/errors_evaluation_week_1.csv", sep=",") 
    write.table(indep_errors_result, file = "data/errors_independance_evaluation_week_1.csv", sep=",") 
}