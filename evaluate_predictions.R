evaluate_predictions <- function (products, comparison_baseline_method) {
    for(i in 1:nrow(products)){
        evaluate_single_product_predictions(products[i,], comparison_baseline_method)
    }
    #calculate_mean_assesment_measures(products)  
}    

evaluate_single_product_predictions <- function (product, comparison_baseline_method) {
    
    forecasts_path = paste("data/", product[1], "/", product[2], "/Predictions", sep="")
    files <- list.files(path=forecasts_path, pattern="*.csv", full.names=TRUE, recursive=FALSE)
    files = mixedsort(files)
    
    first_results = read.csv(file=files[1], header=TRUE, sep=",")
    col_names = colnames(first_results)
    last_error = tail(col_names, n=1)
    number_of_predicted_weeks = (strsplit(last_error, "_", fixed=TRUE))[[1]][2]
    number_of_predicted_weeks = as.integer(number_of_predicted_weeks)
    baseline_predictions = read.csv(file=paste(forecasts_path, "/", comparison_baseline_method ,".csv", sep=""), header=TRUE, sep=",")
    baseline_error_1 = baseline_predictions[, "Error_1"]
    
    methods <- c()
    mean_ae <- c()
    improvement <- c()
    cliff_delta <- c()
    hedges_g <- c()
    errors_autocorrelation <- c()
    wilcoxon_test <- c()

    for(i in 1:length(files)) {
        x = files[i]
        end_of_file_name = strsplit(x, "/Predictions/", fixed=TRUE)
        method_name = (strsplit(end_of_file_name[[1]][2], ".", fixed=TRUE))[[1]][1]
        predictions = read.csv(file=x, header=TRUE, sep=",") 
        methods = c(methods, method_name)                     
     
        errors = predictions[, "Error_1"]
        errors <- errors[!is.na(errors)]                            
        ae = mean(errors, na.rm=TRUE)              
        ljung_box_test = Box.test(errors, lag=30, type="Ljung-Box")             
        ae = round(ae, 2)
        mean_ae = c(mean_ae, ae)

        # test for autocorrelation in errors
        acorr = ljung_box_test$p.value>0.05
        errors_autocorrelation = c(errors_autocorrelation, acorr)      
        
        # cliff's delta and hedges'g coefficients for all methods and naive method
        cd = abs(cliff.delta(baseline_error_1, errors)[[1]])
        if(cd<0.112){
            method_cd = "negligible"
        } else if(0.112<=cd & cd <0.276) {
            method_cd = "small"
        } else if(0.112<= cd & cd <0.428){
            method_cd = "medium"
        } else if (cd>=0.428){
            method_cd = "large"
        }
        method_hg = toString(cohen.d(baseline_error_1, errors, hedges.correction=TRUE, na.rm=TRUE)$magnitude)
        cliff_delta = c(cliff_delta, method_cd)
        hedges_g = c(hedges_g, method_hg)
        
        #wilcoxon test
        #In order to make wilcoxon test length of naive errors adjusted to the length of method errors
        n_error <- tail(baseline_error_1, n=length(errors))
        p = wilcox.test(n_error, errors, paired=TRUE)$p.value
        wilcoxon_test = c(wilcoxon_test, p<0.05)
    }
    
    errors <- data.frame(Method = methods)
    errors[, "Error_1"] = mean_ae     
    
    ## predictions improvement compared to naive method
    for(i in 1:length(files)){
        naive_ae = (errors %>% filter (Method == 'naive') %>% select (Error_1))[[1]]
        method_mean_error = errors[i, "Error_1"]
        impr = (1 - (method_mean_error/naive_ae))*100
        impr = round(impr, 2)              
        improvement = c(improvement, impr)           
    }
        
    ## single product predictions evaluation
    result <- data.frame(Method = methods)   
    result = data.frame(Method = methods)
    result[, "Error_1"] = mean_ae
    result[, "Impr_1"] = improvement
    result[, "Cliff_delta"] = cliff_delta
    result[, "Hedges_g"] = hedges_g
    result[, "Wilcoxon_test"] = wilcoxon_test
    result[, "Independent_errors"] = acorr      
        
    result_file_path = paste("data/", product[1], "/", product[2], "/", comparison_baseline_method, "_baseline_predictions_comparison.csv", sep="")
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
            
            method_indep_errors = (component_evaluation %>% filter (Method == methods[j]) %>% select (Independent_errors))[[1]]
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
        mean_improvement = c(mean_improvement, round(mean(method_improvements), 2))
        improvements_standard_deviation = c(improvements_standard_deviation, round(sqrt(var(method_improvements)), 2))
        
        method_errors = errors_result[,i]
        mean_error = c(mean_error, round(mean(method_errors), 2))
        errors_standard_deviation = c(errors_standard_deviation, round(sqrt(var(method_errors)), 2))   
        
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