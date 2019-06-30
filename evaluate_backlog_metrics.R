evaluate_backlog_metrics <- function (products) {
    product_names <- c()
    autocorelations_lag_1 <- c()
    autocorelations_lag_2 <- c()
    autocorelations_lag_3 <- c()
    autocorelations_lag_4 <- c()
    autocorelations_lag_5 <- c()
    autocorelations_lag_6 <- c()
    autocorelations_lag_7 <- c()
    autocorelations_lag_8 <- c()
    autocorelations_lag_9 <- c()
    autocorelations_lag_10 <- c()
    autocorelations_lag_11 <- c()
    autocorelations_lag_12 <- c()
    autocorelations_lag_13 <- c()
    autocorelations_lag_14 <- c()
    autocorelations_lag_15 <- c()
    autocorelations_lag_16 <- c()
    autocorelations_lag_17 <- c()
    autocorelations_lag_18 <- c()
    autocorelations_lag_19 <- c()
    autocorelations_lag_20 <- c()
    autocorelations_lag_21 <- c()
    autocorelations_lag_22 <- c()
    autocorelations_lag_23 <- c()
    autocorelations_lag_24 <- c()
    autocorelations_lag_25 <- c()
    autocorelations_lag_26 <- c()
    autocorelations_lag_27 <- c()
    autocorelations_lag_28 <- c()
    autocorelations_lag_29 <- c()
    autocorelations_lag_30 <- c()

    backlog_mean_size <- c()
    mean_backlog_size_change <- c()
    
    for(i in 1:nrow(products)){
       product = products[i,]
       defect_backlog = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
       backlog_mean_size = c(backlog_mean_size, round(mean(defect_backlog$backlog_all), 2))      
       
       product_autocorrelation_measures = get_backlog_autocorrelation(product)
       autocorelations_lag_1 = c(autocorelations_lag_1, round(product_autocorrelation_measures[1], 2))
       autocorelations_lag_2 = c(autocorelations_lag_2, round(product_autocorrelation_measures[2], 2))
       autocorelations_lag_3 = c(autocorelations_lag_3, round(product_autocorrelation_measures[3], 2))
       autocorelations_lag_4 = c(autocorelations_lag_4, round(product_autocorrelation_measures[4], 2))
       autocorelations_lag_5 = c(autocorelations_lag_5, round(product_autocorrelation_measures[5], 2))
       autocorelations_lag_6 = c(autocorelations_lag_6, round(product_autocorrelation_measures[6], 2))
       autocorelations_lag_7 = c(autocorelations_lag_7, round(product_autocorrelation_measures[7], 2))
       autocorelations_lag_8 = c(autocorelations_lag_8, round(product_autocorrelation_measures[8], 2))
       autocorelations_lag_9 = c(autocorelations_lag_9, round(product_autocorrelation_measures[9], 2))
       autocorelations_lag_10 = c(autocorelations_lag_10, round(product_autocorrelation_measures[10], 2))
       autocorelations_lag_11 = c(autocorelations_lag_11, round(product_autocorrelation_measures[11], 2))
       autocorelations_lag_12 = c(autocorelations_lag_12, round(product_autocorrelation_measures[12], 2))
       autocorelations_lag_13 = c(autocorelations_lag_13, round(product_autocorrelation_measures[13], 2))
       autocorelations_lag_14 = c(autocorelations_lag_14, round(product_autocorrelation_measures[14], 2))
       autocorelations_lag_15 = c(autocorelations_lag_15, round(product_autocorrelation_measures[15], 2))
       autocorelations_lag_16 = c(autocorelations_lag_16, round(product_autocorrelation_measures[16], 2))
       autocorelations_lag_17 = c(autocorelations_lag_17, round(product_autocorrelation_measures[17], 2))
       autocorelations_lag_18 = c(autocorelations_lag_18, round(product_autocorrelation_measures[18], 2))
       autocorelations_lag_19 = c(autocorelations_lag_19, round(product_autocorrelation_measures[19], 2))
       autocorelations_lag_20 = c(autocorelations_lag_20, round(product_autocorrelation_measures[20], 2))
       autocorelations_lag_21 = c(autocorelations_lag_21, round(product_autocorrelation_measures[21], 2))
       autocorelations_lag_22 = c(autocorelations_lag_22, round(product_autocorrelation_measures[22], 2))
       autocorelations_lag_23 = c(autocorelations_lag_23, round(product_autocorrelation_measures[23], 2))
       autocorelations_lag_24 = c(autocorelations_lag_24, round(product_autocorrelation_measures[24], 2))
       autocorelations_lag_25 = c(autocorelations_lag_25, round(product_autocorrelation_measures[25], 2))
       autocorelations_lag_26 = c(autocorelations_lag_26, round(product_autocorrelation_measures[26], 2))
       autocorelations_lag_27 = c(autocorelations_lag_27, round(product_autocorrelation_measures[27], 2))
       autocorelations_lag_28 = c(autocorelations_lag_28, round(product_autocorrelation_measures[28], 2))
       autocorelations_lag_29 = c(autocorelations_lag_29, round(product_autocorrelation_measures[29], 2))
       autocorelations_lag_30 = c(autocorelations_lag_30, round(product_autocorrelation_measures[30], 2))
        
       backlog_size_change = round(get_mean_backlog_size_change(defect_backlog), 2)
       mean_backlog_size_change = c(mean_backlog_size_change, backlog_size_change )
        
       product_names = c(product_names, paste(product[1], product[2], sep = "_"))
    }
    result = data.frame(Products = product_names,
                        Acf_lag_1 = autocorelations_lag_1,
                        Acf_lag_2 = autocorelations_lag_2,
                        Acf_lag_3 = autocorelations_lag_3,
                        Acf_lag_4 = autocorelations_lag_4,
                        Acf_lag_5 = autocorelations_lag_5,
                        Acf_lag_6 = autocorelations_lag_6,
                        Acf_lag_7 = autocorelations_lag_7,
                        Acf_lag_8 = autocorelations_lag_8,
                        Acf_lag_9 = autocorelations_lag_9,
                        Acf_lag_10 = autocorelations_lag_10,
                        Acf_lag_11 = autocorelations_lag_11,
                        Acf_lag_12 = autocorelations_lag_12,
                        Acf_lag_13 = autocorelations_lag_13,
                        Acf_lag_14 = autocorelations_lag_14,
                        Acf_lag_15 = autocorelations_lag_15,
                        Acf_lag_16 = autocorelations_lag_16,
                        Acf_lag_17 = autocorelations_lag_17,
                        Acf_lag_18 = autocorelations_lag_18,
                        Acf_lag_19 = autocorelations_lag_19,
                        Acf_lag_20 = autocorelations_lag_20,
                        Acf_lag_21 = autocorelations_lag_21,
                        Acf_lag_22 = autocorelations_lag_22,
                        Acf_lag_23 = autocorelations_lag_23,
                        Acf_lag_24 = autocorelations_lag_24,
                        Acf_lag_25 = autocorelations_lag_25,
                        Acf_lag_26 = autocorelations_lag_26,
                        Acf_lag_27 = autocorelations_lag_27,
                        Acf_lag_28 = autocorelations_lag_28,
                        Acf_lag_29 = autocorelations_lag_29,
                        Acf_lag_30 = autocorelations_lag_30,
                        Mean_backlog_size = backlog_mean_size,
                        Mean_backlog_size_change = mean_backlog_size_change)
    write.table(result, file = "data/backlogs_metrics.csv", sep=",")
}    

get_backlog_autocorrelation <- function(product){
    product_path = paste("data/", product[1], "/", product[2], "/", sep="")
    defect_backlog = read.csv(file=paste(product_path, product[1], "_", product[2], "_backlog.csv", sep=""), header=TRUE, sep=",")
    component_evaluation = read.csv(file=paste(product_path, "/predictions_evaluation.csv", sep=""), header=TRUE, sep=",")
    
    backlog_all <- defect_backlog$backlog_all
    autocorelation_lag_10 = (acf(backlog_all, lag.max=30, plot = FALSE))$acf
    
    bound = 2/sqrt(length(defect_backlog$backlog_all))
    return(autocorelation_lag_10)
}

get_mean_backlog_size_change <- function(defect_backlog){
    backlog_size_diff = c()
    for (i in 2:nrow(defect_backlog)){
        previous_week = defect_backlog[i-1,"backlog_all"]
        current_week = defect_backlog[i,"backlog_all"]
        backlog_size_diff = c(backlog_size_diff, abs(previous_week-current_week) )
    }    
    return(mean(backlog_size_diff, na.rm=TRUE))
}
