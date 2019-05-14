part_of_backlog_all_predictions <- function(product, part_of_backlog){
    part_of_backlog_naive_prediction(product, part_of_backlog)
    part_of_backlog_arima_prediction(product, part_of_backlog)
    part_of_backlog_ets_prediction(product, part_of_backlog)
    part_of_backlog_ericsson_prediction(product, part_of_backlog)
}

part_of_backlog_ets_prediction <- function(product, part_of_backlog){
    
    weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")

    weekly_backlog_ts = ts(part_of_backlog$backlog_all)
    first_week_number = part_of_backlog[1, "number"]
    last_week_number = tail(part_of_backlog$number, n=1)
    weeks_to_predict = weeks_to_predict[weeks_to_predict > first_week_number & weeks_to_predict < last_week_number]

    components <- c()
    actuals_1 <- c()
    forecasts_1 <- c()
    errors_1 <- c()

    for(i in 1:length(weeks_to_predict)){
        week_number <- weeks_to_predict[i]
        fit = ets(subset(weekly_backlog_ts, 
                         start = 0,
                         end = week_number-first_week_number))
        fc = forecast(fit, 1)  
        comp = fit$components
        components[i] = paste(comp[1], comp[2], comp[3])

        forecasted_value = round(fc$mean[1])
        forecasts_1 = c(forecasts_1, forecasted_value)

        actual_backlog_size = (part_of_backlog %>% filter (number == week_number) %>% select (backlog_all))[[1]]
        actuals_1 = c(actuals_1, actual_backlog_size)

        error = abs(forecasted_value - actual_backlog_size)
        errors_1 = c(errors_1, error)
    }

    result <- data.frame(Week = weeks_to_predict,
                         ets_components = components, 
                         actual_1 = actuals_1,
                         forecast_1 = forecasts_1,
                         error_1 = errors_1)

    result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/ets_", first_week_number, "-", last_week_number, ".csv", sep="")
    write.table(result, file = result_file_path, sep=",")
}


part_of_backlog_arima_prediction <- function(product, part_of_backlog){
    
    weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")

    weekly_backlog_ts = ts(part_of_backlog$backlog_all)
    first_week_number = part_of_backlog[1, "number"]
    last_week_number = tail(part_of_backlog$number, n=1)
    weeks_to_predict = weeks_to_predict[weeks_to_predict > first_week_number & weeks_to_predict < last_week_number]

    models <- c()
    actuals_1 <- c()
    forecasts_1 <- c()
    errors_1 <- c()

    for(i in 1:length(weeks_to_predict)){
        week_number <- weeks_to_predict[i]
        fit = auto.arima(subset(weekly_backlog_ts, 
                               start = 0,
                               end = week_number-first_week_number))
        fc = forecast(fit, 1)
        arma = fit$arma
        models = c(models, paste(arma[1], arma[6], arma[2]))

        forecasted_value = round(fc$mean[1])
        forecasts_1 = c(forecasts_1, forecasted_value)

        actual_backlog_size = (part_of_backlog %>% filter (number == week_number) %>% select (backlog_all))[[1]]
        actuals_1 = c(actuals_1, actual_backlog_size)

        error = abs(forecasted_value - actual_backlog_size)
        errors_1 = c(errors_1, error)
    }

    result <- data.frame(Week = weeks_to_predict,
                         p_d_q = models, 
                         actual_1 = actuals_1,
                         forecast_1 = forecasts_1,
                         error_1 = errors_1)

    result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/arima_", first_week_number, "-", last_week_number, ".csv", sep="")
    write.table(result, file = result_file_path, sep=",")
}


part_of_backlog_naive_prediction <- function(product, part_of_backlog){
    
    weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")

    weekly_backlog_ts = ts(part_of_backlog$backlog_all)
    first_week_number = part_of_backlog[1, "number"]
    last_week_number = tail(part_of_backlog$number, n=1)
    weeks_to_predict = weeks_to_predict[weeks_to_predict > first_week_number & weeks_to_predict < last_week_number]

    actuals_1 <- c()
    forecasts_1 <- c()
    errors_1 <- c()

    for(i in 1:length(weeks_to_predict)){
        week_number <- weeks_to_predict[i]
        print(week_number)
        if(week_number > first_week_number){
            actual_backlog_size = part_of_backlog %>% filter (number == week_number) %>% select (backlog_all)
            actuals_1 = c(actuals_1, actual_backlog_size[[1]])
            if (week_number == part_of_backlog$number[2]){
                forecasts_1[i] = (part_of_backlog %>% filter (number == part_of_backlog$number[1]) %>% select (backlog_all))[[1]]
                ae <- as.integer(forecasts_1[i]) - as.integer(actuals_1[i])
                errors_1[i] = abs(ae)
            } else {
                model = naive(subset(weekly_backlog_ts, 
                                 start = 0,
                                 end = week_number-first_week_number))
                fc = forecast(model,1)
                forecasts_1[i] = round(fc$mean[1])
                ae <- as.integer(forecasts_1[i]) - as.integer(actuals_1[i])
                errors_1[i] = abs(ae)
            }
        }
    }

    result <- data.frame(Week = weeks_to_predict,
                         actual_1 = actuals_1,
                         forecast_1 = forecasts_1,
                         error_1 = errors_1)

    result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/naive_", first_week_number, "-", last_week_number,".csv", sep="")
    write.table(result, file = result_file_path, sep=",")
}


part_of_backlog_ericsson_prediction <- function(product, part_of_backlog){
    
    weeks_to_predict = read.csv(file=paste("data/", product[1], "/", product[2], "/", product[1], "_", product[2], "_samples.csv", sep=""), header=FALSE, sep=",")

    weekly_backlog_ts = ts(part_of_backlog$backlog_all)
    first_week_number = part_of_backlog[1, "number"]
    last_week_number = tail(part_of_backlog$number, n=1)
    weeks_to_predict = weeks_to_predict[weeks_to_predict > first_week_number & weeks_to_predict < last_week_number]

    actuals_1 <- c()
    forecasts_1 <- c()
    errors_1 <- c()

    for(i in 1:length(weeks_to_predict)){
        week_number <- weeks_to_predict[i]
        print(week_number)
        actual_backlog_size = part_of_backlog %>% filter (number == week_number) %>% select (backlog_all)
        actuals_1[i] = actual_backlog_size[[1]]
        if (week_number - 3 < part_of_backlog$number[1]){
            forecasts_1[i] = NA
            errors_1[i] = NA
        } else {
            db_1 <- (part_of_backlog %>% filter (number == week_number-1) %>% select (backlog_all))[[1]]
            di_1 <- (part_of_backlog %>% filter (number == week_number-1) %>% select (inflow_all))[[1]]
            di_2 <- (part_of_backlog %>% filter (number == week_number-2) %>% select (inflow_all))[[1]]
            di_3 <- (part_of_backlog %>% filter (number == week_number-3) %>% select (inflow_all))[[1]]
            do_1 <- (part_of_backlog %>% filter (number == week_number-1) %>% select (outflow_all))[[1]]
            do_2 <- (part_of_backlog %>% filter (number == week_number-2) %>% select (outflow_all))[[1]]
            do_3 <- (part_of_backlog %>% filter (number == week_number-3) %>% select (outflow_all))[[1]]
            di <- (di_1 + di_2 + di_3)/3
            do <- (do_1 + do_2 + do_3)/3
            db <- db_1 + di - do
            forecasts_1[i] = round(db)
            ae <- as.integer(forecasts_1[i]) - as.integer(actuals_1[i])
            errors_1[i] = abs(ae)
        }
    }

    result <- data.frame(Week = weeks_to_predict,
                         actual_1 = actuals_1,
                         forecast_1 = forecasts_1,
                         error_1 = errors_1)

    result_file_path = paste("data/", product[1], "/", product[2], "/Predictions/Divided_backlog_predictions/ericsson_", first_week_number, "-", last_week_number,".csv", sep="")
    write.table(result, file = result_file_path, sep=",")
}


