library(data.table)
library(lubridate)
library(fasttime)
library(RColorBrewer)
library(ggplot2)
library(ranger)
library(scoringRules)
library(qrnn)

# This i a slimmed down version of the code behind the study 'A framework for probabilistic weather forecast post-processing across models and lead times using machine learning'
# submitted to Phil Trans A on 29/04/2020 by Kirkwood, Economou, Odbert and Pugeault. It removes figure generating code and focusses on the barebones functionality.

dir.create(paste0(getwd(), "/plots/"))
dir.create(paste0(getwd(), "/plots/pdf/"))

# Load hourly forecast and observations data, and process ####


hourdat <- fread("data/hourly_road_forecast_data.csv")
hourdat[, init_time := fastPOSIXct(init_time), ]
hourdat[, validity_time := fastPOSIXct(validity_time), ]
str(hourdat) # this is our dataset

# Plot individual ensemble members of ENUK (just to check data is loaded successfully):
# ggplot(hourdat[model_type == "enuk"][SSPA_ID == sample(unique(SSPA_ID),1)][init_time == sample(unique(init_time), 1)][, c("validity_time", "model_ID", "forecast_temp", "obs_temp")]) + 
#   geom_line(aes(x = validity_time, y = forecast_temp, col = model_ID, group = model_ID)) + geom_line(aes(x = validity_time, y = obs_temp), col = "black")

# Change enuk model_ID to quantiles (i.e. identify from lowest to highest for each validity time per initialisation - this identification is necessary to avoid overdispersed errors)
hourdat[model_type == "enuk", model_ID := paste0(round(seq(0,100,100/11), 0),"th%_enuk"), by = c("SSPA_ID","validity_time","lead_hrs")]

# Engineer seasonality features e.g. time of day, day of year
hourdat[, validity_time_HOD := hour(validity_time), ] # validity hour of day
hourdat[, validity_time_DOY := yday(validity_time), ] # validity day of year

hourdat[, init_time_HOD := hour(init_time), ] # initialisation hour of day
hourdat[, init_time_DOY := yday(init_time), ] # initialisation day of year

# Engineer a residual variable, to learn to correct
hourdat[, residual := obs_temp - forecast_temp, ]

hourdat <- hourdat[order(SSPA_ID, validity_time, model_type, lead_hrs, forecast_temp)] # change data order as you please, for viewing convenience
#View(hourdat[1:100]) # view some of the data if you like - the data table is now ready for model training


# Plot examples of the data and forecast residuals, for visualisation and figures ####


# Assign colours to model types (to be used in subsequent plots)
display.brewer.pal(length(unique(hourdat[,model_type])), "Dark2")
modelpal <- data.frame(model_type = unique(hourdat[,model_type]), col = brewer.pal(length(unique(hourdat[,model_type])), "Dark2"))
modelpalvec <- as.character(modelpal$col)
names(modelpalvec) <- modelpal$model_type

ggplot(hourdat[init_time < '2019-01-05 00:00:00' & validity_time > '2019-01-02 00:00:00'][SSPA_ID == unique(SSPA_ID)[1]]) + 
  geom_line(aes(x = validity_time, y = forecast_temp, group = interaction(init_time, model_ID), col = model_type), alpha = 0.25, size = 0.25) +
  scale_color_manual(values = modelpalvec) +
  geom_line(data = hourdat[validity_time <= '2019-01-05 00:00:00' & validity_time > '2019-01-02 00:00:00'][SSPA_ID == unique(SSPA_ID)[1]],
            aes(x = validity_time, y = obs_temp), col = "black") +
  geom_vline(xintercept = as.numeric(as.POSIXct('2019-01-05', origin = "1970-01-01")), linetype = "dashed") +
  labs(x = "validity time (dashed line represents time zero)", y = "road surface temperature (°C)") +
  scale_x_datetime(expand = c(0,2)) + theme_bw()
#ggsave("plots/pdf/The challenge - probabilistic forecasting from ensemble runs.pdf", width = 136, height = 60, units = "mm", device = cairo_pdf, dpi = 300, scale = 1.4)

# Choose model type to work with for the following plots (one of enuk, eur_eu, eur_uk, glm, glu, pvrn, ukv):
type <- "glm"

ggplot(hourdat[model_type == type][, .SD[sample(.N, 250, replace = FALSE)], by = lead_hrs]) + geom_jitter(aes(x = lead_hrs, y = residual), width = 0.5, alpha = 0.25, shape = 16, size = 0.33) +
  geom_smooth(aes(x = lead_hrs, y = residual), col = "red", se = F) + theme_bw() + scale_y_continuous(limits = c(-11,11)) +
  labs(y = "forecast error (positive = overprediction)", x = "lead time (hours)")# + ggtitle(paste("Visualisation of", type, "NWP model forecast error, by hour"))

# Post-process numerical forecasts to provide probabilistic forecasts of weather outcomes ####


# Choose run-in window in days (how much data to train on prior to prediction):
runin <- 14
# Choose number of scenarios to run:
nscen = 200

set.seed(54321)
# The following loops through forecast scenarios, training a model on previous data (of length runin) before using it to convert future forecasts to probabilities, which can be evaluated:
time <- Sys.time()
results <- list()
#set.seed(321)
pb <- txtProgressBar(0, nscen, style = 3)
i <- 1
for(i in 1:nscen){
  # Select site and forecast time, and train the model on forecast errors ####
  
  
  site <- sample(unique(hourdat$SSPA_ID), 1) # randomly select a site to use
  tfs <- unique(hourdat[SSPA_ID == site][order(validity_time)]$validity_time) # extract all possible forecast times
  available_tfs <- tfs[-(1:(runin*24))] # exclude runin time (can't do the post-processing unless we have at least x days of runin)
  available_tfs <- available_tfs[1:length(available_tfs - max(hourdat[SSPA_ID == site]$lead_hrs))] # exclude forecast times which don't have a long enough future in the data
  length(available_tfs) # how many useable forecast times for the site are available?
  tf <- sample(available_tfs, 1) # select one viable forecast time to use as our 'time zero'
  
  train <- hourdat[SSPA_ID == site & validity_time <= tf & validity_time > (tf - hours(runin*24))] # construct training data as x days runin up to time zero
  
  test <- list() # construct test data as the most recently initialised forecasts available at time zero, for each model type
  for(m in unique(hourdat$model_type)){
    test[[m]] <- hourdat[SSPA_ID == site & model_type == m & init_time <= tf & validity_time > tf & validity_time <= (tf + hours(max(hourdat[SSPA_ID == site & model_type == m]$lead_hrs)))]
  }
  test <- rbindlist(test)
  test <- test[, .SD[init_time == max(init_time)], by = model_type]
  
  test[, run := i, ] # add identifier for this scenario run, to keep trach in evaluation
  
  # Subset the training data in some way if you like (in order to speed up model training, and also to balance across models and lead times by oversampling):
  trainsub <- train[sample(1:.N, .N/2), c("model_ID", "lead_hrs", "validity_time_HOD", "init_time_HOD", "forecast_temp", "residual"), with = FALSE] # total_n/x
  
  # This is the error model - a single random forest, but what it learns is conditional on each model type:
  single_forest <- ranger(formula = residual ~ ., data = trainsub[, c("residual", "model_ID", "lead_hrs"), with = FALSE], 
                          mtry = 1, quantreg = TRUE, min.node.size = 1, sample.fraction = 128/nrow(trainsub), always.split.variables = "model_ID",
                          replace = TRUE, num.trees = 250, keep.inbag = FALSE, splitrule = "variance")#, case.weights = weights$weight)
  
  all_preds <- test
  
  #quants <- seq(0,1, 0.01)
  quants <- c(0.025, 0.05, 0.1, 0.15, 0.25, 0.35, 0.45, 0.5, 0.55, 0.65, 0.75, 0.85, 0.9, 0.95, 0.975)
  quants
  q <- quants[1]
  for(q in quants){
    all_preds[, paste0("quant_", q) := forecast_temp + as.numeric(predict(single_forest, test[, c("run", "model_ID", "lead_hrs", "validity_time_HOD", "init_time_HOD", "forecast_temp"), with = FALSE],
                                                                          type = "quantiles", what = function(x) quantile(x, q))$predictions)]
    all_preds[, paste0("quant_", q, "_mean") := ifelse(.N > 1, mean(get(paste0("quant_", q))), get(paste0("quant_", q))), by = eval("validity_time")]
    all_preds[, paste0("quant_", q, "_median") := ifelse(.N > 1, median(get(paste0("quant_", q))), get(paste0("quant_", q))), by = eval("validity_time")]
    all_preds[, paste0("quant_", q, "_ud") := if(.N < 1){get(paste0("quant_", q))}else{if(q == 0.5){mean(get(paste0("quant_", q)))}else{if(q>0.5){min(get(paste0("quant_", q)))}else{max(get(paste0("quant_", q)))}}}, by = eval("validity_time")]
  }
  all_preds
  
  results[[i]] <- all_preds
  setTxtProgressBar(pb, i)
}
results <- rbindlist(results, fill = TRUE)
gc()
Sys.time() - time


# Evaluation metrics for post-processing across nscen scenarios ####

# Coverage of 95% prediction interval (produced by various quantile averaging approaches):
results[, list(cov = sum(obs_temp < quant_0.975_mean & obs_temp > quant_0.025_mean)/sum(.N)), ]
results[, list(cov = sum(obs_temp < quant_0.95_mean & obs_temp > quant_0.05_mean)/sum(.N)), ]

# MSE of post-processed median vs mean of nwp forecasts
results[, list(RMSE_pp = sqrt(mean((obs_temp-quant_0.5_mean)^2)), RMSE_nwp = sqrt(mean((obs_temp-mean(forecast_temp))^2))), by = c("lead_hrs", "run")][, list(RMSE_pp = mean(RMSE_pp), RMSE_nwp = mean(RMSE_nwp))]
results[, list(RMSE_pp = sqrt(mean((obs_temp-quant_0.5_mean)^2)), RMSE_nwp = sqrt(mean((obs_temp-mean(forecast_temp))^2))), by = c("lead_hrs", "run")][, list(RMSE_pp = mean(RMSE_pp), RMSE_nwp = mean(RMSE_nwp)), by = "lead_hrs"]
ggplot(melt(results[, list(postprocessed = sqrt(mean((obs_temp-quant_0.5_mean)^2)), 
                           avged_nwp = sqrt(mean((obs_temp-mean(forecast_temp))^2))), 
                    by = c("lead_hrs", "SSPA_ID", "run")][, list(QRF_pp = mean(postprocessed), NWP_avg = mean(avged_nwp)), by = c("lead_hrs")], 
            id.vars = c("lead_hrs"), variable.name = "method", value.name = "RMSE")) + scale_y_continuous(limits = c(0,3)) +
  geom_point(aes(x = lead_hrs, y = RMSE, group = method, col = method), alpha = 0.5, shape = 16)  + theme_bw() + labs(x = "hours ahead") +
  stat_smooth(aes(x = lead_hrs, y = RMSE, group = method, col = method), se = FALSE, span = 0.5, method = "loess")
#ggsave("plots/pdf/deterministic_comparison_rmse.pdf", width = 68, height = 60, units = "mm", device = cairo_pdf, dpi = 300, scale = 1.4)

# Plots of coverage for various prediction intervals
ggplot(melt(results[, list("95%" = sum(obs_temp < quant_0.975_mean & obs_temp > quant_0.025_mean)/sum(.N),
                           "90%" = sum(obs_temp < quant_0.95_mean & obs_temp > quant_0.05_mean)/sum(.N),
                           "80%" = sum(obs_temp < quant_0.9_mean & obs_temp > quant_0.1_mean)/sum(.N),
                           "50%" = sum(obs_temp < quant_0.75_mean & obs_temp > quant_0.25_mean)/sum(.N)),
                    , by = c("lead_hrs")], 
            id.vars = c("lead_hrs"), variable.name = "interval", value.name = "coverage")) +
  geom_hline(yintercept = c(0.95, 0.9, 0.8, 0.5)) + theme_bw() + labs(x = "hours ahead") +
  geom_point(aes(x = lead_hrs, y = coverage, group = interval, col = interval), alpha = 0.5, shape = 16) + scale_y_continuous(limits = c(0,1)) +
  geom_smooth(aes(x = lead_hrs, y = coverage, group = interval, col = interval), se = FALSE, alpha = 0.33)# +
#ggtitle("Coverage of mean blend of post-processed model quantiles") # of the mean blend
#ggsave("plots/pdf/multiple_interval_evaluation3.pdf", width = 68, height = 60, units = "mm", device = cairo_pdf, dpi = 300, scale = 1.4)


# Simulating from interpolated CDF ####
scen <- sample(unique(results$run), 1)
scen <- 150L # 102,169,150,31,114

time <- Sys.time()
simeval <- list()
pb <- txtProgressBar(0, nscen, style = 3)
for(scen in sample(unique(results$run), nscen)){
  randscen <- results[run == scen]
  randscen[, hours_ahead := as.numeric((validity_time - min(unique(validity_time)))/60/60), ]
  
  nsim <- 1000
  simpred <- unique(randscen[, c("validity_time", "hours_ahead"), with = FALSE])[rep(1:.N, each = nsim)]
  eval <- unique(randscen[, c("run", "hours_ahead"), with = FALSE])
  
  h <- 50
  for(h in unique(simpred$hours_ahead)){
    pred_quants <- data.frame(Temperature = as.numeric(unique(randscen[hours_ahead == h, grepl("_mean", names(randscen)), with = FALSE])), CP = quants)
    pred_quants <- pred_quants[!duplicated(pred_quants$Temperature), ]
    
    simpred[hours_ahead == h, preds := rquantile(nsim, tau = pred_quants$CP, quant = pred_quants$Temperature), ]
    
    eval[hours_ahead == h, CRPS := crps_sample(mean(randscen[hours_ahead == h]$obs_temp), t(as.matrix(simpred[hours_ahead == h]$preds)))]
    eval[hours_ahead == h, LogS := logs_sample(mean(randscen[hours_ahead == h]$obs_temp), t(as.matrix(simpred[hours_ahead == h]$preds)))]
  }
  
  simeval[[scen]] <- eval
  
  # ggplot(randscen) +
  #   #geom_jitter(data = simpred, aes(x = hours_ahead, y = preds), alpha = 0.03, shape = 16, size = 0.5, width = 0.45, col = "grey36") +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds), drop = FALSE) +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds, fill = ..density.., geom = 'raster', interpolate = F, drop = F)) + scale_fill_viridis_c(option = "inferno") +
  #   geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",quants[1],"_mean"), ymax = paste0("quant_",quants[length(quants)],"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.05,"_mean"), ymax = paste0("quant_",0.95,"_mean")), alpha = 0.33, fill = "grey") +
  #   geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.1,"_mean"), ymax = paste0("quant_",0.9,"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.25,"_mean"), ymax = paste0("quant_",0.75,"_mean")), alpha = 0.33, fill = "grey") +
  #   #geom_line(aes(x = hours_ahead, y = quant_0.5_mean), alpha = 0.75, col = "grey", size = 2) +
  #   geom_line(aes(x = hours_ahead, y = forecast_temp, group = model_ID, col = model_type), alpha = 0.25) +
  #   geom_line(aes(x = hours_ahead, y = obs_temp)) + scale_y_continuous(limits = c(-5,20)) +
  #   scale_color_manual(values = modelpalvec) + theme_bw() + labs(x = "hours ahead", y = "road surface temperature (°C)") + 
  #   theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.direction="horizontal")#+
  # # ggtitle(paste("One example of random forest based NWP post-processing"),
  # #         subtitle = "black line = true observations, coloured lines = NWP forecasts (see legend)\ngrey line and shading = post-processed forecast mean and prediction intervals")
  # ggsave("plots/pdf/one_example_of_forecast_intervals-intervals.pdf", width = 136, height = 60, units = "mm", device = cairo_pdf, scale = 1.4, dpi = 300)
  # 
  # ggplot(randscen) +
  #   geom_jitter(data = simpred, aes(x = hours_ahead, y = preds), alpha = 0.03, shape = 16, size = 0.33, width = 0.5, col = "grey36") +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds), drop = FALSE) +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds, fill = ..density.., geom = 'raster', interpolate = F, drop = F)) + scale_fill_viridis_c(option = "inferno") +
  #   #geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",quants[1],"_mean"), ymax = paste0("quant_",quants[length(quants)],"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.05,"_mean"), ymax = paste0("quant_",0.95,"_mean")), alpha = 0.33, fill = "grey") +
  #   #geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.1,"_mean"), ymax = paste0("quant_",0.9,"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.25,"_mean"), ymax = paste0("quant_",0.75,"_mean")), alpha = 0.33, fill = "grey") +
  #   #geom_line(aes(x = hours_ahead, y = quant_0.5_mean), alpha = 0.75, col = "grey", size = 2) +
  #   geom_line(aes(x = hours_ahead, y = forecast_temp, group = model_ID, col = model_type), alpha = 0.25) +
  #   geom_line(aes(x = hours_ahead, y = obs_temp)) + scale_y_continuous(limits = c(-5,20)) +
  #   scale_color_manual(values = modelpalvec) + theme_bw() + labs(x = "hours ahead", y = "road surface temperature (°C)") + 
  #   theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.direction="horizontal")#+
  # # ggtitle(paste("One example of random forest based NWP post-processing"),
  # #         subtitle = "black line = true observations, coloured lines = NWP forecasts (see legend)\ngrey line and shading = post-processed forecast mean and prediction intervals")
  # ggsave("plots/pdf/one_example_of_forecast_intervals-simulations.pdf", width = 136, height = 60, units = "mm", device = cairo_pdf, scale = 1.4, dpi = 300)
  # 
  # ggplot(randscen) +
  #   geom_jitter(data = simpred, aes(x = hours_ahead, y = preds), alpha = 0.03, shape = 16, size = 0.33, width = 0.45, col = "grey36") +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds), drop = FALSE) +
  #   #stat_bin_2d(data = simpred, aes(x = hours_ahead, y = preds, fill = ..density.., geom = 'raster', interpolate = F, drop = F)) + scale_fill_viridis_c(option = "inferno") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",quants[1],"_mean"), ymax = paste0("quant_",quants[length(quants)],"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.05,"_mean"), ymax = paste0("quant_",0.95,"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.1,"_mean"), ymax = paste0("quant_",0.9,"_mean")), alpha = 0.33, fill = "grey") +
  #   # geom_ribbon(aes_string(x = "hours_ahead", ymin = paste0("quant_",0.25,"_mean"), ymax = paste0("quant_",0.75,"_mean")), alpha = 0.33, fill = "grey") +
  #   #geom_line(aes(x = hours_ahead, y = quant_0.5_mean), alpha = 0.75, col = "grey", size = 2) +
  #   geom_line(aes(x = hours_ahead, y = forecast_temp, group = model_ID, col = model_type), alpha = 0.25) +
  #   geom_line(aes(x = hours_ahead, y = obs_temp)) + scale_y_continuous(limits = c(-20,20), sec.axis = sec_axis(~(.+20)/75, name = "predicted probability of temperature < 0")) +
  #   geom_line(data = simpred[, list(prob_zero = length(which(preds < 0))/.N), by = "hours_ahead"], aes(x = hours_ahead, y = prob_zero*75-20), col = "darkred") +
  #   scale_color_manual(values = modelpalvec) + theme_bw() + labs(x = "hours ahead", y = "road surface temperature (°C)") + 
  #   theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99), legend.direction="horizontal")#+
  # # ggtitle(paste("One example of random forest based NWP post-processing"),
  # #         subtitle = "black line = true observations, coloured lines = NWP forecasts (see legend)\ngrey line and shading = post-processed forecast mean and prediction intervals")
  # ggsave("plots/pdf/one_example_of_forecast_intervals-simulations_and_zeroprob.pdf", width = 136, height = 92, units = "mm", device = cairo_pdf, scale = 1.4, dpi = 300)
  
  setTxtProgressBar(pb, scen)
}
simeval <- rbindlist(simeval)
Sys.time() - time

# Plot of CRPS and Log Score of simulated predictive distribution by hours ahead:
ggplot(melt(simeval[,.SD[sample(.N, 50, replace = TRUE)], by = c("hours_ahead")], id.vars = c("hours_ahead", "run"), variable.name = "metric", value.name = "score")) + 
  geom_jitter(aes(x = hours_ahead, y = score, group = metric, col = metric), alpha = 0.1, width = 0.5, shape = 16, size = 0.67) +
  stat_smooth(aes(x = hours_ahead, y = score, group = metric, col = metric), span = 0.33, method = "loess", se = FALSE, size = 1) +
  scale_y_continuous(limits = c(0, 8)) + theme_bw()
ggsave("plots/pdf/CRPS_and_LogS_eval_200_scenarios_subsample.pdf", width = 136, height = 60, units = "mm", device = cairo_pdf, scale = 1.4, dpi = 300)

simeval[, list(CRPS = mean(CRPS), LogS = mean(LogS)), by = "hours_ahead"][hours_ahead == 48]
results[, list(RMSE_pp = sqrt(mean((obs_temp-quant_0.5_mean)^2)), RMSE_nwp = sqrt(mean((obs_temp-mean(forecast_temp))^2))), by = c("lead_hrs", "run")][, list(RMSE_pp = mean(RMSE_pp), RMSE_nwp = mean(RMSE_nwp)), by = "lead_hrs"][lead_hrs == 48]