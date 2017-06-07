##For each four data sets, roughly 80% accuracy.

rm(list=ls()); # clear all variables
graphics.off() #

library(R.matlab)
library(quantreg)

data_ <- readMat("/Users/xbb/Desktop/mouse/mat_files/an198503_2013_03_03_data_struct.mat")
#an198503_2013_03_01_data_struct.mat : 1301 nuerons, 400 trials.
#an194181_2013_01_18_data_struct.mat
#an194672_2013_02_01_data_struct.mat
#an198503_2013_03_03_data_struct.mat
mouse <- data_




#########SAVING TEMPLATE######

root <- "/Users/xbb/Desktop/mouse/"
file_directory <- "an198503_2013_03_01_data_struct/"
#Recorded trials
saveRDS(mouse[[1]][10][[1]][[3]][[2]][[1]][6][[1]][1,], paste0(root, file_directory, "recorded_trials.rds"))
#Recorded timings
saveRDS(mouse[[1]][10][[1]][[3]][[2]][[1]][5][[1]][1,], paste0(root, file_directory, "recorded_timings.rds"))
#Recorded values
saveRDS(mouse[[1]][10][[1]][[3]][[2]][[1]][7][[1]], paste0(root, file_directory, "recorded_values.rds"))

#touches times
"Protraction contacts for d1"
"Retraction contacts for d1"
"Protraction contacts for d2"                        
"Retraction contacts for d2"                         
touches_timing <- c()

for( i in mouse[[1]][11][[1]][3][[1]][[2]][[1]][5][[1]]) {
    if (!is.na(i[[1]][1])) {
        touches_timing <- c(touches_timing, i[[1]][1,])
    }
}
touches_timing <- sort(touches_timing)#Total touch moments



touches_trials <- c()

for( i in mouse[[1]][11][[1]][3][[1]][[2]][[1]][6][[1]]) {
    if (!is.na(i[[1]][1])) {
        touches_trials <- c(touches_trials, i[[1]][1,])
    }
}
touches_trials <- sort(touches_trials)#Total touch trials

saveRDS(touches_timing, paste0(root, file_directory, "touches_timing.rds"))
saveRDS(touches_trials, paste0(root, file_directory, "touches_trials.rds"))


#touches_timing <- mouse[[1]][11][[1]][3][[1]][[2]][[1]][5][[1]][[1]][[1]][1,]#Touches times
#touches_trials <- mouse[[1]][11][[1]][3][[1]][[2]][[1]][6][[1]][[1]][[1]][1,]#Touches Trials

#Pole rising times
pole_timing <- mouse[[1]][11][[1]][3][[1]][[1]][[1]][6][[1]][1,]
pole_trials <- mouse[[1]][11][[1]][3][[1]][[1]][[1]][7][[1]][1,]


saveRDS(pole_timing, paste0(root, file_directory, "pole_timing.rds"))
saveRDS(pole_trials, paste0(root, file_directory, "pole_trials.rds"))
#reward cues
reward_timing <- mouse[[1]][11][[1]][3][[1]][[7]][[1]][6][[1]][1,]
reward_trials <- mouse[[1]][11][[1]][3][[1]][[7]][[1]][7][[1]][1,]

saveRDS(reward_timing, paste0(root, file_directory, "reward_timing.rds"))
saveRDS(reward_trials, paste0(root, file_directory, "reward_trials.rds"))






#########LOADING TEMPLATE######


rm(list=ls()); # clear all variables
graphics.off() #


source("/Users/xbb/Desktop/mouse/linear_oga.r")#Currently not used.
source("/Users/xbb/Desktop/mouse/CS_logistic_r.r")
library(R.matlab)
library(quantreg)
#/Users/apple/Desktop/Mouse/an194672_2013_02_01_data_struct
#/Users/xbb/Desktop/mouse/an198503_2013_03_03_data_struct 
root <- "/Users/xbb/Desktop/mouse/"
file_directory <- "an198503_2013_03_01_data_struct/"
#Recorded trials
recorded_trials <- readRDS(paste0(root, file_directory, "recorded_trials.rds"))
#Recorded timings
recorded_timings <- readRDS(paste0(root, file_directory, "recorded_timings.rds"))
#Recorded values
recorded_values <- readRDS(paste0(root, file_directory, "recorded_values.rds"))


touches_timings <- readRDS(paste0(root, file_directory, "touches_timing.rds"))
touches_trials <- readRDS(paste0(root, file_directory, "touches_trials.rds"))

pole_timings <- readRDS(paste0(root, file_directory, "pole_timing.rds"))
pole_trials <- readRDS(paste0(root, file_directory, "pole_trials.rds"))


reward_timings <- readRDS(paste0(root, file_directory, "reward_timing.rds"))
reward_trials <- readRDS(paste0(root, file_directory, "reward_trials.rds"))


#matching_rate_for_all_neurons <- readRDS(paste0(root, file_directory, "matching_rate_for_all_neurons.rds"))


#
#touches_trials <- touches_trials[-c(1,2,3,4)]#Don't know what's going on with the original data.
#####END OF DATA PROCESSING######


valid_trials_ <- function(recorded_trials, recorded_values, touches_trials, pole_trials, reward_trials) {
    set_1 <- intersect(unique(recorded_trials), unique(recorded_trials)) 
    set_2 <- intersect(unique(pole_trials), unique(reward_trials)) 
    set_3 <- intersect(set_1, set_2)
    #nan_trials <- unique(recorded_trials[which(is.nan(recorded_values[1,]) == TRUE)])
    #set_4 <- setdiff(set_3, nan_trials)
    sort(set_3)
}



valid_trials <- valid_trials_(recorded_trials, recorded_values, touches_trials, pole_trials, reward_trials)

#Doesn't handle valid nerons
series_maker_util <- function(which_nerons_set, which_trials_set, valid_trials, recorded_values, 
                         recorded_trials, touches_trials, pole_trials, reward_trials,
                         recorded_timings, touches_timings, pole_timings, reward_timings) {
              
    trials_set <- valid_trials[which_trials_set]
                             
    result <- c()
    for (trial in trials_set) {
        result <- c(result, rep(trial, sum(recorded_trials == trial)))
    }

                      
    x_axis <- recorded_timings[recorded_trials %in% trials_set]
    
    values <- recorded_values[which_nerons_set, recorded_trials %in% trials_set]
    
    poles <- pole_timings[pole_trials %in% trials_set]
    
    rewards <- reward_timings[reward_trials %in% trials_set]

    touches <- touches_timings[touches_trials %in% trials_set]
    
    list('trials_values_matrix'=values, 'trials_axis'=x_axis, 'pole_trials'=poles, 'reward_trials'=rewards, 'touches_trials'=touches, 'vector_trials'=result)
}

series_maker <- function(i_set, t_set) {
    series_maker_util(i_set, t_set, valid_trials, recorded_values, 
                         recorded_trials, touches_trials, pole_trials, reward_trials,
                         recorded_timings, touches_timings, pole_timings, reward_timings)
}

#series_maker(c(1,2),c(1,2,3))



plotting_util <- function(i_set, t_set) {
    list_ <- series_maker(i_set, t_set)
    x_axis <- list_$trials_axis
    diff_ <- 0
    length_ <- 0
    t_ <- 1
    matrix_ <- list_$trials_values_matrix
    
    
    if (is.null(nrow(matrix_))) {
        length_ <- 1  
        plot(x_axis, matrix_, ylim=c(-2,3), type='l')
        
    } else {
        length_ <- nrow(matrix_)
        
        for (j in 1:length_) {
            y <- matrix_[j,]
            if (t_ == 1) {
                plot(x_axis, y, ylim=c(-2, length_ * 1.7), type='l')
            } else {
                points(x_axis, y + diff_, type='l')
            }
            t_ <- t_ + 1
            diff_ <- diff_ + 1.5 
        }    
    }
    
    
    for (x in list_$pole_trials) {
        segments(x, -1, x, -1 + 0.1, cex=1.5, col='red')
    }
    
    
    for (x in list_$reward_trials) {
        segments(x, -1, x, -1 + 0.1, cex=1.5, col='black')

    }
    
    
    for (x in list_$touches) {
        segments(x, -1.3, x, -1.3 + 0.2, cex=1.5, col='gray')
    }
}

#plotting_util(c(1,2), 1:20)
#series_maker(c(1,2), c(1,4))




#This is based on series_maker. All the data loaded for any testing procedure should be based on series_maker.
rq_series <- function(i_set, t_set) {
    total_run_times <- 0
    total_run_times_withNA <- 0
    touching_trials <- c()
    tau_relative_high <- c()
    firsttouch_after_pole <- c()
    firsttest_after_pole <- c()
    soft_relative <- c()
    speak <- TRUE
    
    trials <- c()
    
    for (i_ in i_set) { 
        for (t_ in t_set) {
            total_run_times_withNA <- total_run_times_withNA + 1 
            data_ <- series_maker(i_, t_)
            
            
            
            trials <- c(trials, t_)

            values <- data_$trials_values_matrix
            x_axis <- data_$trials_axis
            
            
            
            targeted_region <- c(data_$pole_trials[1], data_$pole_trials[2])
            
            
            #Deal with the true results
            if (sum(data_$touches > targeted_region[1] & data_$touches < targeted_region[2]) != 0) {
                touching_trials <- c(touching_trials, 1)
            } else {
                touching_trials <- c(touching_trials, 0)
            }  

            ####AR(2) quantile regression
            length_ <- length(values)
            y <- values[3:length_]
            x1 <- values[2:(length_-1)]
            x2 <- values[1:(length_-2)]
            
            #Shut down the process if there's NA in this round x
            if (any(is.na(x1)) | any(is.na(x2))) {
                tau_relative_high <- c(tau_relative_high, NA)
                firsttouch_after_pole <- c(firsttouch_after_pole, NA)
                firsttest_after_pole <- c(firsttest_after_pole, NA)
                soft_relative <- c(soft_relative, NA)
                next
            }
            
            resi <- rq(y ~ x1 + x2, tau=0.95)$resi
            resi_second <- rq(y ~ x1 + x2, tau=0.50)$resi
            
            
            mid_ <- data_$touches[data_$touches > targeted_region[1] & data_$touches < targeted_region[2]][1]
            if(is.na(mid_)) {
                firsttouch_after_pole <- c(firsttouch_after_pole, 0)
            } else {
                firsttouch_after_pole <- c(firsttouch_after_pole, mid_)
            }
            
            
            
            
            
            
            
            ##rearange the x_axis
            x_axis_time_series <- x_axis[3:length_] 
            bool_axis <- x_axis_time_series > mid_ & x_axis_time_series < mid_ + 350
            
            
            
            if (is.na(bool_axis[1]) | !any(resi[bool_axis] >= 0)) {
                firsttest_after_pole <- c(firsttest_after_pole, 0)
            } else {            				
                firsttest_after_pole <- c(firsttest_after_pole, x_axis_time_series[bool_axis][1])
            }
           
            bool_axis <- x_axis_time_series > targeted_region[1] & x_axis_time_series < targeted_region[2]
            if (any((which(resi[bool_axis] >= 0) + 1) %in% which(resi_second[bool_axis] >= 0))) {                     				            				
            				#if (any((which(resi[bool_axis] >= 0) + 1) %in% which(resi_second[bool_axis] >= 0))) { print(y) }
            				
                tau_relative_high <- c(tau_relative_high, 1)
                total_run_times <- total_run_times + 1
            } else {
                tau_relative_high <- c(tau_relative_high, 0)
                total_run_times <- total_run_times + 1
            }
                        
            
            
            if (any(resi[bool_axis] >= 0)) {
            				soft_relative <- c(soft_relative, 1)
            } else {
            				soft_relative <- c(soft_relative, 0)
            }
        }
    }
    
    #Doesnt deal with NA
    matching_rate <- 1 - sum(abs(touching_trials - tau_relative_high), na.rm=TRUE) / total_run_times_withNA
    true_negative <- sum((touching_trials - tau_relative_high) > 0, na.rm=TRUE) / total_run_times
    false_positive <- sum((tau_relative_high - touching_trials) > 0, na.rm=TRUE) / total_run_times
    #Deal with NA
    matching_rate_noNA <- 1 - sum(abs(touching_trials - tau_relative_high), na.rm=TRUE) / total_run_times
    true_negative_noNA <- sum((touching_trials - tau_relative_high) > 0, na.rm=TRUE) / total_run_times
    false_positive_noNA <- sum((tau_relative_high - touching_trials) > 0, na.rm=TRUE) / total_run_times
    
    error_rate_timing <- mean(((firsttouch_after_pole > 0) - (firsttest_after_pole > 0))^{2}, na.rm=TRUE)
    
    #Soft with NA
    soft_matching_rate_noNA <- 1 - sum(abs(touching_trials - soft_relative), na.rm=TRUE) / total_run_times
    
    list('total_run_times'=total_run_times, 'touching_trials'=touching_trials, 'relative_high'=tau_relative_high, 'matching_rate'=matching_rate, 'false_positive'=false_positive, 'true_negative'=true_negative, 'matching_rate_noNA'=matching_rate_noNA, 'false_positive_noNA'=false_positive_noNA, 'true_negative_noNA'=true_negative_noNA, 'error_rate_timing'=error_rate_timing, 'firsttest_after_pole'=firsttest_after_pole, 'firsttouch_after_pole'=firsttouch_after_pole, 'trials'=trials, 'soft_relative'=soft_relative, 'soft_matching_rate_noNA'=soft_matching_rate_noNA)
    
}


###Pridction scheme with timing in the series:
predictor <- function(i_set, t_set) {
    total_run_times <- 0
    resutls_hard <- c()
    resutls_soft <- c()
    speak <- TRUE
    
    trials <- c()
    
    for (i_ in i_set) {
        firsttest_after_pole <- c()
        relative_high <- c()
        soft_relative <- c()
        
        for (t_ in t_set) {
            data_ <- series_maker(i_, c(t_-1,t_))
            
            
            trials <- c(trials, t_)

            values <- data_$trials_values_matrix
            x_axis <- data_$trials_axis
            
            
            
            targeted_region <- c(data_$pole_trials[3], data_$pole_trials[4])
            
            
            ####AR(2) quantile regression
            length_ <- length(values)
            y <- values[3:length_]
            x1 <- values[2:(length_-1)]
            x2 <- values[1:(length_-2)]
            #Shut down the process if there's NA in this round x
            if (any(is.na(x1)) | any(is.na(x2))) {
                firsttest_after_pole <- c(firsttest_after_pole, NA)
                relative_high <- c(relative_high, NA)
                soft_relative <- c(soft_relative, NA)
                next
            }
            
            resi <- rq(y ~ x1 + x2, tau=0.95)$resi
            resi_second <- rq(y ~ x1 + x2, tau=0.50)$resi
            
            ##rearange the x_axis
            x_axis_time_series <- x_axis[3:length_] 
            bool_axis <- x_axis_time_series > targeted_region[1] & x_axis_time_series < targeted_region[2]
            if (any((which(resi[bool_axis] >= 0) + 1) %in% which(resi_second[bool_axis] >= 0))) {
            				relative_high <- c(relative_high, 1)
            } else {
            				relative_high <- c(relative_high, 0)
            }
            
            
            if (any(resi[bool_axis] >= 0)) {
            				soft_relative <- c(soft_relative, 1)
            } else {
            				soft_relative <- c(soft_relative, 0)
            }
            
            
        }        
        resutls_hard <- cbind(resutls_hard, relative_high)
        resutls_soft <- cbind(resutls_soft, soft_relative)
    }
    return(list(resutls_hard, resutls_soft))
}
###

###
###
###
###
###Quality neurons finding
###Model Selection Trick
#Pole rising timings is aligned to 0.
same_neuron_plot_util <- function(i, t_set) {
    diff_ <- 0
    j <- 1
    ylim_ <- 1.5 * length(t_set)
    xlim_ <- 14000 
    for (t_ in t_set) {
        list_ <- series_maker(i, t_)        
        mid_1 <- list_$trials_axis
        mid_2 <- list_$pole[1]
        x_axis <- mid_1 - mid_2
        matrix_ <- (list_$trials_values_matrix)
        
        if (j == 1) {
            plot(x_axis, matrix_, ylim=c(-2,ylim_), xlim=c(-2000,xlim_), type='l')
            diff_ <- diff_ + 1.5
            j <- j + 1
        } else {
            points(x_axis, matrix_ + diff_, type='l')
            diff_ <- diff_ + 1.5
            j <- j + 1
        }
    }
}




#ms_prediction
ms_prediction <- function(i, testing_data, training_data=1:60, quality_trials=list()) {
    design_ <- c()
    trials <- c()
    for (t_ in training_data) {
        if (t_ %in% quality_trials) {
            stuff <- series_maker(i, t_)
            series <- stuff$trials_values_matrix[2:19]
            
            if (!any(is.na(series))) {
                design_ <- cbind(design_, series)
                trials <- c(trials, stuff$vector_trials[1])
            }
        } else {
            stuff <- series_maker(i, t_)
            series <- stuff$trials_values_matrix[2:19]
            series2 <- stuff$trials_values_matrix[3:20]       
            series3 <- stuff$trials_values_matrix[1:18]
            
            if (!any(is.na(series))) {
                design_ <- cbind(design_, series)
                design_ <- cbind(design_, series2)
                design_ <- cbind(design_, series3)
                trials <- c(trials, stuff$vector_trials[1], stuff$vector_trials[1], stuff$vector_trials[1])
            }
        }
    }
    
    y <- array(series_maker(i, testing_data)$trials_values_matrix[2:19], c(18,1))
    if (!any(is.nan(y))) {
        return(list(y, design_, trials, OHT(y, design_, sqrt(length(design_[1,])))))
    } else {
        return(list(y, design_, trials, NA))
    }
}


#Inverse valid_trials function
inverse_valid_trials <- function(trial) {
    return(which(valid_trials %in% trial))
}



###
###
###Finding the relevent neurons
quality_calculator <- function(good_neurons_larger) {
    s_ <- 1
    quality_keeper <- rep(0, length(good_neurons_larger))

    for (i in good_neurons_larger) {
        ms_matching <- rep(3, b_ - a_)
        matching_neuron_trials <- vector('list', b_ - a_)
        
        tunning_trials_touching_records <- rq_series(i, (a_ + 1):b_)$touch
        touching_records <- rq_series(i, 1:a_)$touch
        predicted_records <- rq_series(i, 1:a_)$relative
        
        
        #Quality trials!
        quality_trials <- c()
        ambiguous_trials <- c()
        
        positive_set <- which(touching_records == predicted_records & (predicted_records == 1))
        sd_results_positive <- rep(0, length(positive_set))
        j <- 1
        
        for (t_ in which(touching_records == predicted_records & (predicted_records == 1))) {
            sd_results_positive[j] <- sd(series_maker(i, t_)$trials_v[5:25])
            j <- j + 1
        }
        
        
        
        #For neuron i, trials with shape. If there's no such nerrons, we might just ignore this neuron i.
        quality_trials <- positive_set[which(sd_results_positive >= mean(sd_results_positive) + 0.5 * sd(sd_results_positive))]
        mid_ <- rq_series(i, 1:a_)
        no_quality_trials <- mid_$trials[mid_$relative == 0 & !is.na(mid_$relative)]
        
        aimed_trials <- c(quality_trials, no_quality_trials) 
        
        focusing_set <- rep(0, a_)
        focusing_set[quality_trials] <- 1
        
        for (j in 1:(b_-a_)) {
            tunning_touching_event <- tunning_trials_touching_records[j]
            
    
            ms_results <- ms_prediction(i, (j + a_), training_data=aimed_trials)
            if (any(is.na(ms_results[[4]]))) {next;}
            matching_trials <- ms_results[[3]][which(ms_results[[4]][[2]] == 1)]
        
            matching_trials_enumeration <- inverse_valid_trials(matching_trials)
            
            
            matching_neuron_trials[[j]] <- matching_trials_enumeration
        
            
            #This compare to the recoreded touching reuslts for trials in 61 to 80.
            ms_matching[j] <- (sum(focusing_set[matching_trials_enumeration]) > 0)
        }
        tunning_trials_touching_records
        ms_matching
        #matching_neuron_trials        
        #
        
        if (sum(ms_matching == 1) != 0) {
            quality_keeper[s_] <- sum(tunning_trials_touching_records - ms_matching < 0 & ms_matching != 3) / sum(ms_matching == 1)
        } else {
            quality_keeper[s_] = 1.0#Bad neuron
        }
        s_ <- s_ + 1
    }#
    return(quality_keeper)
}



###Variance Learning
###Finding the relevent neurons for prediction false event
quality_calculator_negative <- function(good_neurons_larger) {
    s_ <- 1
    quality_negative_keeper <- rep(0, length(good_neurons_larger))

    for (i in good_neurons_larger) {
        ms_matching <- rep(3, b_ - a_)
        matching_neuron_trials <- vector('list', b_ - a_)
        
        
        
        tunning_trials_touching_records <- rq_series(i, (a_ + 1):b_)$touch
        mid_ <- rq_series(i, 1:a_)
        touching_records <- mid_$touch
        predicted_records <- mid_$relative
        
        
        
        #Deal with the standard deviation
        sd_results <- rep(i, a_)
        for (re in 1:a_) {
            sd_results[re] <- sd(series_maker(i, re)$trials_v[5:25])
        }
        
        #I think (mu - 0.5 * sigmna) is low enough
        threshold <- mean(sd_results, na.rm=TRUE) - 0.5 * sd(sd_results, na.rm=TRUE)        
        
        
        trials_ <- sd_results <= threshold        
        if (sum(trials_, na.rm=TRUE) != 0) {
        				quality_negative_keeper[s_] <- sum(touching_records[trials_], na.rm=TRUE) / sum(trials_, na.rm=TRUE)               
        } else {
        				quality_negative_keeper[s_] <- 1.0#bad neurons!
        }
        s_ <- s_ + 1
    }#
    return(quality_negative_keeper)
}


#False prediction parameter tunning.
qulaity_false_prediction <- function(i_set, t_set) {
    results <- c()
    for (i in i_set) {                
        #Deal with the standard deviation
        sd_results <- rep(i, a_)
        for (t_ in 1:a_) {
            sd_results[t_] <- sd(series_maker(i, t_)$trials_v[5:25])
        }
        
        #I think (mu - 0.5 * sigmna) is low enough
        threshold <- mean(sd_results, na.rm=TRUE) - 0.5 * sd(sd_results, na.rm=TRUE)        
                
        
        sd_negative_prediction <- rep(NA, length(t_set))
        s_ <- 1
        
        for (t_ in t_set) {
            sd_negative_prediction[s_] <- threshold < sd(series_maker(i, t_)$trials_v[5:25])
            s_ <- s_ + 1
        }
        results <- cbind(results, sd_negative_prediction)
    }
    results
}





##

false_prediction_parameter_tunning <- function(false_prediction_neurons, tunning_prediction, false_predictor) {
    para <- seq(0.0,0.95,by=0.03)
    mid_ <- false_predictor#qulaity_false_prediction(false_prediction_neurons, (a_+1):b_)
    y_mid <- rq_series(1, (a_+1):b_)$touch
    mid_results <- rep(NA, length(para))    
    s_ <- 1
    
    for (pa in para) {
    				mid_tunning <- tunning_prediction
    				mid_tunning[rowSums(mid_) <= (length(false_prediction_neurons) * pa)] <- 0
    				mid_tunning[mid_tunning==4] <- NA
        mid_results[s_] <- mean(abs(mid_tunning - y_mid), na.rm=TRUE)
        
        
        if (any(is.nan(mid_results[s_]))) {
            mid_results[s_] <- 0.0
        }
        
        s_ <- s_ + 1
    }
    
    
    para[which(mid_results == min(mid_results))[sum(mid_results == min(mid_results))]]
}




#fds <- qulaity_false_prediction(false_prediction_neurons, (b_+1):c_)

#y_predict[rowSums(fds) <= 17]



###Some computation:
#Training data trials

a_ <- 80
# (a_+1):b_ are the tunning trials 
b_ <- 150
#End run in this data set
#(b_+1):c_ are the testing trials
c_ <- 320 
t_ <- 1:b_
##data has been saved##
results <- rep(0, length(recorded_values[,1]))
results_soft <- rep(0, length(recorded_values[,1]))
whole_matrix <- array(0, c(b_, length(recorded_values[,1])))
soft_matrix <- array(0, c(b_, length(recorded_values[,1])))

for (i in 1:length(recorded_values[,1])) {
				mid_ <- rq_series(i,1:a_)
    results[i] <- mid_$matching_rate_noNA
    results_soft[i] <- mid_$soft_matching_rate_noNA
    whole_matrix[1:a_,i] <- mid_$relative
    soft_matrix[1:a_,i] <- mid_$soft_relative
    
    mid_ <- rq_series(i,(a_+1):b_)
    whole_matrix[(a_+1):b_,i] <- mid_$relative
    soft_matrix[(a_+1):b_,i] <- mid_$soft_relative
    
}





###
###
#e_ <- 40
#'40' is for computational loading condsideration
#good_neurons_larger <- which(results %in% sort(results, de=TRUE)[1:80])[1:80]
good_neurons_larger <- intersect(which(results %in% sort(results, de=TRUE)[1:80])[1:80], which(results_soft %in% sort(results_soft, de=TRUE)[1:80])[1:80])
if (length(good_neurons_larger) >= 40) {good_neurons_larger <- good_neurons_larger[1:40]}
e_ <- length(good_neurons_larger)

stuff <- predictor(good_neurons_larger, (b_+1):c_)

y_predict <- rq_series(1, (b_+1):c_)$touching_trials

#For tau. 
#neurons_selection <- good_neurons_larger##which(results %in% sort(results)[1:100])



y_fitting <- rq_series(1, 1:a_)$touching_trials
y_tunning <- rq_series(1, (a_+1):b_)$touching_trials
fitting_set_lasso <- soft_matrix[1:a_,]






###
###
###
#Variance learning finding the negative predictor(neurons).
##parameter tunning for variance learning
quality_negative <- quality_calculator_negative(good_neurons_larger)

false_prediction_neurons <- good_neurons_larger[quality_negative <= mean(quality_negative) - 0.1 * sd(quality_negative)]

false_tunner <- qulaity_false_prediction(false_prediction_neurons, (a_+1):b_)


###Tunning Parameter
###Fitting the coefficients
##GGG Multi selection:

G_generator <- function(frac) {
    function(x) {
        if (any(is.na(x))) {return(4)}
        X <- x[x != 0]
        
        
        
        if (length(X) >= frac) {
            return(1)       
        } else {
            return(0)
        }
    }
}

#Tunning fraction parameter
which_to_use <- c(0, 0)
para_negative_hard <- rep(0, e_)
para_negative_soft <- rep(0, e_)

#HARD
tunning_set <- whole_matrix[(a_+1):b_,]
frac_results_hard <- rep(0, e_)
s_ <- 1

for (frac_ in 1:e_) {
    G_tmp <- G_generator(frac_)
    
    base_tunning <- apply(tunning_set[,good_neurons_larger], FUN=G_tmp, MARGIN=1)
    
    
    para_mid <- seq(0.01, 0.99, by=0.03)
    results_mid <- rep(NA, length(para_mid))
    o_ <- 1
    for (para_ in para_mid) {
								mid_ <- base_tunning
								
								
								mid_[rowSums(false_tunner) <= length(false_prediction_neurons) * (para_)] <- 0
								results_mid[o_] <- sum(mid_ - y_tunning == 1 | mid_ - y_tunning == -1) / sum(mid_ != 4 & mid_ != 3)
								o_ <- o_ + 1
    }
    para_negative_hard[s_] <- para_mid[which(results_mid == min(results_mid))[1]]
    
    base_tunning[rowSums(false_tunner) <= length(false_prediction_neurons) * (para_negative_hard[s_])] <- 0
    frac_results_hard[s_] <- sum(base_tunning - y_tunning == 1 | base_tunning - y_tunning == -1) / sum(base_tunning != 4 & base_tunning != 3)     
    
    s_ <- s_ + 1
}
which_to_use[1] <- min(frac_results_hard)
#SOFT
tunning_set <- soft_matrix[(a_+1):b_,]
frac_results_soft <- rep(0, e_)
s_ <- 1

for (frac_ in 1:e_) {
    G_tmp <- G_generator(frac_)
    
    base_tunning <- apply(tunning_set[,good_neurons_larger], FUN=G_tmp, MARGIN=1)
    
    
    para_mid <- seq(0.01, 0.99, by=0.03)
    results_mid <- rep(NA, length(para_mid))
    o_ <- 1
    for (para_ in para_mid) {
								mid_ <- base_tunning
								
								
								mid_[rowSums(false_tunner) <= length(false_prediction_neurons) * (para_)] <- 0
								results_mid[o_] <- sum(mid_ - y_tunning == 1 | mid_ - y_tunning == -1) / sum(mid_ != 4 & mid_ != 3)
								o_ <- o_ + 1
    }
    
    
    
    
    para_negative_soft[s_] <- para_mid[which(results_mid == min(results_mid))[1]]

    
    

    base_tunning[rowSums(false_tunner) <= length(false_prediction_neurons) * (para_negative_soft[s_])] <- 0
    
    frac_results_soft[s_] <- sum(base_tunning - y_tunning == 1 | base_tunning - y_tunning == -1) / sum(base_tunning != 4 & base_tunning != 3)     
    
    s_ <- s_ + 1
}
which_to_use[2] <- min(frac_results_soft)



if (which_to_use[1] <= which_to_use[2]) {
				
				prediction_set_logistic <- stuff[[1]]#hard shape
				fitting_set <- whole_matrix[1:a_,]
				tunning_set <- whole_matrix[(a_+1):b_,]				
				win_ <- which(frac_results_hard == min(frac_results_hard))[1]
				
				para_negative <- para_negative_hard[which(frac_results_hard == min(frac_results_hard))[1]]
} else {
				
				prediction_set_logistic <- stuff[[2]]#soft shape
				fitting_set <- soft_matrix[1:a_,]
				tunning_set <- soft_matrix[(a_+1):b_,]
				win_ <- which(frac_results_soft == min(frac_results_soft))[1]
				
				para_negative <- para_negative_soft[which(frac_results_soft == min(frac_results_soft))[1]]
}

prediction_set_lasso <- prediction_set_logistic#predictor(neurons_selection, (b_+1):c_)


para_negative#Set!
win_

G_ <- G_generator(win_)
#For prediction
false_predictor <- qulaity_false_prediction(false_prediction_neurons, (b_+1):c_)






#Logistic.

neurons_selection <- good_neurons_larger
XX <- rbind(fitting_set, tunning_set)[,neurons_selection]
yy <- c(y_fitting, y_tunning)
yy <- yy[!is.na(rowSums(XX))]
XX <- XX[!is.na(rowSums(XX)),]
fit_oga_logistic <- Logistic.fit(XX, yy)










###
###Prediction Process:
#voting process
base_prediction <- apply(prediction_set_logistic, FUN=G_, MARGIN=1)
sum(base_prediction - y_predict == 1 | base_prediction - y_predict == -1, na.rm=TRUE) / sum(!is.na(base_prediction))

which(base_prediction - y_predict == -1)
which(base_prediction - y_predict == 1)


##

###variance learning, again
base_prediction[rowSums(false_predictor) <= length(false_prediction_neurons) * (para_negative)] <- 0
sum(base_prediction - y_predict == 1 | base_prediction - y_predict == -1, na.rm=TRUE) / sum(!is.na(base_prediction))

which(base_prediction - y_predict == -1)
which(base_prediction - y_predict == 1)


























#######
#######
#######Currently stop right here




###SHAPE LEARNING


###
###Tend to be aggressive
###
#good_neurons prediction
#pick up the quality neurons. Give up this process if there is without quality neurons.


#'20' is for computational loading condsideration
ms_neurons <- which(results %in% sort(results, de=TRUE)[1:20])


quality_keeper <- quality_calculator(ms_neurons)
good_neurons <- ms_neurons[quality_keeper <= mean(quality_keeper) - sd(quality_keeper)]
#Total testing trials
d_ <- c_ - b_
#i <- 39
ms_results_ <- c()
for (i in good_neurons) { 
    ms_matching <- rep(NA, d_)
    matching_neuron_trials <- vector('list', d_)
    
    #tunning_trials_touching_records <- rq_series(i, (a_ + 1):(a_ + d_))$touch
    mid_ <- rq_series(i, 1:a_)
    touching_records <- mid_$touch
    predicted_records <- mid_$relative
    
    
    #Quality trials!
    quality_trials <- c()
    ambiguous_trials <- c()
    
    positive_set <- which(touching_records == predicted_records & (predicted_records == 1))
    sd_results_positive <- rep(0, length(positive_set))
    j <- 1
    
    for (t_ in which(touching_records == predicted_records & (predicted_records == 1))) {
        #5:25 data for variance evaluation
        sd_results_positive[j] <- sd(series_maker(i, t_)$trials_v[5:25])
        j <- j + 1
    }
    
    
    
    #For neuron i, trials with shape. If there's no such nerrons, we might just ignore this neuron i.
    quality_trials <- positive_set[which(sd_results_positive >= mean(sd_results_positive) + 0.5 * sd(sd_results_positive))]
    
    no_quality_trials <- mid_$trials[mid_$relative == 0 & !is.na(mid_$relative)]
    
    aimed_trials <- c(quality_trials, no_quality_trials) 
    
    focusing_set <- rep(0, a_)
    focusing_set[quality_trials] <- 1
    
    for (j in 1:d_) {
        
    
        ms_results <- ms_prediction(i, (j + b_), training_data=aimed_trials)
        if (any(is.na(ms_results[[4]]))) {next;}
        matching_trials <- ms_results[[3]][which(ms_results[[4]][[2]] == 1)]
    
        matching_trials_enumeration <- inverse_valid_trials(matching_trials)
        
        
        matching_neuron_trials[[j]] <- matching_trials_enumeration
    
        
        #This compare to the recoreded touching reuslts for trials in 61 to 80.
        ms_matching[j] <- (sum(focusing_set[matching_trials_enumeration]) > 0)
    }
    ms_results_ <- cbind(ms_results_, ms_matching)
}

###Positive parameter tunning
#These are going to be positive prediction
#Adjustment from tunning set!
d_ <- b_ - a_
#i <- 39
ms_results_positive <- c()
for (i in good_neurons) { 
    ms_matching <- rep(NA, d_)
    matching_neuron_trials <- vector('list', d_)
    
    #tunning_trials_touching_records <- rq_series(i, (a_ + 1):(a_ + d_))$touch
    mid_ <- rq_series(i, 1:a_)
    touching_records <- mid_$touch
    predicted_records <- mid_$relative
    
    
    #Quality trials!
    quality_trials <- c()
    ambiguous_trials <- c()
    
    positive_set <- which(touching_records == predicted_records & (predicted_records == 1))
    sd_results_positive <- rep(0, length(positive_set))
    j <- 1
    
    for (t_ in which(touching_records == predicted_records & (predicted_records == 1))) {
        #5:25 data for variance evaluation
        sd_results_positive[j] <- sd(series_maker(i, t_)$trials_v[5:25])
        j <- j + 1
    }
    
    
    
    #For neuron i, trials with shape. If there's no such nerrons, we might just ignore this neuron i.
    quality_trials <- positive_set[which(sd_results_positive >= mean(sd_results_positive) + 0.5 * sd(sd_results_positive))]
    
    no_quality_trials <- mid_$trials[mid_$relative == 0 & !is.na(mid_$relative)]
    
    aimed_trials <- c(quality_trials, no_quality_trials) 
    
    focusing_set <- rep(0, a_)
    focusing_set[quality_trials] <- 1
    
    for (j in 1:d_) {
        
    
        ms_results <- ms_prediction(i, (j + a_), training_data=aimed_trials)
        if (any(is.na(ms_results[[4]]))) {next;}
        matching_trials <- ms_results[[3]][which(ms_results[[4]][[2]] == 1)]
    
        matching_trials_enumeration <- inverse_valid_trials(matching_trials)
        
        
        matching_neuron_trials[[j]] <- matching_trials_enumeration
    
        
        #This compare to the recoreded touching reuslts for trials in 61 to 80.
        ms_matching[j] <- (sum(focusing_set[matching_trials_enumeration]) > 0)
    }
    ms_results_positive <- cbind(ms_results_positive, ms_matching)
}

#Positive shape learning parameter tunning
para_ <- seq(0.02, 0.98, by=0.05)
y_mid <- rq_series(1, (a_+1):b_)$touch
results_mid <- rep(0, length(para_))
s_ <- 1

for (para in para_) {
    mid_ <- rowSums(ms_results_positive) >= (length(ms_results_positive[1,]) * para)
    if (sum(y_mid[mid_], na.rm=TRUE) == 0) {
    				results_mid[s_] <- 0.0
    } else {
    				results_mid[s_] <- mean(y_mid[mid_], na.rm=TRUE)
    }
    s_ <- s_ + 1
}

if (sort(results_mid, de=TRUE)[1] - sort(results_mid, de=TRUE)[2] >= min(frac_results)) {
				para_positive <- para_[which(sort(results_mid, de=TRUE)[2] == results_mid)[1]]
} else {
				para_positive <- para_[which(max(results_mid) == results_mid)[1] - 1]
}

#Use para_positive and ms_reuslts_ for prediction_aux_information
prediction_aux_information <- rowSums(ms_results_) >= (length(ms_results_[1,]) * para_positive)
prediction_aux_information[is.na(prediction_aux_information)] <- 0

base_prediction[which(prediction_aux_information == 1)] <- 1

sum(base_prediction - y_predict == 1 | base_prediction - y_predict == -1, na.rm=TRUE) / sum(!is.na(base_prediction))

which(base_prediction - y_predict == -1)
which(base_prediction - y_predict == 1)




