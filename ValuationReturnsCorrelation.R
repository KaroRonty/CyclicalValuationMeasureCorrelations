source("https://github.com/KaroRonty/ShillerGoyalDataRetriever/raw/master/ShillerGoyalDataRetriever.r")

# Return calculation for the next i years
returns <- as.data.frame(matrix(nrow = nrow(full_data)))
for(i in 1:30){
  temp <- (lead(full_data$index, 12 * i) / full_data$index) ^ (1 / i)
  returns <- cbind(returns, temp)
  temp <- NA
  colnames(returns)[i + 1] <- paste0("ret_", i)
}
returns <- returns %>% select(-V1)

# Calculate book value & real book value
full_data$BookValue <- as.numeric(full_data$bm) * full_data$P
full_data$RealBookValue <- full_data$BookValue * last(full_data$CPI) / full_data$CPI

# Valuation measure calculation for the next i years
capes <- capbs <- capds <- NA
calculate_measures <- function(measure){
  measures <- as.data.frame(matrix(nrow = nrow(full_data)))
  temp_df <- NA
  for(i in 1:30){
    for(j in 1:I(nrow(full_data) - 12)){
      if(measure == "capes"){
        temp_df[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$Earnings[j:I(j + i * 12 - 1)])
      } else if(measure == "capbs"){
        temp_df[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$RealBookValue[j:I(j + i * 12 - 1)])
      } else if(measure == "capds"){
        temp_df[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$Dividend[j:I(j + i * 12 - 1)])
      }
    }
    temp_df <- temp_df[1:nrow(full_data)]
    measures <- cbind(measures, temp_df)
    temp_df <- NA
    colnames(measures)[i + 1] <- paste0("measure_", i)
  }
  measures <- measures %>% select(-V1) 
  assign(measure, measures, envir = .GlobalEnv) 
}

calculate_measures("capes")
calculate_measures("capbs")
calculate_measures("capds")

# Calculate correlations and save into csv for formatting
correlate <- function(measure){
  correlations_manual <- as.data.frame(matrix(nrow = 30, ncol = 30))
  for(i in 1:30){
    for(j in 1:30){
      correlations_manual[i, j] <- cor(get(measure)[, i], returns[, j], use = "complete.obs")
    }
  }
  colnames(correlations_manual) <- 1:30
  write.csv(correlations_manual, paste0(measure, "_correlations.csv"))
}

correlate("capes")
correlate("capbs")
correlate("capds")
