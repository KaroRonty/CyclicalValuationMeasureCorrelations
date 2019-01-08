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


# CAPE calculation for the next i years
capes <- as.data.frame(matrix(nrow = nrow(full_data)))
temp2 <- NA
for(i in 1:30){
  for(j in 1:I(nrow(full_data) - 12)){
    temp2[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$Earnings[j:I(j + i * 12 - 1)])
  }
  temp2 <- temp2[1:nrow(full_data)]
  capes <- cbind(capes, temp2)
  temp2 <- NA
  colnames(capes)[i + 1] <- paste0("cape_", i)
}
capes <- capes %>% select(-V1) 

# Calculate book value
full_data$BookValue <- as.numeric(full_data$bm) * full_data$Price

# CAPB calculation for the next i years
capbs <- as.data.frame(matrix(nrow = nrow(full_data)))
temp3 <- NA
for(i in 1:30){
  for(j in 1:I(nrow(full_data) - 12)){
    temp3[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$BookValue[j:I(j + i * 12 - 1)])
  }
  temp3 <- temp3[1:nrow(full_data)]
  capbs <- cbind(capbs, temp3)
  temp3 <- NA
  colnames(capbs)[i + 1] <- paste0("capb_", i)
}
capbs <- capbs %>% select(-V1)

# CAPD calculation for the next i years
capds <- as.data.frame(matrix(nrow = nrow(full_data)))
temp4 <- NA
for(i in 1:30){
  for(j in 1:I(nrow(full_data) - 12)){
    temp4[j + i * 12] <- full_data$Price[j + i * 12] / mean(full_data$Dividend[j:I(j + i * 12 - 1)])
  }
  temp4 <- temp4[1:nrow(full_data)]
  capds <- cbind(capds, temp4)
  temp4 <- NA
  colnames(capds)[i + 1] <- paste0("capd_", i)
}
capds <- capds %>% select(-V1)


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
