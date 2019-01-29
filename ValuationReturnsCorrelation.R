source("https://github.com/KaroRonty/ShillerGoyalDataRetriever/raw/master/ShillerGoyalDataRetriever.r")
library(ggplot2)
library(reshape2)
library(tidyverse)

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
  assign(paste0(measure, "_corr"), correlations_manual, envir = .GlobalEnv)
  write.csv(correlations_manual, paste0(measure, "_correlations.csv"))
}

correlate("capes")
correlate("capbs")
correlate("capds")

# Make correlation graphs
plot_corrs <- function(measure){  
  correlations_manual <- get(paste0(measure, "_corr"))
  correlations_manual <- correlations_manual ^ 2
  graph_data <- as.data.frame(t(correlations_manual))
  graph_data$col <- rownames(graph_data)
  graph_data <- melt(graph_data, variable.name = "Formation", id.vars = c("col"))
  graph_data$Formation <- str_replace(graph_data$Formation, "V", "")
  graph_data$Formation <- factor(graph_data$Formation, levels = unique(graph_data$Formation))
  
  ggplot(graph_data, aes(x = as.numeric(col), y = value, color = Formation)) +
    geom_line() +
    xlab("Measurement period") +
    ylab("R^2") + 
    ggtitle(paste(toupper(substr(measure, 1, 4)), "correlation with subsequent returns"))
}

plot_corrs("capes")
plot_corrs("capbs")
plot_corrs("capds")

# Plot rolling 10-year correlations and trend
cor_holder <- c()
for(i in 1:I(nrow(full_data) - 120)){
  cor_holder <- c(cor_holder, cor(full_data$tenyear[i:I(i + 119)],
                                  full_data$CAPE[i:I(i + 119)]) ^ 2)
}
cor_holder <- as.numeric(na.omit(cor_holder))
plot(cor_holder, type = "l", ann = F)
abline(lm(cor_holder ~ I(1:length(cor_holder))))
title(main = "10-year rolling correlations of CAPE and returns",
      xlab = "Time",
      ylab = "Rolling correlation")
