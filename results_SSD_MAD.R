

library(benford.analysis)
library(tidyr)
library(dplyr)

## LOAD DATA 

lib <- read.csv2("YOUR/PATH/library.csv")
dist <- read.csv2("YOUR/PATH/distance_red_deer_enclosures_7.csv")
dist <- merge(dist,lib, by = "id")
distl <- read.csv2("YOUR/PATH/distance_free_ranging_red_deer.csv")
distl <- merge(distl,lib, by = "id")
dist5 <- read.csv2("YOUR/PATH/distance_red_deer_enclosures_5.csv")
dist5 <- merge(dist5,lib, by = "id")
benford <- read.csv2("YOUR/PATH/benford.csv")
benford <- rename(benford, digit = Value)


# dist, distl and dist5 are based on previous code provided



## BENFORD CALCULATION

# Function to calculate frequency, SSD, and MAD for a dataset
calculate_benford_stats <- function(data, digit, number_of_digits = 1) {
  # Count frequencies of digits
  benf <- as.data.frame(table(data[[digit]]))
  total_count <- sum(benf$Freq)
  
  # Calculate frequency in percentage
  benf$frequency <- round((benf$Freq / total_count),4)
  
  # Calculate SSD and MAD
  benford_expected <- round((log10(1 + 1 / (1:9))),4) # Expected Benford frequencies
  ssd <- round(sum((benf$frequency - benford_expected)^2) * 10000, 1)
  mad <- round(benford(data[[digit]], number.of.digits = number_of_digits)$MAD, 4)
  
  list(digit_stats = benf, SSD = ssd, MAD = mad)
}


## PREPARE DATASET LIST

datasets <- list(
  Benford = benford,
  Enclosure7 = dist,
  Enclosure5 = dist5,
  FreeRanging = distl,
  FreeMale = subset(distl, sex == "m"),
  FreeFemale = subset(distl, sex == "f"),
  A = subset(dist, origin == "a"),
  B = subset(dist, origin == "b"),
  C = subset(dist, origin == "c"),
  EnclosureMale7 = subset(dist, sex == "m"),
  EnclosureFemale7 = subset(dist, sex == "f"),
  EnclosureMale5 = subset(dist5, sex == "m"),
  EnclosureFemale5 = subset(dist5, sex == "f")
)

## COMPUTE BENFORD STATISTICS

digit_stats_list <- list() # For digit counts and frequencies
ssd_mad_list <- list()     # For SSD and MAD values

for (name in names(datasets)) {
  result <- calculate_benford_stats(datasets[[name]], "digit")
  
  # Add digit stats for the dataset
  digit_stats_list[[name]] <- result$digit_stats
  
  # Add SSD and MAD values for the dataset
  ssd_mad_list[[name]] <- c(result$SSD, result$MAD)
}

## COMBINE RESULTS

# Combine digit stats into one dataframe
digit_stats_df <- do.call(cbind, lapply(digit_stats_list, function(x) x[, c("Freq", "frequency")]))

# Rename the columns for count and frequency
colnames(digit_stats_df) <- gsub("\\.Freq", " count", colnames(digit_stats_df))
colnames(digit_stats_df) <- gsub("\\.frequency", " frequency", colnames(digit_stats_df))

# Combine SSD and MAD into one dataframe
ssd_mad_df <- data.frame(do.call(rbind, ssd_mad_list))
rownames(ssd_mad_df) <- names(datasets)  # Set row names as dataset names
colnames(ssd_mad_df) <- c("SSD", "MAD")   # Set column names

# Resulting dataframes
digit_stats_df  # First dataframe: counts and frequencies for each dataset (renamed columns)
ssd_mad_df      # Second dataframe: SSD and MAD values for each dataset


##EXPORT RESULTS

write.csv2(digit_stats_df, "frequency_result.csv")
write.csv2(ssd_mad_df,"statistics_result.csv", row.names = TRUE)

