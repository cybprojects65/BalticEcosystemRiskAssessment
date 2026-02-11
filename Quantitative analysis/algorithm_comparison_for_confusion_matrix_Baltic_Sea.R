# Clean environment
rm(list=ls(all=TRUE))

# Packages
library(dplyr)

# Function to calculate Accuracy and Kappa
calcPerformance <- function(A_1_B_1, A_0_B_1, A_1_B_0, A_0_B_0, comparisondf){
  accuracy <- (nrow(A_1_B_1) + nrow(A_0_B_0)) * 100 / nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1), nrow(A_1_B_0)),
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  
  # Kappa
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props * col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  
  cat("Accuracy =", accuracy, "%\n")
  cat("Kappa =", k, "\n")
  
  return(c(accuracy, k))
}

anno <- 2020  # fixed year

# # Creazione cartella di output (facoltativa)
# output_folder <- "output_csv"
# if(!dir.exists(output_folder)) dir.create(output_folder)

# Function to map approach -> CSV file
trova_file <- function(approccio, anno){
  file_map <- list(
    "mkm_all"   = "binary_mkm_all_spp.csv",
    "vae_all"   = "binary_vae_all_spp.csv",
    "mkm_five"  = "binary_mkm_five_spp.csv",
    "vae_five"  = "binary_vae_five_spp.csv",
    "mkm_gadus" = "binary_mkm_gadus_morhua.csv",
    "vae_gadus" = "binary_vae_gadus_morhua.csv",
    "mkm_ensamble" = "binary_mkm_ensamble.csv",
    "vae_ensamble" = "binary_vae_ensamble.csv"
  )
  
  if(!approccio %in% names(file_map)){
    stop(paste("Approach not present in file_map:", approccio))
  }
  
  return(file_map[[approccio]])
}

# Function to compare two approaches
confronta_approcci <- function(approccio1, approccio2, anno){
  cat("\n####", approccio1, "vs", approccio2, "anno", anno, "\n")
  
  file1 <- trova_file(approccio1, anno)
  file2 <- trova_file(approccio2, anno)
  
  data1 <- read.csv(file1)
  data2 <- read.csv(file2)
  
  # Create key to match rows
  xy_data1 <- paste0(data1$longitude, ";", data1$latitude)
  xy_data2 <- paste0(data2$longitude, ";", data2$latitude)
  
  if(length(xy_data1) != length(xy_data2) || any(!(xy_data1 %in% xy_data2))){
    stop("Error: coordinates do not match between the two files")
  }
  
  # Align data
  comparisondf <- data.frame(longitude = numeric(0), latitude = numeric(0), A = numeric(0), B = numeric(0))
  
  for(r in 1:nrow(data1)){
    r2 <- which(xy_data2 == xy_data1[r])
    row_data1 <- data1[r,]
    row_data2 <- data2[r2,]
    
    new_row <- data.frame(longitude=row_data1$longitude,
                          latitude=row_data1$latitude,
                          A=row_data1$hotspot,
                          B=row_data2$hotspot)
    comparisondf <- rbind(comparisondf, new_row)
  }
  
  # Compare hotspots with threshold 0.9
  threshold1_A <- 0.9
  threshold1_B <- 0.9
  
  A_1_B_1 <- comparisondf[which(comparisondf$A > threshold1_A & comparisondf$B > threshold1_B),]
  A_0_B_1 <- comparisondf[which(comparisondf$A <= threshold1_A & comparisondf$B > threshold1_B),]
  A_1_B_0 <- comparisondf[which(comparisondf$A > threshold1_A & comparisondf$B <= threshold1_B),]
  A_0_B_0 <- comparisondf[which(comparisondf$A <= threshold1_A & comparisondf$B <= threshold1_B),]
  
  accuracy_k <- calcPerformance(A_1_B_1, A_0_B_1, A_1_B_0, A_0_B_0, comparisondf)
  return(accuracy_k)
}

# List of the 4 comparisons to perform
confronti <- list(
  c("mkm_all", "vae_all"),
  c("mkm_five", "vae_five"),
  c("mkm_gadus", "vae_gadus"),
  c("mkm_ensamble", "vae_ensamble")
)

# Dataframe to store results
comparison_df_total <- data.frame(method1=character(),
                                  method2=character(),
                                  year=numeric(),
                                  accuracy=numeric(),
                                  kappa=numeric())

# Loop through comparisons
for(coppia in confronti){
  approccio1 <- coppia[1]
  approccio2 <- coppia[2]
  
  accuracy_k <- confronta_approcci(approccio1, approccio2, anno)
  
  rowtoinsert <- data.frame(method1=approccio1,
                            method2=approccio2,
                            year=anno,
                            accuracy=accuracy_k[1],
                            kappa=accuracy_k[2])
  
  comparison_df_total <- rbind(comparison_df_total, rowtoinsert)
}

# Export final CSV
write.csv(comparison_df_total, "comparisons_2020.csv", row.names=FALSE)

cat("\nComparison completed! Results saved in comparisons_2020.csv\n")
