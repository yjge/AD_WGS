library(data.table)  
library(dplyr)    
library(tidyr)    
library(stats)     
library(readr)
file1_path <- "SNV_extract.txt"
genotype_data <- fread(file1_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
genotype_long <- genotype_data %>%
  select(IID, contains(":")) %>%
  gather(key = "Variant", value = "Genotype", -IID)
phenotype_data <- fread("disease_age_sex_center_batch.txt",header = TRUE, sep = "\t", stringsAsFactors = FALSE)
results <- data.frame()
dementia_columns <- c('ACD', 'AD', 'VaD', 'FTD', 'PDD')
merged_data <- genotype_long %>%
  left_join(phenotype_data, by = "IID")
calculate_penetrance <- function(data, variant_name, phenotype){
  variant_data <- data %>%
  filter(Gene == variant_name & (Genotype==1))
  total_carriers <- nrow(variant_data)
  affected_carriers <- variant_data %>% filter(get(phenotype) == 1) %>% nrow()
  penetrance <- ifelse(total_carriers >0, affected_carriers / total_carriers, NA)
  return(data.frame(Gene = variant_name,
                    Phenotype = phenotype,
                    Total_Carriers = total_carriers,
                    Affected_Carriers = affected_carriers,
                    Penetrance = penetrance))
}

results <- data.frame()
for(v in unique(merged_data$Gene)){
  for(p in dementia_columns){
    res <- calculate_penetrance(merged_data, v, p)
    results <- rbind(results, res)
  }
}
output_file <- "genecentric_penetrance.csv"
write.csv(results, output_file, row.names = FALSE)


