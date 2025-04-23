library(readr)
library(dplyr)
library(data.table)
file1_path <- "~/ACD_norm.csv"
file1 <- fread(file1_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)
file2_path <- "~/CIP.csv"
file2 <- read.csv(file2_path, header = TRUE, stringsAsFactors = FALSE)
file3_path <- "~/VUS.csv"
file3 <- read.csv(file3_path, header = TRUE, stringsAsFactors = FALSE)
colnames(file1)[2]<-"variantID"
merged_data <- merge(file2,file1,by="variantID")
output_path <- "~/CIP_merged.csv"  
write.csv(merged_data, file = output_path,  row.names = FALSE, quote = FALSE)
merged_data <- merge(file3,file1,by="variantID")
output_path <- "~/VUS_merged.csv" 
write.csv(merged_data, file = output_path,  row.names = FALSE, quote = FALSE)

CIP_figure <- read_csv("~/CIP_figure.csv")
VUS_figure <- read_csv("~/VUS_figure.csv")

library(ggplot2)
CIP_figure$Classification <- factor(CIP_figure$Classification,
                                    levels = c("Inconclusive", "Supports benign", "VIP"))
p1<-ggplot(CIP_figure, aes(x = MAF, y = OR, color = Classification)) +
  geom_errorbar(
    data = subset(CIP_figure, Classification == "Inconclusive"),
    aes(ymin = LCI, ymax = UCI),
    width = 0.02,
    alpha = 0.3
  ) +
  geom_point(
    data = subset(CIP_figure, Classification == "Inconclusive"),
    alpha = 0.3
  ) +
  geom_errorbar(
    data = subset(CIP_figure, Classification != "Inconclusive"),
    aes(ymin = LCI, ymax = UCI),
    width = 0.02
  ) +
  geom_point(
    data = subset(CIP_figure, Classification != "Inconclusive"),
    alpha = 1
  ) +
  scale_x_log10(
    breaks = c(1e-4, 1e-3, 1e-2),
    labels = c("1e-4", "0.001", "0.01"),
    limits = c(1e-5, 1e-1)
  ) +
  scale_y_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100"),
    limits = c(0.01, 100)
  ) +  
  scale_color_manual(values = c(
    "Inconclusive" = "grey",
    "Supports benign" = mycolor[4],
    "VIP" = mycolor[8]
  )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top",
    legend.direction = "vertical",
    legend.justification = "left"
  ) +
  coord_cartesian(xlim = c(4e-5, 1e-2), ylim = c(0, 80))
labs(
  x = "MAF",
  y = "OR (95%CI)",
  color ="CIP"
)  +
  coord_cartesian(xlim = c(4e-5, 1e-2), ylim = c(0.01, 100))
ggsave("CIP.pdf", plot = p1, width = 16, height = 12, dpi = 300, limitsize = FALSE)

