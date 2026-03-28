# -Immune_Analysis.R
A simple R-pipeline to identify significant gene changes (Biomarkers) in immune cells after infection.
# -------------------------------------------------------------------------
# Project: Professional Immune Response Transcriptomics (IRT)
# Script: Immune_Analysis.R
# Author: [Your Name] - DVM & Bioinformatics Aspiring Scholar
# Purpose: Identification of immune biomarkers using Differential Expression
# -------------------------------------------------------------------------

# 1. Environment Setup (Standard Research Libraries)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggrepel")) install.packages("ggrepel")

library(tidyverse)
library(ggrepel) # Handling label overlaps in the plot

# 2. Data Generation: Mocking Clinical Immune Response
# Generating 1000 immune-related genes for 6 samples (3 Control vs 3 Infected)
set.seed(2026)
gene_names <- c(paste0("IL-", 1:10), paste0("TNF-", 1:5), paste0("CXCL-", 1:5), paste0("Gene-", 21:1000))
n_genes <- length(gene_names)

# Generating random expression data with significant signal for immune markers
results_df <- data.frame(
  Gene = gene_names,
  log2FoldChange = rnorm(n_genes, mean = 0, sd = 1.5),
  p_value = runif(n_genes, 0, 1)
)

# Elevating expression values for immune indices to simulate "Up-regulated" Biomarkers
immune_indices <- 1:20
results_df$log2FoldChange[immune_indices] <- runif(20, 2.5, 5)
results_df$p_value[immune_indices] <- runif(20, 1e-10, 1e-5)

# 3. Statistical Significance Thresholding
results_df <- results_df %>%
  mutate(
    neg_log10_p = -log10(p_value),
    Significance = case_when(
      log2FoldChange >= 2 & p_value <= 0.05 ~ "Up-regulated (High)",
      log2FoldChange <= -2 & p_value <= 0.05 ~ "Down-regulated (Low)",
      TRUE ~ "Not Significant"
    )
  )

# 4. High-End Visualization: The Professional Volcano Plot
volcano_plot <- ggplot(results_df, aes(x = log2FoldChange, y = neg_log10_p, color = Significance)) +
  geom_point(alpha = 0.5, size = 1.8) +
  scale_color_manual(values = c("Down-regulated (Low)" = "#2c7fb8", 
                                "Not Significant" = "grey70", 
                                "Up-regulated (High)" = "#e31a1c")) +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "darkgrey") +
  
  # Labeling top immune markers (ILs, TNFs, etc.)
  geom_text_repel(data = head(results_df, 10), aes(label = Gene), 
                  size = 3.5, fontface = "bold", color = "black", max.overlaps = 10) +
  
  theme_classic() + # Academic standard theme
  labs(title = "Immune Landscape Analysis: Pathogen Challenge",
       subtitle = "Differential Gene Expression (DGE) Pipeline in R",
       x = "Log2 Fold Change (Infected / Control)",
       y = "-log10 (P-value)",
       caption = "Analysis by: [Your Name] | Tools: R, ggplot2, ggrepel") +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))

# 5. Output Execution
print(volcano_plot)

# Save output (Optional)
# ggsave("Immune_Analysis_Plot.png", plot = volcano_plot, width = 8, height = 6, dpi = 300)
