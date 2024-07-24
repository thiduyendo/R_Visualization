# Load libraries
library(ggsignif)
library(extrafont)
library(cowplot)
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

# Function to create boxplot with significance
create_boxplot <- function(data, y_var, x_label, comparison) {
  ggplot(data, aes(x = Model, y = !!sym(y_var), fill = Model)) +  
    geom_boxplot() +
    theme_minimal() + theme_bw() +
    theme(panel.grid.major = element_line(color = "lightblue", linetype = "dashed")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 10)) +
    scale_y_continuous(limits = c(0.2, 1.1)) +
    geom_smooth(method='lm') +
    scale_fill_manual(values = c("#00a650", "#f8766d")) +
    labs(x = x_label, y = " ") +
    geom_signif(comparisons = list(comparison), test = "wilcox.test",
                map_signif_level = TRUE, textsize = 3, vjust = -0.2, y_position = c(1)) 
}

# Read data
df0 <- read_excel("Ada_GA_comparison_Training.xlsx")

# Prepare datasets
datasets <- list(
  AUC = df0 %>% select(-c(Accuracy, Precision, Recall, F1score, MCC, Feat_No)),
  Accuracy = df0 %>% select(-c(AUC, Precision, Recall, F1score, MCC, Feat_No)),
  Precision = df0 %>% select(-c(Accuracy, AUC, Recall, F1score, MCC, Feat_No)),
  Recall = df0 %>% select(-c(Accuracy, AUC, Precision, F1score, MCC, Feat_No)),
  F1score = df0 %>% select(-c(Accuracy, AUC, Precision, Recall, MCC, Feat_No)),
  MCC = df0 %>% select(-c(Accuracy, AUC, Precision, Recall, F1score, Feat_No))
)

# Create plots
plots <- lapply(names(datasets), function(var) {
  create_boxplot(datasets[[var]], var, var, c("AdaBoost", "GA-SVM"))
})

# Combine plots
combined_plot <- plot_grid(plotlist = plots, ncol = 2, labels = paste0(1:length(plots)))
print(combined_plot)
ggsave("Metrics_ADA_GARandom_comparison.png")
