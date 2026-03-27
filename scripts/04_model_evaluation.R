# scripts/04_model_evaluation.R
if (!require("ggplot2")) install.packages("ggplot2", repos="http://cran.rstudio.com/")
if (!require("randomForest")) install.packages("randomForest", repos="http://cran.rstudio.com/")

library(ggplot2)
library(randomForest)

print("Starting Model Evaluation Graphs...")

out_dir <- "outputs/plots"
if (!dir.exists(out_dir) && dir.exists("../outputs/plots")) {
    out_dir <- "../outputs/plots"
} else if (!dir.exists(out_dir)) {
    dir.create("outputs")
    dir.create("outputs/plots")
}

models_dir <- "models"
if (!dir.exists(models_dir) && dir.exists("../models")) {
    models_dir <- "../models"
}

# --- 1. Random Forest Evaluation ---
rf_model_path <- file.path(models_dir, "rf_demand_model.rds")
if (file.exists(rf_model_path)) {
    rf_model <- readRDS(rf_model_path)
    
    # 1A. Actual vs Predicted Plot
    if (!is.null(rf_model$y) && !is.null(rf_model$predicted)) {
        rmse <- sqrt(mean((rf_model$y - rf_model$predicted)^2))
        df_perf <- data.frame(Actual = rf_model$y, Predicted = rf_model$predicted)
        
        p1 <- ggplot(df_perf, aes(x=Actual, y=Predicted)) +
          geom_point(alpha=0.5, color="steelblue", size=3) +
          geom_abline(slope=1, intercept=0, color="red", linetype="dashed", size=1) +
          theme_minimal() +
          labs(title="Random Forest: Actual vs Predicted Demand",
               x="Actual Daily Quantity", y="Predicted Daily Quantity", 
               subtitle = paste("Training RMSE:", round(rmse, 2))) +
          theme(text = element_text(size=14))
        
        ggsave(file.path(out_dir, "rf_actual_vs_predicted.png"), plot = p1, width = 8, height = 6)
        print("rf_actual_vs_predicted.png created.")
    }
    
    # 1B. Variable Importance Plot
    if (!is.null(rf_model$importance)) {
        imp <- as.data.frame(rf_model$importance)
        imp$Variable <- rownames(imp)
        imp$Score <- imp[, ncol(imp)]
        
        p2 <- ggplot(imp, aes(x=reorder(Variable, Score), y=Score, fill=Variable)) +
          geom_bar(stat="identity") +
          coord_flip() +
          theme_minimal() +
          labs(title="Random Forest: Feature Importance", x="Features", y="Importance Score") +
          theme(legend.position="none", text = element_text(size=14))
          
        ggsave(file.path(out_dir, "rf_feature_importance.png"), plot = p2, width = 8, height = 6)
        print("rf_feature_importance.png created.")
    }
} else {
    print("rf_demand_model.rds not found.")
}

# --- 2. K-Means Evaluation ---
segments_path <- file.path(models_dir, "customer_segments.rds")
if (file.exists(segments_path)) {
    segments_df <- readRDS(segments_path)
    
    if (nrow(segments_df) > 0) {
        p3 <- ggplot(segments_df, aes(x=frequency, y=monetary, color=Cluster, size=monetary)) +
          geom_point(alpha=0.7) +
          theme_minimal() +
          labs(title="K-Means Clustering: Customer Segments",
               x="Purchase Frequency", y="Total Monetary Value (₹)") +
          scale_color_brewer(palette="Set1") +
          theme(text = element_text(size=14))
          
        ggsave(file.path(out_dir, "kmeans_customer_segments.png"), plot = p3, width = 8, height = 6)
        print("kmeans_customer_segments.png created.")
    }
} else {
    print("customer_segments.rds not found.")
}

print("Model Evaluation Complete!")
