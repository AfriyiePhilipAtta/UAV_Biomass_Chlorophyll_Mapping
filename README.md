This project demonstrates the use of UAV multispectral imagery and field data to estimate Leaf Area Index (LAI) and chlorophyll content (SPAD values). The workflow integrates remote sensing, ground truth data, and supervised regression modeling in R to generate spatial prediction maps that support precision agriculture and vegetation monitoring.

📂 Repository Structure
UAV-LAI-SPAD-Prediction/
│
├── datasets/                # Raw UAV imagery, AOI, subplots, and field data
│   ├── DS4_UAV_Multispectral_Image.tif
│   ├── DS4_Subplots.gpkg
│   ├── DS4_Fielddata.csv
│   └── DS4_field_spectral_data.csv
│
├── scripts/                 # R scripts for full workflow
│   ├── compute_indices.R
│   ├── merge_field_data.R
│   ├── train_models.R
│   ├── validate_models.R
│   └── predict_maps.R
│
├── results/                 # Model outputs and prediction maps
│   ├── Predicted_LAI_CV.tif
│   ├── Predicted_SPAD_CV.tif
│   └── performance_summary.csv
│
├── docs/                    # Documentation and figures
│   ├── README.md
│   └── plots/
│       ├── LAI_vs_NDVI.png
│       ├── LAI_vs_NDRE.png
│       ├── SPAD_vs_GNDVI.png
│       ├── Rsquared_comparison.png
│       └── RMSE_comparison.png
│
├── LICENSE                  # Open-source license
└── .gitignore               # Ignore large files, temporary data, logs

            

⚙️ Requirements

Install the following R packages before running the scripts:

install.packages(c("sf", "terra", "ggplot2", "caret", "readr", "dplyr", "viridis"))

🚀 Workflow
1. Load UAV Imagery
ms_image <- rast("DS4_UAV_Multispectral_Image.tif")
names(ms_image) <- c("GR", "RD", "RE", "NI")

2. Compute Vegetation Indices
ms_image$ndvi  <- (ms_image$NI - ms_image$RD) / (ms_image$NI + ms_image$RD)
ms_image$ndre  <- (ms_image$NI - ms_image$RE) / (ms_image$NI + ms_image$RE)
ms_image$gndvi <- (ms_image$NI - ms_image$GR) / (ms_image$NI + ms_image$GR)
ms_image$evi   <- 2.5 * (ms_image$NI - ms_image$RD) / 
                  (ms_image$NI + 6 * ms_image$RD - 7.5 * ms_image$GR + 1)

3. Merge Subplots & Field Data
subplots   <- st_read("DS4_Subplots.gpkg")
field_data <- read_csv("DS4_Fielddata.csv")

field_data_plots <- inner_join(subplots, field_data, by = "layer")

# Extract spectral values per subplot
extracted_values <- extract(ms_image, vect(field_data_plots), fun = mean, na.rm = TRUE)
analysis_data <- cbind(st_drop_geometry(field_data_plots), extracted_values[,-1])
write.csv(analysis_data, "DS4_field_spectral_data.csv", row.names = FALSE)

4. Regression Modeling

Fit models for LAI and SPAD against each vegetation index:

lm_lai_ndvi  <- lm(LAI ~ ndvi,  data = analysis_data)
lm_lai_gndvi <- lm(LAI ~ gndvi, data = analysis_data)
lm_lai_ndre  <- lm(LAI ~ ndre,  data = analysis_data)
lm_lai_evi   <- lm(LAI ~ evi,   data = analysis_data)

lm_spad_ndvi  <- lm(SPAD ~ ndvi,  data = analysis_data)
lm_spad_gndvi <- lm(SPAD ~ gndvi, data = analysis_data)
lm_spad_ndre  <- lm(SPAD ~ ndre,  data = analysis_data)
lm_spad_evi   <- lm(SPAD ~ evi,   data = analysis_data)

Scatterplots are generated for each index with regression lines (using ggplot2).

5. Cross-Validation (Caret)
ctrl <- trainControl(method = "cv", number = 5)

# LAI models
lai_ndvi_cv  <- train(LAI ~ ndvi,  data = analysis_data, method = "lm", trControl = ctrl)
lai_gndvi_cv <- train(LAI ~ gndvi, data = analysis_data, method = "lm", trControl = ctrl)
lai_ndre_cv  <- train(LAI ~ ndre,  data = analysis_data, method = "lm", trControl = ctrl)
lai_evi_cv   <- train(LAI ~ evi,   data = analysis_data, method = "lm", trControl = ctrl)

# SPAD models
spad_ndvi_cv  <- train(SPAD ~ ndvi,  data = analysis_data, method = "lm", trControl = ctrl)
spad_gndvi_cv <- train(SPAD ~ gndvi, data = analysis_data, method = "lm", trControl = ctrl)
spad_ndre_cv  <- train(SPAD ~ ndre,  data = analysis_data, method = "lm", trControl = ctrl)
spad_evi_cv   <- train(SPAD ~ evi,   data = analysis_data, method = "lm", trControl = ctrl)


Performance is compared using R² and RMSE with dotplots.

6. Prediction Maps
# Best-performing indices: NDRE (LAI) and GNDVI (SPAD)
lai_map <- predict(ms_image$ndre, lai_ndre_cv$finalModel)
lai_map[lai_map < 0] <- 0
writeRaster(lai_map, "Predicted_LAI_CV.tif", overwrite = TRUE)

spad_map <- predict(ms_image$gndvi, spad_gndvi_cv$finalModel)
spad_map[spad_map < 0] <- 0
writeRaster(spad_map, "Predicted_SPAD_CV.tif", overwrite = TRUE)

7. Correlation Analysis
correlation <- cor(analysis_data$LAI, analysis_data$SPAD, use = "complete.obs")
print(paste("Correlation between LAI and SPAD:", round(correlation, 3)))

Scatterplot with regression line shows the relationship between LAI and SPAD.

📊 Results
LAI: NDRE performed best (highest R² ≈ 0.75; lowest RMSE ≈ 0.73).
SPAD: GNDVI performed best (highest R² ≈ 0.85); NDVI had the lowest RMSE (~3.6).
EVI: Weakest performance for both LAI and SPAD.
LAI and SPAD were positively correlated (r ≈ 0.6–0.7 depending on dataset).

🌍 Applications
Precision agriculture (crop vigor, canopy development monitoring).
Early stress detection and yield prediction.
Supporting sustainable land and crop management.

🛡️ License
This course is licensed under the Apache-2.0. You are free to use, modify, and share this project with proper attribution.
