# ============================================================
# Project: UAV-Based Vegetation Indices → LAI & SPAD Prediction
# Dataset: DS4
# ============================================================

# --- Load libraries ---
library(sf)
library(terra)
library(ggplot2)
library(caret)
library(readr)
library(dplyr)
library(viridis)

# --- Set working directory ---
setwd("/Users/User/Downloads/GIS_Engineer/DS4")

# ============================================================
# 1. Load UAV Imagery
# ============================================================
ms_image <- rast("DS4_UAV_Multispectral_Image.tif")
names(ms_image) <- c("GR", "RD", "RE", "NI")   # adjust order if needed
print(ms_image)

# ============================================================
# 2. Compute Vegetation Indices
# ============================================================
ms_image$ndvi  <- (ms_image$NI - ms_image$RD) / (ms_image$NI + ms_image$RD)
ms_image$ndre  <- (ms_image$NI - ms_image$RE) / (ms_image$NI + ms_image$RE)
ms_image$gndvi <- (ms_image$NI - ms_image$GR) / (ms_image$NI + ms_image$GR)
ms_image$evi   <- 2.5 * (ms_image$NI - ms_image$RD) / (ms_image$NI + 6 * ms_image$RD - 7.5 * ms_image$GR + 1)

# ============================================================
# 3. Load Subplots & Field Data
# ============================================================
subplots   <- st_read("DS4_Subplots.gpkg")
field_data <- read_csv("DS_4_Fielddata.csv")   # contains LAI & SPAD
field_data_plots <- inner_join(subplots, field_data, by = "layer")

# ============================================================
# 4. Extract Spectral & VI Values
# ============================================================
extracted_values <- extract(ms_image, vect(field_data_plots), fun = mean, na.rm = TRUE)
analysis_data <- cbind(st_drop_geometry(field_data_plots), extracted_values[,-1])
write.csv(analysis_data, "DS4_field_spectral_data.csv", row.names = FALSE)
head(analysis_data)

# ============================================================
# 5. Regression Modeling & Plots (LAI)
# ============================================================
lm_lai_ndvi  <- lm(LAI ~ ndvi,  data = analysis_data)
lm_lai_gndvi <- lm(LAI ~ gndvi, data = analysis_data)
lm_lai_ndre  <- lm(LAI ~ ndre,  data = analysis_data)
lm_lai_evi   <- lm(LAI ~ evi,   data = analysis_data)

# Plots for LAI vs indices
ggplot(analysis_data, aes(x = ndvi, y = LAI)) +
  geom_point(color = "darkgreen", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "LAI vs NDVI", x = "NDVI", y = "LAI") + theme_minimal()

ggplot(analysis_data, aes(x = gndvi, y = LAI)) +
  geom_point(color = "blue", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "LAI vs GNDVI", x = "GNDVI", y = "LAI") + theme_minimal()

ggplot(analysis_data, aes(x = ndre, y = LAI)) +
  geom_point(color = "orange", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "LAI vs NDRE", x = "NDRE", y = "LAI") + theme_minimal()

ggplot(analysis_data, aes(x = evi, y = LAI)) +
  geom_point(color = "brown", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "LAI vs EVI", x = "EVI", y = "LAI") + theme_minimal()

# ============================================================
# 6. Regression Modeling & Plots (SPAD)
# ============================================================
lm_spad_ndvi  <- lm(SPAD ~ ndvi,  data = analysis_data)
lm_spad_gndvi <- lm(SPAD ~ gndvi, data = analysis_data)
lm_spad_ndre  <- lm(SPAD ~ ndre,  data = analysis_data)
lm_spad_evi   <- lm(SPAD ~ evi,   data = analysis_data)

# Plots for SPAD vs indices
ggplot(analysis_data, aes(x = ndvi, y = SPAD)) +
  geom_point(color = "darkgreen", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "SPAD vs NDVI", x = "NDVI", y = "SPAD") + theme_minimal()

ggplot(analysis_data, aes(x = gndvi, y = SPAD)) +
  geom_point(color = "blue", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "SPAD vs GNDVI", x = "GNDVI", y = "SPAD") + theme_minimal()

ggplot(analysis_data, aes(x = ndre, y = SPAD)) +
  geom_point(color = "orange", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "SPAD vs NDRE", x = "NDRE", y = "SPAD") + theme_minimal()

ggplot(analysis_data, aes(x = evi, y = SPAD)) +
  geom_point(color = "brown", size = 3) + geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "SPAD vs EVI", x = "EVI", y = "SPAD") + theme_minimal()

# ============================================================
# 7. Cross-Validation (Caret, 4 Indices)
# ============================================================
ctrl <- trainControl(method = "cv", number = 5)

# --- LAI models ---
lai_ndvi_cv  <- train(LAI ~ ndvi,  data = analysis_data, method = "lm", trControl = ctrl)
lai_gndvi_cv <- train(LAI ~ gndvi, data = analysis_data, method = "lm", trControl = ctrl)
lai_ndre_cv  <- train(LAI ~ ndre,  data = analysis_data, method = "lm", trControl = ctrl)
lai_evi_cv   <- train(LAI ~ evi,   data = analysis_data, method = "lm", trControl = ctrl)

resamps_lai <- resamples(list(
  "NDVI"  = lai_ndvi_cv,
  "GNDVI" = lai_gndvi_cv,
  "NDRE"  = lai_ndre_cv,
  "EVI"   = lai_evi_cv
))
summary(resamps_lai)
dotplot(resamps_lai, metric = "Rsquared")
dotplot(resamps_lai, metric = "RMSE")

# --- SPAD models ---
spad_ndvi_cv  <- train(SPAD ~ ndvi,  data = analysis_data, method = "lm", trControl = ctrl)
spad_gndvi_cv <- train(SPAD ~ gndvi, data = analysis_data, method = "lm", trControl = ctrl)
spad_ndre_cv  <- train(SPAD ~ ndre,  data = analysis_data, method = "lm", trControl = ctrl)
spad_evi_cv   <- train(SPAD ~ evi,   data = analysis_data, method = "lm", trControl = ctrl)

resamps_spad <- resamples(list(
  "NDVI"  = spad_ndvi_cv,
  "GNDVI" = spad_gndvi_cv,
  "NDRE"  = spad_ndre_cv,
  "EVI"   = spad_evi_cv
))
summary(resamps_spad)
dotplot(resamps_spad, metric = "Rsquared")
dotplot(resamps_spad, metric = "RMSE")

# ============================================================
# 8. Mapping Predictions (using the best performing indices)
# ============================================================
lai_map <- predict(ms_image$ndre, lai_ndre_cv$finalModel)
lai_map[lai_map < 0] <- 0
plot(lai_map, col = viridis(100), main = "Predicted LAI Map")
writeRaster(lai_map, "Predicted_LAI_CV.tif", overwrite = TRUE)

spad_map <- predict(ms_image$gndvi, spad_gndvi_cv$finalModel)
spad_map[spad_map < 0] <- 0
plot(spad_map, col = viridis(100), main = "Predicted SPAD Map")
writeRaster(spad_map, "Predicted_SPAD_CV.tif", overwrite = TRUE)

# ============================================================
# 9. Correlation LAI vs SPAD
# ============================================================
correlation <- cor(analysis_data$LAI, analysis_data$SPAD, use = "complete.obs")
print(paste("Correlation between LAI and SPAD:", round(correlation, 3)))

ggplot(analysis_data, aes(x = LAI, y = SPAD)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = paste("LAI vs SPAD (r =", round(correlation, 3), ")"),
       x = "LAI", y = "SPAD") +
  theme_minimal()

