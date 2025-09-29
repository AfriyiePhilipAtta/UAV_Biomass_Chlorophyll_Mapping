This project demonstrates the use of UAV multispectral imagery and field data to estimate Leaf Area Index (LAI) and chlorophyll content (SPAD values). The workflow integrates remote sensing, ground truth data, and supervised regression modeling in R to generate spatial prediction maps that support precision agriculture and vegetation monitoring.

📂 Project Structure

DS4_UAV_Multispectral.tif → UAV multispectral imagery (raw spectral bands).

DS4_AOI.gpkg → Area of Interest (AOI) boundary shapefile.

DS4_Subplots.gpkg → Subplot boundaries for field sampling.

DS4_field_spectral_data.csv → Spectral readings collected in the field.

DS4_Fielddata.csv → Ground truth LAI and SPAD measurements.

Predicted_LAI_Best.tif → Predicted LAI raster (using best-performing index).

Predicted_SPAD_Best.tif → Predicted SPAD raster (using best-performing index).

README.md → Project documentation.

⚙️ Requirements

Install the following R packages before running the scripts:

install.packages(c("sf", "terra", "ggplot2", "caret", "readr", "dplyr", "viridis"))

🚀 Workflow
1. Load Data
library(sf)
library(terra)
library(caret)
library(dplyr)
library(viridis)

# Load UAV imagery
ms_image <- rast("DS4_UAV_Multispectral.tif")
names(ms_image) <- c("GR", "RD", "RE", "NI")

# Load AOI, subplots, and field data
aoi <- st_read("DS4_AOI.gpkg")
subplots <- st_read("DS4_Subplots.gpkg")
field_data <- read_csv("DS4_Fielddata.csv")

2. Merge Field Data
# Convert subplot polygons to centroids
subplots_centroid <- st_centroid(subplots)

# Join field data with subplot geometry
field_data_sf <- subplots_centroid %>%
  left_join(field_data, by = "subplot_id")

# Extract spectral values from UAV imagery
spectral_data <- terra::extract(ms_image, vect(field_data_sf))

# Merge indices with ground truth measurements
merged_data <- cbind(field_data_sf, spectral_data)

3. Compute Vegetation Indices
# Vegetation indices
ms_image$ndvi  <- (ms_image$NI - ms_image$RD) / (ms_image$NI + ms_image$RD)
ms_image$ndre  <- (ms_image$NI - ms_image$RE) / (ms_image$NI + ms_image$RE)
ms_image$gndvi <- (ms_image$NI - ms_image$GR) / (ms_image$NI + ms_image$GR)
ms_image$evi   <- 2.5 * ((ms_image$NI - ms_image$RD) / 
                         (ms_image$NI + 6*ms_image$RD - 7.5*ms_image$GR + 1))

4. Train Models for All Indices
# Train control
ctrl <- trainControl(method = "cv", number = 10)

# Train models for LAI
lai_models <- list(
  NDVI  = train(LAI ~ ndvi, data = merged_data, method = "lm", trControl = ctrl),
  NDRE  = train(LAI ~ ndre, data = merged_data, method = "lm", trControl = ctrl),
  GNDVI = train(LAI ~ gndvi, data = merged_data, method = "lm", trControl = ctrl),
  EVI   = train(LAI ~ evi, data = merged_data, method = "lm", trControl = ctrl)
)

# Train models for SPAD
spad_models <- list(
  NDVI  = train(SPAD ~ ndvi, data = merged_data, method = "lm", trControl = ctrl),
  NDRE  = train(SPAD ~ ndre, data = merged_data, method = "lm", trControl = ctrl),
  GNDVI = train(SPAD ~ gndvi, data = merged_data, method = "lm", trControl = ctrl),
  EVI   = train(SPAD ~ evi, data = merged_data, method = "lm", trControl = ctrl)
)

5. Compare Performance
# Summarize results
lai_results <- resamples(lai_models)
spad_results <- resamples(spad_models)

summary(lai_results)
summary(spad_results)

# Visualize comparisons
bwplot(lai_results, metric = "Rsquared")
bwplot(spad_results, metric = "Rsquared")
bwplot(lai_results, metric = "RMSE")
bwplot(spad_results, metric = "RMSE")

6. Select Best Model Automatically
# Select best LAI model (highest R²)
lai_best <- lai_models[[which.max(sapply(lai_models, function(m) max(m$results$Rsquared)))]]
lai_best_index <- names(lai_models)[which.max(sapply(lai_models, function(m) max(m$results$Rsquared)))]

# Select best SPAD model (highest R²)
spad_best <- spad_models[[which.max(sapply(spad_models, function(m) max(m$results$Rsquared)))]]
spad_best_index <- names(spad_models)[which.max(sapply(spad_models, function(m) max(m$results$Rsquared)))]

print(lai_best)
print(spad_best)

7. Generate Prediction Maps
# Predict LAI using best index
lai_map <- predict(ms_image[[tolower(lai_best_index)]], lai_best$finalModel)
lai_map[lai_map < 0] <- 0
plot(lai_map, col = viridis(100), main = paste("Predicted LAI Map -", lai_best_index))
writeRaster(lai_map, "Predicted_LAI_Best.tif", overwrite = TRUE)

# Predict SPAD using best index
spad_map <- predict(ms_image[[tolower(spad_best_index)]], spad_best$finalModel)
spad_map[spad_map < 0] <- 0
plot(spad_map, col = viridis(100), main = paste("Predicted SPAD Map -", spad_best_index))
writeRaster(spad_map, "Predicted_SPAD_Best.tif", overwrite = TRUE)

📊 Results

LAI → NDRE was selected as the best-performing index (highest R², lowest RMSE).

SPAD → GNDVI was selected as the best-performing index (highest R²), though NDVI showed the lowest RMSE.

EVI underperformed for both LAI and SPAD.

🌍 Applications

Precision agriculture (crop vigor and canopy development).

Yield prediction and early stress detection.

Supporting sustainable farm management practices.
