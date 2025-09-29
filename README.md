UAV-Based Biomass and Chlorophyll Mapping
This project demonstrates the use of UAV multispectral imagery and field data to estimate Leaf Area Index (LAI) and chlorophyll content (SPAD values). The workflow integrates remote sensing, ground truth data, and supervised regression modeling in R to generate spatial prediction maps that support precision agriculture and vegetation monitoring.

📂 Project Structure
* DS4_UAV_Multispectral.tif → UAV multispectral imagery (raw spectral bands).
* DS4_AOI.gpkg → Area of Interest (AOI) boundary shapefile.
* DS4_Subplots.gpkg → Subplot boundaries for field sampling.
* DS4_field_spectral_data.csv → Spectral readings collected in the field.
* DS4_Fielddata.csv → Ground truth LAI and SPAD measurements.
* Predicted_LAI_CV.tif → Predicted LAI raster (cross-validation output using NDRE).
* Predicted_SPAD_CV.tif → Predicted SPAD raster (cross-validation output using GNDVI/NDVI).
* README.md → Project documentation.

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

# Load AOI and field data
aoi <- st_read("DS4_AOI.gpkg")
field_data <- read_csv("DS4_Fielddata.csv")

2. Compute Vegetation Indices

# Vegetation indices
ms_image$ndvi  <- (ms_image$NI - ms_image$RD) / (ms_image$NI + ms_image$RD)
ms_image$ndre  <- (ms_image$NI - ms_image$RE) / (ms_image$NI + ms_image$RE)
ms_image$gndvi <- (ms_image$NI - ms_image$GR) / (ms_image$NI + ms_image$GR)
ms_image$evi   <- 2.5 * ((ms_image$NI - ms_image$RD) / 
                         (ms_image$NI + 6*ms_image$RD - 7.5*ms_image$GR + 1))

3. Model Calibration and Validation

# train control
ctrl <- trainControl(method = "cv", number = 10)

# Train LAI model using NDRE
lai_ndre_cv <- train(LAI ~ NDRE, data = field_data, 
                     method = "lm", trControl = ctrl)

# Train SPAD model using GNDVI
spad_gndvi_cv <- train(SPAD ~ GNDVI, data = field_data, 
                       method = "lm", trControl = ctrl)

NB. Use it for all the indices to select the best performing for the prediction

4. Spatial Prediction

# Predict LAI using NDRE
lai_map <- predict(ms_image$ndre, lai_ndre_cv$finalModel)
lai_map[lai_map < 0] <- 0
plot(lai_map, col = viridis(100), main = "Predicted LAI Map")
writeRaster(lai_map, "Predicted_LAI_CV.tif", overwrite = TRUE)

# Predict SPAD using GNDVI
spad_map <- predict(ms_image$gndvi, spad_gndvi_cv$finalModel)
spad_map[spad_map < 0] <- 0
plot(spad_map, col = viridis(100), main = "Predicted SPAD Map")
writeRaster(spad_map, "Predicted_SPAD_CV.tif", overwrite = TRUE)

📊 Results
* NDRE → Best predictor of LAI (R² ≈ 0.75; RMSE ≈ 0.73).
* GNDVI → Best predictor of SPAD (R² ≈ 0.85).
* NDVI → Strong alternative for SPAD (lowest RMSE ≈ 3.6).
* EVI → Weak performance for both LAI and SPAD.

🌍 Applications
* Precision agriculture (crop vigor and canopy development).
* Yield prediction and early stress detection.
* Supporting sustainable farm management practices.
