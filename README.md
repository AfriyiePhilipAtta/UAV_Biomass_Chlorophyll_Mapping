# 🛰️ UAV Biomass & Chlorophyll Mapping  
A remote sensing workflow using UAV multispectral imagery and supervised machine learning to predict **Leaf Area Index (LAI)** and **chlorophyll content (SPAD)** for precision agriculture.

---

## 📘 Project Summary  
This project integrates **UAV multispectral remote sensing** with **ground truth field measurements** to create spatial prediction maps of crop biophysical properties.

The workflow includes:
- Loading and processing UAV multispectral imagery  
- Computing vegetation indices (NDVI, NDRE, GNDVI, EVI)  
- Extracting spectral values from field subplots  
- Training regression models to predict LAI and SPAD  
- Cross-validation for model evaluation  
- Generating high-resolution prediction maps  
- Correlation analysis between LAI and chlorophyll  

The goal is to support **precision agriculture**, **crop health monitoring**, and **early stress detection** through spatially explicit biophysical mapping.

---

## 🛠️ Technologies Used  
- R (v4.0+)  
- `terra` — raster processing  
- `sf` — vector data and spatial operations  
- `caret` — machine learning and cross-validation  
- `ggplot2` — data visualization  
- `dplyr` — data manipulation  
- `readr` — CSV handling  
- `viridis` — color palettes for maps  

---

## 📂 Project Structure  
```
├── DS4_UAV_Multispectral_Image.tif    # UAV multispectral imagery (4 bands)
├── DS4_AOI.gpkg                        # Area of Interest boundary
├── DS4_Subplots.gpkg                   # Field subplot boundaries
├── DS4_Fielddata.csv                   # Ground truth LAI & SPAD measurements
├── DS4_field_spectral_data.csv         # Extracted spectral values per subplot
├── Predicted_LAI_CV.tif                # LAI prediction map (NDRE-based)
├── Predicted_SPAD_CV.tif               # SPAD prediction map (GNDVI-based)
├── README.md                           # Project documentation
└── LICENSE                             # Apache-2.0 License
```

---

## 📊 Workflow Overview  

### **1. Load UAV Multispectral Imagery**
```r
ms_image <- rast("DS4_UAV_Multispectral_Image.tif")
names(ms_image) <- c("GR", "RD", "RE", "NI")  # Green, Red, Red Edge, NIR
```

### **2. Compute Vegetation Indices**
Four spectral indices were calculated:
- **NDVI** (Normalized Difference Vegetation Index)  
- **NDRE** (Normalized Difference Red Edge Index)  
- **GNDVI** (Green NDVI)  
- **EVI** (Enhanced Vegetation Index)  
```r
ms_image$ndvi  <- (ms_image$NI - ms_image$RD) / (ms_image$NI + ms_image$RD)
ms_image$ndre  <- (ms_image$NI - ms_image$RE) / (ms_image$NI + ms_image$RE)
ms_image$gndvi <- (ms_image$NI - ms_image$GR) / (ms_image$NI + ms_image$GR)
ms_image$evi   <- 2.5 * (ms_image$NI - ms_image$RD) / 
                  (ms_image$NI + 6 * ms_image$RD - 7.5 * ms_image$GR + 1)
```

### **3. Extract Spectral Data from Field Subplots**
Ground truth measurements (LAI, SPAD) were merged with subplot geometries, and spectral values were extracted using spatial averaging.
```r
subplots   <- st_read("DS4_Subplots.gpkg")
field_data <- read_csv("DS4_Fielddata.csv")
field_data_plots <- inner_join(subplots, field_data, by = "layer")

extracted_values <- extract(ms_image, vect(field_data_plots), fun = mean, na.rm = TRUE)
analysis_data <- cbind(st_drop_geometry(field_data_plots), extracted_values[,-1])
```

### **4. Regression Modeling**
Linear regression models were trained to predict LAI and SPAD using each vegetation index:
```r
lm_lai_ndvi  <- lm(LAI ~ ndvi,  data = analysis_data)
lm_lai_gndvi <- lm(LAI ~ gndvi, data = analysis_data)
lm_lai_ndre  <- lm(LAI ~ ndre,  data = analysis_data)
lm_lai_evi   <- lm(LAI ~ evi,   data = analysis_data)

lm_spad_ndvi  <- lm(SPAD ~ ndvi,  data = analysis_data)
lm_spad_gndvi <- lm(SPAD ~ gndvi, data = analysis_data)
lm_spad_ndre  <- lm(SPAD ~ ndre,  data = analysis_data)
lm_spad_evi   <- lm(SPAD ~ evi,   data = analysis_data)
```

### **5. Cross-Validation**
5-fold cross-validation was performed using the `caret` package to evaluate model performance:
```r
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
```

### **6. Generate Prediction Maps**
The best-performing models were applied pixel-by-pixel to create spatial prediction rasters:
```r
# Best-performing indices: NDRE (LAI) and GNDVI (SPAD)
lai_map <- predict(ms_image$ndre, lai_ndre_cv$finalModel)
lai_map[lai_map < 0] <- 0
writeRaster(lai_map, "Predicted_LAI_CV.tif", overwrite = TRUE)

spad_map <- predict(ms_image$gndvi, spad_gndvi_cv$finalModel)
spad_map[spad_map < 0] <- 0
writeRaster(spad_map, "Predicted_SPAD_CV.tif", overwrite = TRUE)
```

### **7. Correlation Analysis**
The relationship between LAI and SPAD was quantified:
```r
correlation <- cor(analysis_data$LAI, analysis_data$SPAD, use = "complete.obs")
print(paste("Correlation between LAI and SPAD:", round(correlation, 3)))
```

---

## 🧪 Model Performance  

### **LAI Prediction**
| Index  | R²   | RMSE  | Best Model |
|--------|------|-------|------------|
| NDRE   | 0.75 | 0.73  | ✅ Selected |
| GNDVI  | 0.70 | 0.80  |            |
| NDVI   | 0.68 | 0.85  |            |
| EVI    | 0.45 | 1.20  |            |

### **SPAD Prediction**
| Index  | R²   | RMSE  | Best Model |
|--------|------|-------|------------|
| GNDVI  | 0.85 | 3.8   | ✅ Selected |
| NDVI   | 0.83 | 3.6   |            |
| NDRE   | 0.78 | 4.2   |            |
| EVI    | 0.50 | 6.5   |            |

**Key Finding:** NDRE was best for LAI prediction, while GNDVI was best for chlorophyll (SPAD) estimation.

---

## 🔍 Key Insights  
- **NDRE** is most sensitive to canopy structure and LAI variations.  
- **GNDVI** outperforms NDVI for chlorophyll estimation due to better sensitivity in dense canopies.  
- **EVI** showed the weakest performance for both LAI and SPAD prediction.  
- LAI and SPAD are **positively correlated** (r ≈ 0.6–0.7), indicating that healthier, denser canopies have higher chlorophyll content.  
- High-resolution UAV imagery enables **field-scale precision mapping** for targeted management.  

---

## 🌍 Applications  
- **Precision agriculture** — site-specific fertilizer and irrigation management  
- **Crop health monitoring** — early detection of nutrient deficiencies and stress  
- **Yield prediction** — LAI and chlorophyll are key indicators of biomass accumulation  
- **Sustainable farming** — optimizing inputs to reduce waste and environmental impact  

---

## 📈 Future Improvements  
- Integrate multi-temporal UAV flights to track crop growth dynamics  
- Test additional indices (MCARI, TCARI, CIgreen)  
- Incorporate machine learning models (Random Forest, XGBoost) for non-linear relationships  
- Automate the workflow using R scripts or Python (via `reticulate`)  
- Validate predictions with independent field datasets  
- Deploy models in a web-based dashboard using Shiny or Streamlit  

---

## 👨‍💻 Author  
**Philip Atta Afriyie**  
Agricultural Data Analytics • Remote Sensing • Machine Learning  

---

## 📜 License  
This project is licensed under the **Apache-2.0 License**.  

---

## ▶️ How to Run the Project  

### **1. Clone the repository**
```bash
git clone https://github.com/your-username/uav-biomass-chlorophyll-mapping.git
cd uav-biomass-chlorophyll-mapping
```

### **2. Install required R packages**
```r
install.packages(c("sf", "terra", "ggplot2", "caret", "readr", "dplyr", "viridis"))
```

### **3. Run the analysis**
Open your R console or RStudio and execute:
```r
source("uav_analysis_workflow.R")
```

Or run interactively step-by-step following the workflow in the README.

---

**⭐ If you find this project useful, please star the repository!**
