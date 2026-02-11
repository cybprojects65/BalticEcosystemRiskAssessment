# 🌊 Baltic Sea Ecosystem Risk Analysis Repository

This repository contains the full material related to the ecosystem risk analysis conducted for the Baltic Sea at spatial resolution 0.1° × 0.1°.
The analysis combines environmental stressors, human activities, and species-related variables using unsupervised learning and deep learning approaches to identify ecosystem risk hotspots.

The repository includes datasets, R scripts, model outputs, hotspot maps, and quantitative comparison analyses.

> 📌 **QGIS Project Availability**  
> A QGIS project, supporting the cartographic visualisation of data, is available on the Zenodo repository at the following link:  
> https://doi.org/10.5281/zenodo.18493559

---

## 📂 Repository Structure

The repository is organized by methodological approach and analysis type.

├── Baltic Sea Multi k-means
├── Baltic sea VAE
├── Heatmap Baltic Sea
├── Quantitative analysis
├── List_of_species.xlsx
├── dataset_baltic_sea_2020_ices2126_original.csv
├── Metadata_baltic_sea.xlsx


---

## 🔬 Methods Included

### Multi K-means Analysis

Contains all scripts and outputs related to the Multi K-means clustering workflow applied to standardized ecosystem stressors.

### Variational Autoencoder (VAE)

Contains deep learning outputs and scripts used to derive reconstruction probabilities and anomaly/risk patterns from the stressor dataset.

The VAE directory also includes a dedicated subfolder containing the results of the Multi K-means clustering applied to the VAE-derived outputs, enabling a secondary clustering-based interpretation of reconstruction probability patterns and risk structures.

---

## 🧬 Species Group Subdivision

Both Multi K-means and VAE folders are further subdivided into species-based analytical groups:

- Species richness
- Commercial species
- Single relevant species (Gadus morhua)

Each subgroup contains independent analyses and outputs.

---

## 📊 Contents Inside Each Method Subfolder

Each species-group subfolder includes:

- Stressors dataset
- R scripts used for:
  - clustering or VAE processing
- Output CSV files with:
  - cluster assignments (Multi K-means)
  - reconstruction probabilities (VAE)
- Refinement result files:
  - classification of ecosystem risk hotspots
  - post-processing outputs

---

## 🗺️ Heatmap Baltic Sea

This folder contains the generated spatial outputs:

- Ecosystem risk hotspot maps
- Heatmaps derived from clustering and VAE outputs
- Visualization products used for spatial interpretation

---

## 📈 Quantitative Analysis

This folder includes:

- Scripts and datasets used for quantitative comparison
- Agreement metrics between methods
- Cross-method hotspot comparison results

---

## 🐟 Species List

**Species_list_145.xlsx**

Contains the complete list of the 145 species included in the analysis.

---

## 🌍 Full Dataset

**dataset_baltic_sea_2020_ices2126_original.csv**

Contains the complete processed dataset used in the analysis, including:

- Latitude
- Longitude
- Environmental variables
- Anthropogenic variables
- Species-related variables

Spatial resolution: 0.1° × 0.1° grid

---

## 📚 Metadata and Data Sources

**metadata_baltic_sea.xlsx**

Contains:

- Metadata description of all variables
- Source repositories and portals
- Data provider links (e.g., Copernicus, HELCOM, and related services)

---

## ⚙️ Software Used

- R / RStudio
- Java
- QGIS

---

## 📌 Notes

This repository is intended to ensure:

- transparency
- reproducibility
- methodological traceability
