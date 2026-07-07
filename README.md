# SMD-AI-Dx

> **Artificial Intelligence-Assisted Diagnostic System for Severe Mental Disorders**

# 📖 Introduction

**SMD-AI-Dx** is an artificial intelligence-assisted diagnostic system for severe mental disorders built with **R Shiny**. It provides an end-to-end workflow from data preprocessing and feature selection to machine learning prediction and result interpretation.

The platform integrates multiple machine learning algorithms and supports both individual model prediction and ensemble learning, providing intelligent support for clinical decision-making.

---

# ✨ Features

| Module | Description |
|---------|-------------|
| **Data Preprocessing** | Data cleaning, missing value imputation, normalization/standardization, outlier detection |
| **Feature Selection** | Statistical tests and machine learning-based feature selection |
| **Model Prediction** | Logistic Regression, Random Forest, Support Vector Machine, Neural Network, etc. |
| **Ensemble Diagnosis** | Ensemble learning for improved predictive accuracy and robustness |
| **Result Analysis** | ROC curves, confusion matrix, performance evaluation, SHAP feature importance |

---

# 🏗️ Architecture

```text
SMD-AI-Dx
│
├── Data Preprocessing
│
├── Feature Selection
│
├── Model Prediction
│
├── Ensemble Diagnosis
│
├── Result Analysis
│
└── Local Gene Annotation Database (Optional)
```

---

# 📦 Dependencies

## Core Framework

- shiny
- shinydashboard
- shinycssloaders
- DT
- plotly

## Machine Learning

- glmnet (Lasso/Ridge Regression)
- randomForest
- e1071 (Support Vector Machine)
- nnet (Neural Network)
- MASS

## Evaluation & Statistics

- pROC
- ROCR
- epiR
- gmodels

## Data Processing

- dplyr
- foreign
- zip

---

# 🚀 Quick Start

## 1. Clone Repository

```bash
git clone https://github.com/YJF0329/SMD-AI-Dx.git
cd SMD-AI-Dx
```

---

## 2. Install Required Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinycssloaders",
  "DT",
  "plotly",
  "glmnet",
  "randomForest",
  "e1071",
  "nnet",
  "MASS",
  "pROC",
  "ROCR",
  "epiR",
  "gmodels",
  "dplyr",
  "foreign",
  "zip"
))
```

---

## 3. Download Local Gene Annotation Database (Optional)

```bash
Rscript download_gene_mapping.R
```

---

## 4. Launch Application

```r
shiny::runApp("app.R")
```

---

# 📁 Project Structure

```text
SMD-AI-Dx/
│
├── app.R
│
├── ui_preprocessing.R
├── server_preprocessing.R
├── preprocessing_functions.R
│
├── local_annotation_functions.R
│
├── ui_feature_selection.R
├── server_feature_selection.R
│
├── ui_prediction.R
├── server_predict_model.R
│
├── ui_ensemble_predict.R
├── server_ensemble_predict.R
│
├── ui_analysis.R
├── server_analysis.R
│
├── local/
│   └── download_gene_mapping.R
│
└── README.md
```

---

# 🧠 Workflow

```text
Upload Data
      │
      ▼
Data Preprocessing
      │
      ▼
Feature Selection
      │
      ▼
Model Training & Prediction
      │
      ├────────► Single Model Prediction
      │
      └────────► Ensemble Diagnosis
                     │
                     ▼
             Result Analysis
        (ROC / Confusion Matrix / SHAP)
```

---

# 🛠️ Technology Stack

| Technology | Purpose |
|------------|---------|
| R Shiny | Web application framework |
| shinydashboard | Dashboard UI |
| glmnet | Logistic/Lasso/Ridge Regression |
| randomForest | Random Forest |
| e1071 | Support Vector Machine |
| nnet | Neural Network |
| pROC / ROCR | Model evaluation |
| plotly | Interactive visualization |
| DT | Interactive data tables |

---

# 📧 Contact

- 📮 Submit an Issue
- 📧 Email: jingfan.yao@sxmu.edu.cn

---

# ⭐ Support

If you find this project useful, please consider giving it a **Star ⭐** on GitHub!
