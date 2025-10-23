# miter

<!-- badges: start -->
<!-- badges: end -->

**miter** provides tools for iterative modeling and forecasting with tidymodels. It facilitates creating nested tables for grouped time series data, adding multiple modeling workflows, performing cross-validation, fitting models, generating predictions, and creating ensemble models.

## Installation

You can install the development version of miter from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("regisely/miter")
```

## Features

* **Nested Tables**: Create nested `miter_tbl` objects for grouped data
* **Multiple Workflows**: Add and manage multiple modeling workflows per group
* **Time Series Splits**: Create holdout and rolling cross-validation splits
* **Automated Fitting**: Fit models with automatic hyperparameter tuning
* **Predictions**: Generate predictions across all models and groups
* **Ensemble Models**: Create ensemble models from base model predictions
* **Performance Metrics**: Calculate and visualize model performance
* **Visualization**: Built-in plotting for predictions and metrics

## Quick Start

```r
library(dplyr)
library(miter)

# Load example data
data(icms_br)

# Initialize time series models
workflows <- initialize_ts_models(icms_br, "value", "uf", "date")

# Create miter table and fit models
results <- icms_br %>%
  group_by(uf) %>%
  miter_table() %>%
  add_workflows(workflows) %>%
  holdout_time_split(prop = 0.8) %>%
  fit(splits)

# Generate predictions
preds <- results %>%
  predict(fitted_splits)

# Calculate metrics
metrics <- calculate_metrics(preds)

# Visualize
autoplot(preds)
autoplot(metrics)
```

## Workflow Example

### 1. Create Miter Table

```r
tbl <- icms_br %>%
  group_by(uf) %>%
  miter_table()
```

### 2. Add Workflows

```r
workflows <- initialize_ts_models(icms_br, "value", "uf", "date")
tbl <- tbl %>% add_workflows(workflows)
```

### 3. Create Splits

```r
# Holdout split
tbl <- tbl %>% holdout_time_split(prop = 0.8)

# Or create resamples for tuning
tbl <- tbl %>% cv_time_split(splits, initial = 36, assess = 12)
```

### 4. Fit Models

```r
tbl <- tbl %>% fit(splits)
```

### 5. Generate Predictions

```r
preds <- tbl %>% predict(fitted_splits)
```

### 6. Calculate Metrics

```r
metrics <- calculate_metrics(preds)
```

### 7. Create Ensembles

```r
ensemble_workflows <- initialize_ensemble_models(preds, "date")
ensemble_tbl <- create_ensemble(preds, ensemble_workflows)
ensemble_preds <- add_ensemble(ensemble_tbl, preds)
```

## Advanced Features

### Multiple Model Types

```r
# Initialize comprehensive model set
workflows <- initialize_all_models(
  data = icms_br,
  outcome_var = "value",
  id_var = "uf",
  date_var = "date",
  outcome_lags = 3
)
```

### Nested Cross-Validation

```r
tbl <- tbl %>%
  nested_cv_time_split(data) %>%
  fit(nested_cv_data)
```

### Custom Control Parameters

```r
ctrl <- control_miter(
  progress = TRUE,
  verbose = FALSE,
  save_pred = TRUE
)

tbl <- tbl %>% fit(splits, control = ctrl)
```

## Main Functions

### Creating Miter Tables
* `miter_table()`: Create a nested table
* `add_workflows()`: Add modeling workflows

### Data Splitting
* `holdout_split()`, `holdout_time_split()`: Create train/test splits
* `cv_split()`, `cv_time_split()`: Create cross-validation resamples
* `nested_cv_time_split()`: Create nested cross-validation

### Modeling
* `fit()`: Fit models
* `predict()`: Generate predictions
* `calculate_metrics()`: Calculate performance metrics

### Model Initialization
* `initialize_ts_models()`: Initialize time series models
* `initialize_all_models()`: Initialize comprehensive model set
* `initialize_ensemble_models()`: Initialize ensemble models

### Ensembles
* `create_ensemble()`: Create ensemble models
* `add_ensemble()`: Add ensemble predictions

### Utilities
* `control_miter()`, `control_miter_race()`: Control parameters
* `autoplot()`: Visualization

## License

MIT + file LICENSE
