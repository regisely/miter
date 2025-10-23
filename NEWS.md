# miter 0.0.0.9000

## Initial Development Version

### Features

* Created `miter_tbl` class for nested grouped data
* Implemented `add_workflows()` for adding multiple modeling workflows
* Added data splitting functions:
  - `holdout_split()` and `holdout_time_split()` for train/test splits
  - `cv_split()` and `cv_time_split()` for cross-validation
  - `nested_cv_time_split()` for nested cross-validation
  - `rolling_cv()` for rolling origin cross-validation
* Implemented `fit()` method for miter tables with automatic tuning
* Added `predict()` method for generating predictions
* Created `calculate_metrics()` for model evaluation
* Implemented `autoplot()` methods for visualization
* Added model initialization functions:
  - `initialize_ts_models()` for time series models
  - `initialize_all_models()` for comprehensive model set
  - `initialize_ensemble_models()` for ensemble models
* Created ensemble functionality:
  - `create_ensemble()` for creating ensemble models
  - `add_ensemble()` for adding ensemble predictions
* Added control functions:
  - `control_miter()` for standard tuning
  - `control_miter_race()` for racing with early stopping
* Included example dataset `icms_br` (Brazilian tax revenue data)
* Utility functions:
  - `generate_lags()` for creating lagged features
  - Various internal helper functions

### Documentation

* Complete function documentation with roxygen2
* Package-level documentation
* Data documentation for `icms_br`
* README with examples and workflow
* Comprehensive test suite

### Package Structure

* Imports from tidymodels ecosystem (parsnip, recipes, workflows, rsample, tune, yardstick)
* Suggests modeltime and various modeling engines
* MIT license
