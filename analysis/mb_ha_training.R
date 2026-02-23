library(tidyverse)
library(tidysdm)
library(sf)
library(terra)
library(tidyterra)
library(ggbeeswarm)
library(DALEX)
library(DALEXtra)
library(units)
library(future)
library(patchwork)

library(azores.fkw)

future::plan(multisession, workers = 10)
set.seed(seed = 42)

bkw_occurrence <- azores.bkw::bkw_occurrence

#
# Create a occurrence for Mb and Ha, by keeping absences only for species that not bkw.
#

mb_ha_occurrence01 <-
  bkw_occurrence |>
  dplyr::filter((
    class == "presence" &
      species %in% c("Mesoplodon bidens", "Hyperoodon ampullatus")
  ) |
    (
      class == "absence" & !species %in% c(
        "Mesoplodon mirus",
        "Mesoplodon densirostris",
        "Mesoplodon europaeus",
        "Ziphius cavirostris"
      )
    ))

#
# Define data set
#
vars_of_interest <- c(
  "class",
  "geometry",
  "sst",
  # "hmlmeso",
  "lmeso",
  "mlmeso",
  "depth",
  "slope")


mb_ha_occurrence02 <-
  mb_ha_occurrence01 |>
  dplyr::select(dplyr::all_of(vars_of_interest)) |>
  dplyr::mutate(class = factor(class, levels = c("presence", "absence"))) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    sst = as.double(sst),
    lmeso = as.double(lmeso),
    mlmeso = as.double(mlmeso),
    depth = as.double(depth),
    slope = as.double(slope))

mb_ha_occurrence <- mb_ha_occurrence02

mb_ha_recipe <- recipes::recipe(x = mb_ha_occurrence, class ~ .)
mb_ha_occurrence |> tidysdm::check_sdm_presence(class)

mb_ha_models01 <-
  # create the workflow_set
  workflowsets::workflow_set(
    preproc = list(default = mb_ha_recipe),
    models = list(
      # the standard glm specs
      glm = tidysdm::sdm_spec_glm(),
      # rf specs with tuning
      rf = tidysdm::sdm_spec_rf(),
      # boosted tree model (gbm) specs with tuning
      gbm = tidysdm::sdm_spec_boost_tree(),
      # maxent specs with tuning
      maxent = tidysdm::sdm_spec_maxent()
    ),
    # make all combinations of preproc and models,
    cross = TRUE
  ) |>
  # tweak controls to store information needed later to create the ensemble
  workflowsets::option_add(control = tune::control_grid(save_pred = TRUE, save_workflow = TRUE, parallel_over = "everything"))

mb_ha_cv <- spatialsample::spatial_block_cv(mb_ha_occurrence, v = 5)
autoplot(mb_ha_cv)

mb_ha_models02 <-
  mb_ha_models01 |>
  workflowsets::workflow_map(
    "tune_grid",
    resamples = mb_ha_cv,
    grid = 3,
    metrics = tidysdm::sdm_metric_set(),
    verbose = TRUE
  )

mb_ha_models <- mb_ha_models02

mb_ha_ensemble <- simple_ensemble() |>
  add_member(mb_ha_models, metric = "roc_auc")


mb_ha_model_metrics <- mb_ha_ensemble |> collect_metrics()
mb_ha_ensemble_explainer <- explain_tidysdm(mb_ha_ensemble, by_workflow = TRUE)

autoplot(mb_ha_models)
autoplot(mb_ha_ensemble)


# Export occurrences
readr::write_rds(x = mb_ha_occurrence, file = "analysis/occurrences/mb_ha_occurrence.rds", compress = "xz")

# Export model
readr::write_rds(x = mb_ha_ensemble, file = "analysis/models/ensemble-model-mb-ha.rds", compress = "xz")

# Export model metrics
readr::write_rds(mb_ha_model_metrics, file = "analysis/models/mb_ha_model_metrics.rds")
readr::write_rds(mb_ha_ensemble_explainer, file = "analysis/models/mb_ha_ensemble_explainer.rds")
