library(tidyverse)
library(tidysdm)
library(sf)
library(terra)
library(tidyterra)
library(ggbeeswarm)
library(DALEX)
library(units)
library(future)
library(patchwork)

library(azores.fkw)


future::plan(multisession, workers = 10)

bkw_occurrence <- sf::st_as_sf(azores.bkw::bkw_occurrence, coords = "geometry")

#
# Create a occurrence for Mb and Ha, by keeping absences only for species that not bkw.
#

mb_ha_occurrence <- bkw_occurrence |>
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


mb_ha_occurrence <-
  mb_ha_occurrence |>
  dplyr::select(dplyr::all_of(vars_of_interest)) |>
  dplyr::mutate(class = factor(class, levels = c("presence", "absence"))) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    sst = as.double(sst),
    #hmlmeso = as.double(hmlmeso),
    lmeso = as.double(lmeso),
    mlmeso = as.double(mlmeso),
    depth = as.double(depth),
    slope = as.double(slope))

bkw_recipe <- recipes::recipe(x = mb_ha_occurrence, class ~ .)
mb_ha_occurrence |> tidysdm::check_sdm_presence(class)

bkw_models <-
  # create the workflow_set
  workflowsets::workflow_set(
    preproc = list(default = bkw_recipe),
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

# TO BE REMOVED: mb_ha_occurrence_UTM <- sf::st_transform(mb_ha_occurrence, 32626)
bkw_cv <- spatialsample::spatial_block_cv(mb_ha_occurrence_UTM, v = 5)
autoplot(bkw_cv)

bkw_models02 <-
  bkw_models %>%
  workflowsets::workflow_map(
    "tune_grid",
    resamples = bkw_cv,
    grid = 3,
    metrics = tidysdm::sdm_metric_set(),
    verbose = TRUE
  )

autoplot(bkw_models02)

bkw_ensemble <- simple_ensemble() %>%
  add_member(bkw_models02, metric = "roc_auc") # TO BE ASSESSED.

readr::write_rds(x = bkw_ensemble, file = "data/bkw_ensemble_rps.rds", compress = "xz")

autoplot(bkw_ensemble)

bkw_ensemble |> collect_metrics()
bkw_ensemble2 <- explain_tidysdm(bkw_ensemble, by_workflow = TRUE)

