library(tidyverse)
library(tidysdm)
library(sf)
library(terra)
library(tidyterra)
library(ggbeeswarm)
library(DALEX)
library(units)
library(future)
library(azores.fkw)
library(patchwork)

future::plan(multisession, workers = 10)

setwd("/Users/ruisantos/Desktop/Tidysdm/breeding/")

bkw_occurrence <- sf::st_read("bkw_occurrence_rps.shp")

#
# Keep absences only for deep divers that not bkw.
#

bkw_occurrence <- bkw_occurrence %>%
  dplyr::filter(
    (class == "presence" & species %in% c("Mesoplodon bidens", "Hyperoodon ampullatus")) |
      (class == "absence" & !species %in% c("Mesoplodon mirus",
                                            "Mesoplodon densirostris",
                                            "Mesoplodon europaeus",
                                            "Ziphius cavirostris"))
  )

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


bkw.occurrence <-
  bkw_occurrence |>
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
bkw.occurrence
saveRDS(bkw.occurrence, file = "bkw.occurrence.rds")

bkw_recipe <- recipes::recipe(x = bkw.occurrence, class ~ .)
bkw.occurrence |> tidysdm::check_sdm_presence(class)

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

bkw.occurrence_UTM <- sf::st_transform(bkw.occurrence, 32626)
bkw_cv <- spatialsample::spatial_block_cv(bkw.occurrences_UTM, v = 5)
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
  add_member(bkw_models02, metric = "roc_auc")

readr::write_rds(x = bkw_ensemble, file = "bkw_ensemble_rps.rds", compress = "xz")

autoplot(bkw_ensemble)

bkw_ensemble |> collect_metrics()
bkw_ensemble2 <- explain_tidysdm(bkw_ensemble, by_workflow = TRUE)

