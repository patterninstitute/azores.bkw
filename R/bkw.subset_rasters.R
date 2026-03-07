#' Compute monthly mean rasters across all years
#'
#' @description
#' [mb_ha.subset_rasters_monthly_mean_all_years()] extracts all layers
#' corresponding to a given month across multiple years for each dynamic
#' predictor and computes the mean raster for that month.
#'
#' Static predictors (e.g., depth and slope) are appended unchanged.
#'
#' This function is intended for generating climatological monthly means
#' across years for habitat suitability modelling.
#'
#' @param month A character string indicating the target month in `"MM"`
#' format (e.g., `"01"` for January, `"12"` for December).
#' @param sst_rast A `SpatRaster` containing sea surface temperature layers
#' named in the format `sst_YYYYMM`.
#' @param hmlmeso_rast A `SpatRaster` containing high mesozooplankton biomass
#' layers named in the format `hmlmeso_YYYYMM`.
#' @param lmeso_rast A `SpatRaster` containing low mesozooplankton biomass
#' layers named in the format `lmeso_YYYYMM`.
#' @param mlmeso_rast A `SpatRaster` containing medium–large mesozooplankton
#' biomass layers named in the format `mlmeso_YYYYMM`.
#' @param depth_rast A static (non time-dependent) `SpatRaster` representing depth.
#' @param slope_rast A static (non time-dependent) `SpatRaster` representing slope.
#'
#' @returns A `SpatRaster` composed of:
#'
#'  - Monthly mean `sst`
#'  - Monthly mean `hmlmeso`
#'  - Monthly mean `lmeso`
#'  - Monthly mean `mlmeso`
#'  - `depth` (static layer)
#'  - `slope` (static layer)
#'
#' @details
#' The function internally identifies layers corresponding to the selected
#' month using regular expressions that match the pattern `prefix_YYYYMM`.
#' All matching layers across years are averaged using [terra::mean()].
#'
#' Layer naming must follow the convention `prefix_YYYYMM` for correct
#' matching. If no layers match the requested month, the function will return
#' an error.
#'
#' @export
mb_ha.subset_rasters_monthly_mean_all_years <- function(month,
                                                      sst_rast,
                                                      hmlmeso_rast,
                                                      lmeso_rast,
                                                      mlmeso_rast,
                                                      depth_rast,
                                                      slope_rast) {
  # Helper function to extract layers for a specific month across all years
  extract_month_all_years <- function(raster, var_prefix, month) {
    layer_indices <- grep(paste0(var_prefix, "_\\d{4}", month), names(raster))
    terra::mean(raster[[layer_indices]])
  }

  c(
    extract_month_all_years(sst_rast, "sst", month),
    extract_month_all_years(hmlmeso_rast, "hmlmeso", month),
    extract_month_all_years(lmeso_rast, "lmeso", month),
    extract_month_all_years(mlmeso_rast, "mlmeso", month),
    depth_rast,
    slope_rast
  )
}

#' Compute seasonal mean rasters across all years
#'
#' @description
#' [mb_ha.subset_rasters_seasonal_mean_all_years()] extracts all layers
#' corresponding to a given season across multiple years for each dynamic
#' predictor and computes the mean raster for that season.
#'
#' Static predictors (e.g., depth and slope) are appended unchanged.
#'
#' Seasons are defined as:
#'  - `"JFM"`: January–March
#'  - `"AMJ"`: April–June
#'  - `"JAS"`: July–September
#'  - `"OND"`: October–December
#'
#' This function is intended for generating climatological seasonal means
#' across years for habitat suitability modelling.
#'
#' @param season A character string indicating the target season.
#' Must be one of `"JFM"`, `"AMJ"`, `"JAS"`, or `"OND"`.
#' @param sst_rast A `SpatRaster` containing sea surface temperature layers
#' named in the format `sst_YYYYMM`.
#' @param hmlmeso_rast A `SpatRaster` containing high mesozooplankton biomass
#' layers named in the format `hmlmeso_YYYYMM`.
#' @param lmeso_rast A `SpatRaster` containing low mesozooplankton biomass
#' layers named in the format `lmeso_YYYYMM`.
#' @param mlmeso_rast A `SpatRaster` containing medium–large mesozooplankton
#' biomass layers named in the format `mlmeso_YYYYMM`.
#' @param depth_rast A static (non time-dependent) `SpatRaster` representing depth.
#' @param slope_rast A static (non time-dependent) `SpatRaster` representing slope.
#'
#' @returns A `SpatRaster` composed of:
#'
#'  - Seasonal mean `sst`
#'  - Seasonal mean `hmlmeso`
#'  - Seasonal mean `lmeso`
#'  - Seasonal mean `mlmeso`
#'  - `depth` (static layer)
#'  - `slope` (static layer)
#'
#' @details
#' The function internally identifies layers corresponding to the selected
#' season using regular expressions that match the pattern `prefix_YYYYMM`.
#' All matching layers across years and across the three months defining
#' the season are averaged using [terra::mean()].
#'
#' Layer naming must follow the convention `prefix_YYYYMM` for correct
#' matching. An error is returned if an invalid season is supplied.
#'
#' @export
mb_ha.subset_rasters_seasonal_mean_all_years <- function(season,
                                                       sst_rast,
                                                       hmlmeso_rast,
                                                       lmeso_rast,
                                                       mlmeso_rast,
                                                       depth_rast,
                                                       slope_rast) {
  # Helper function to extract layers for specific months across all years
  extract_season_all_years <- function(raster, var_prefix, season_months) {
    month_regex <- paste0(season_months, collapse = "|")
    layer_indices <- grep(paste0(var_prefix, "_\\d{4}(", month_regex, ")"), names(raster))
    terra::mean(raster[[layer_indices]])
  }

  seasons <- list(
    "JFM" = c("01", "02", "03"),
    "AMJ" = c("04", "05", "06"),
    "JAS" = c("07", "08", "09"),
    "OND" = c("10", "11", "12")
  )

  if (!season %in% names(seasons)) {
    stop("Invalid season. Use one of: 'JFM', 'AMJ', 'JAS', 'OND'")
  }

  season_months <- seasons[[season]]

  c(
    extract_season_all_years(sst_rast, "sst", season_months),
    extract_season_all_years(hmlmeso_rast, "hmlmeso", season_months),
    extract_season_all_years(lmeso_rast, "lmeso", season_months),
    extract_season_all_years(mlmeso_rast, "mlmeso", season_months),
    depth_rast,
    slope_rast
  )
}
