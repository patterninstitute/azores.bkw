library(sf)
library(terra)
library(dplyr)

library(azores.fkw)
library(azores.bathymetry)

#
# Reading raw data for environmental variable rasters.
#

nc_data_path <- "data-raw/cms-azores-data"
sst_raw_rast <- terra::rast(file.path(nc_data_path, "METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1741967803421.nc"))
hmlmeso_raw_rast <- terra::rast(file.path(nc_data_path, "hmlmeso.2012.2018.nc"))
lmeso_raw_rast <- terra::rast(file.path(nc_data_path, "lmeso.2012.2018.nc"))
mlmeso_raw_rast <- terra::rast(file.path(nc_data_path, "mlmeso.2012.2018.nc"))
depth_raw_rast <- azores.bathymetry::bathymetry("depth", resolution = 500)
slope_raw_rast <- azores.bathymetry::bathymetry("depth_slope", resolution = 500)

#
# Rasters with NAs imputed by average of neighboring points in a 3 x 3 window.
#
sst_raw_rast2 <- terra::focal(sst_raw_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
hmlmeso_raw_rast2 <- terra::focal(hmlmeso_raw_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
lmeso_raw_rast2 <- terra::focal(lmeso_raw_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)
mlmeso_raw_rast2 <- terra::focal(mlmeso_raw_rast, w = 3, fun = mean, na.policy = "only", na.rm = TRUE)

# Give the same CRS to both depth rast and slope rasters as the ref_rast
depth_raw_rast2 <-  terra::project(
  depth_raw_rast,
  terra::crs(ref_rast)  # continuous variable → use bilinear
)

slope_raw_rast2 <- terra::project(
  slope_raw_rast,
  terra::crs(ref_rast))

#
# Conversion from Kelvin to Celsius.
#
sst_rast <- sst_raw_rast2 - 273.15

#
# Reference raster: the raster whose extent and resolution is used for
# re-rasterizing the other rasters.
#
ref_rast <- sst_rast

#
# Re-rasterization of other rasters.
#
hmlmeso_rast <- terra::resample(hmlmeso_raw_rast2, ref_rast, method = "bilinear")
lmeso_rast <- terra::resample(lmeso_raw_rast2, ref_rast, method = "bilinear")
mlmeso_rast <- terra::resample(mlmeso_raw_rast2, ref_rast, method = "bilinear")
depth_rast <- terra::resample(depth_raw_rast2, ref_rast, method = "bilinear")
slope_rast <- terra::resample(slope_raw_rast2, ref_rast, method = "bilinear")



#Update variable names
hmlmeso_rast <- update_raster_names(hmlmeso_rast, "hmlmeso", "HMLMESO", "highly migrant lower mesopelagic")
lmeso_rast <- update_raster_names(lmeso_rast, "lmeso", "LMESO", "lower mesopelagic micronekton")
mlmeso_rast <- update_raster_names(mlmeso_rast, "mlmeso", "MLMESO", "migrant lower mesopelagic micronekton")
sst_rast <- update_raster_names(sst_rast, "sst", "SST", "Sea Surface Temperature")


bkw_climate_2012_2018 <- c(
  sst_rast,
  hmlmeso_rast,
  lmeso_rast,
  mlmeso_rast,
  depth_rast,
  slope_rast)
# Remove units by setting them to an empty string
units(bkw_climate_2012_2018) <- ""


# Example usage for April-June (AMJ)
bkw.climate_2012_2018_spring <- bkw.subset_rasters_seasonal_mean_all_years(
  season = "AMJ",
  sst_rast = sst_rast,
  hmlmeso_rast = hmlmeso_rast,
  lmeso_rast = lmeso_rast,
  mlmeso_rast = mlmeso_rast,
  depth_rast = depth_rast,
  slope_rast = slope_rast
)
names(bkw.climate_2012_2018_spring) <- c("sst", "hmlmeso", "lmeso" ,"mlmeso", "depth", "slope")

# Example for July-September (JAS)
bkw.climate_2012_2018_summer <- bkw.subset_rasters_seasonal_mean_all_years(
  season = "JAS",
  sst_rast = sst_rast,
  hmlmeso_rast = hmlmeso_rast,
  lmeso_rast = lmeso_rast,
  mlmeso_rast = mlmeso_rast,
  depth_rast = depth_rast,
  slope_rast = slope_rast
)
names(bkw.climate_2012_2018_summer) <- c("sst", "hmlmeso", "lmeso" ,"mlmeso", "depth", "slope")

# Example for October-December (OND)
bkw.climate_2012_2018_autumn <- bkw.subset_rasters_seasonal_mean_all_years(
  season = "OND",
  sst_rast = sst_rast,
  hmlmeso_rast = hmlmeso_rast,
  lmeso_rast = lmeso_rast,
  mlmeso_rast = mlmeso_rast,
  depth_rast = depth_rast,
  slope_rast = slope_rast
)
names(bkw.climate_2012_2018_autumn) <- c("sst", "hmlmeso", "lmeso" ,"mlmeso", "depth", "slope")


terra::writeRaster(x = bkw.climate_2012_2018_spring, filename = "data-raw/cms-azores-data/bkw.climate_2012_2018_spring.tif", overwrite = TRUE)
terra::writeRaster(x = bkw.climate_2012_2018_summer, filename = "data-raw/cms-azores-data/bkw.climate_2012_2018_summer.tif", overwrite = TRUE)
terra::writeRaster(x = bkw.climate_2012_2018_autumn, filename = "data-raw/cms-azores-data/bkw.climate_2012_2018_autumn.tif", overwrite = TRUE)
