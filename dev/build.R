#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


# Base of the study region and dictionaries -------------------------------

# Possible sequences for autozooms. Every module must have one or multiple of these
# possible scale sequences.
scales_sequences <- list(c("CSD", "DA", "DB", "building"),
                         c("CSD", "DA", "building"))

# List all the regions geometries to create the master polygon
all_regions <- list(CD = list(CD = 5926))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_regions = all_regions,
    region = c("CD"),
    name = c(CD = "Census Division Area"),
    to_compare = c(CD = "in the Comox Valley region"),
    to_compare_determ = c(CD = "the Comox Valley region"),
    to_compare_short = c(CD = "in the region"),
    pickable = c(CMA = TRUE))


# Build scales ------------------------------------------------------------

### Build census scales
census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      levels = c("CSD", "DA", "DB"),
                      fill_CTs_with_CSDs = FALSE,
                      crs = crs)

# Create the census scales dictionary
scales_dictionary <- census_scales_dictionary(census_scales)


### Build building scale
# # From MySQL
# building <- cc.data::db_read_long_table(table = "buildings",
#                                          DA_ID = census_scales$DA$ID)
# qs::qsave(building, file = "dev/data/built/building.qs")
# # From Local
# building <- qs::qread("dev/data/canada_buildings.qs")
# building <- building[building$DA_ID %in% census_scales$DA$ID, ]
# building <- qs::qsave(building, "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "building",
                             sing_with_article = "the building",
                             plur = "buildings",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "{name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building))

scales_consolidated <- consolidate_scales(scales_sequences = scales_sequences,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs)

regions_dictionary <- regions_dictionary_add_scales(
  regions_dictionary = regions_dictionary,
  region_dict_scales = scales_consolidated$for_region_dict_scales)

scales_dictionary <- add_regions_to_scales_dictionary(
  scales_dictionary = scales_dictionary, regions = base_polygons$regions,
  scales_consolidated = scales_consolidated,
  DA_carto = base_polygons$DA_carto)

# Verify conformity -------------------------------------------------------

verify_dictionaries(scales = scales_consolidated$scales,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated$scales)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
scales_variables_modules$data <- lapply(scales_consolidated$scales, \(x) list())

qs::qsavem(census_scales, scales_variables_modules, crs,
           scales_dictionary, regions_dictionary, base_polygons, 
           scales_consolidated, all_scales, scales_sequences,
           file = "dev/data/built/empty_scales_variables_modules.qsm")
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")

# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)
# future::plan(list(future::tweak(future::multisession,
#                                 workers = 2),
#                   future::tweak(future::multisession,
#                                 workers = length(cc.data::census_years))))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE,
                 scales_sequences = scales_sequences,
                 scales_to_interpolate = {
                   names(scales_variables_modules$scales)[
                     !names(scales_variables_modules$scales) %in% c("building", "street", "DB")
                   ]
                 })
census_variables <- get_census_vectors_details()

future::plan(future::multisession(), workers = 6)

# scales_variables_modules <-
#   ru_vac_rate(scales_variables_modules = scales_variables_modules,
#               crs = crs, geo_uid = cancensus_CD_code,
#               approximate_name_match = FALSE)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID,
         scales_sequences = scales_sequences)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID,
             scales_sequences = scales_sequences)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         crs = crs,
         scales_sequences = scales_sequences)

scales_variables_modules <- 
  ba_ndvi(scales_variables_modules = scales_variables_modules, 
          master_polygon = base_polygons$master_polygon, 
          all_scales = all_scales, 
          data_output_path = "dev/data/ndvi/", 
          crs = crs,
          scales_sequences = scales_sequences)

save.image("dev/data/built/pre_tt.RData")
load("dev/data/built/pre_tt.RData")

# Access ------------------------------------------------------------------

# # Calculate the travel time matrice
# DB_centroid <- scales_variables_modules$scales$DB
# DB_centroid <- sf::st_centroid(DB_centroid)
# DB_centroid <- sf::st_transform(DB_centroid, 4326)
# DB_centroid <- DB_centroid[c("ID")]
# 
# future::plan(future::multisession(), workers = 8)
# traveltimes <- cc.data::tt_calculate_all_modes(
#   DA_table = DB_centroid, 
#   dest_folder = "dev/data/ttm",
#   osm_pbf = paste0("http://download.geofabrik.de/north-america/canada/british-",
#                    "columbia-latest.osm.pbf"))
# # Convert to minutes
# traveltimes <- lapply(traveltimes, \(tt) {
#   lapply(tt, \(t) {
#     t[[2]] <- t[[2]] / 60
#     t
#   })
# })
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

# Get the amenities
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_or_DB_IDs = census_scales$DB$ID,
                          traveltimes = traveltimes, 
                          themes = c("healthcare", "educational", "cultural"),
                          crs = crs,
                          scales_sequences = scales_sequences,
                          DA_DB = "DB",
                          default_var = "access_bicycle_educational_total")

# BC zoning
scales_variables_modules <-
  zoning(scales_variables_modules = scales_variables_modules, 
         username = "curbcut", 
         access_token = .cc_mb_token)

# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)


qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, base_polygons,
           all_scales, scales_sequences,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

# postal_codes <- build_postal_codes(census_scales$DA$ID)
# qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  scales_sequences = scales_sequences,
  zoom_levels = list(first = 0, DA = 10, DB = 11, building = 16))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# # Tilesets ----------------------------------------------------------------
# 
# tileset_upload_all(all_scales = scales_variables_modules$scales,
#                    map_zoom_levels = map_zoom_levels,
#                    prefix = "com",
#                    username = "curbcut",
#                    access_token = .cc_mb_token)


# Add possible regions to modules -----------------------------------------

scales_variables_modules <- pages_regions(svm = scales_variables_modules,
                                          regions_dictionary = regions_dictionary)


# Place explorer page ----------------------------------------------------

avail_scale_combinations <- sapply(scales_sequences, paste0, collapse = "_")

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the map to ",
               "see how it compares to the rest of the region across a variety of sust",
               "ainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             avail_scale_combinations = avail_scale_combinations,
             regions = regions_dictionary$region)


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()
qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# stories <- build_stories()
# qs::qsave(stories, file = "data/stories.qs")
# stories_create_tileset(stories = stories,
#                        prefix = "com",
#                        username = "curbcut",
#                        access_token = .cc_mb_token)
# cc.buildr::resize_image(folder = "www/stories/photos/", max_size_in_MB = 1)


# # Add Montréal stories
# scales_variables_modules$modules <-
#   scales_variables_modules$modules |>
#   add_module(
#     id = "stories",
#     theme = "Urban life",
#     nav_title = "Comox stories",
#     title_text_title = "Comox stories",
#     title_text_main = paste0(
#       "Explore narrative case studies about specific urban sustainability and ",
#       "planning issues in the Comox region."),
#     title_text_extra = paste0(
#       "<p>These narrative case studies are written by the Curbcut team and its contributors."),
#     metadata = FALSE,
#     dataset_info = ""
#   )

# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   svm = scales_variables_modules)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save large dfs as sqlite ------------------------------------------------

save_bslike_sqlite("building", all_scales = scales_variables_modules$scales)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")

# For compare, only keep the large brackets of age
scales_variables_modules$modules$var_right <- lapply(
  scales_variables_modules$modules$var_right, \(x) {
    if (is.null(x)) return(NULL)
    not_age <- x[!grepl("^age_", x)]
    age <- x[grepl("^age_", x)]
    
    age_keep <- age[age %in% c("age_0_14", "age_15_64", "age_65_plus")]
    
    c(not_age, age_keep)
  })

qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
tictoc::toc()

# Write data to AWS bucket
# cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.toronto.data")

# Create DYKs -------------------------------------------------------------

library(tidyverse)
vars_dyk <- dyk_prep(svm = scales_variables_modules, scales_dictionary = scales_dictionary)
variables <- scales_variables_modules$variables
dyk <- dyk_uni(vars_dyk,
               svm = scales_variables_modules,
               langs = c("en", "fr"),
               scales_dictionary = scales_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")

# Home page ---------------------------------------------------------------

home_page(modules = scales_variables_modules$modules, stories = NULL)


# Place explorer content creation -----------------------------------------

# Should be done once the data is saved

future::plan(future::multisession(), workers = 4)

# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# 
# # ONLY KEEP FIRST SCALE
# pe_main_card_data$main_card_data <- lapply(pe_main_card_data$main_card_data, lapply, `[`, 1)
# pe_main_card_data$avail_df <- dplyr::distinct(pe_main_card_data$avail_df, region, .keep_all = TRUE)
# 
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

svm_first_scale <- scales_variables_modules
svm_first_scale$scales <- lapply(svm_first_scale$scales, `[`, 1)

library(curbcut)
placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "com",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE)

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")

# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-toronto") # Production
# heroku_deploy("cc-toronto-2") # Dev
