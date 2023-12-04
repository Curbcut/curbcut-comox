## BUILD AND APPEND ALLEY DATA #################################################

zoning <- function(scales_variables_modules, username, access_token) {
  
  # Filter zoning -----------------------------------------------------------
  
  # z <- sf::st_read("dev/data/zoning/LG_ZONING.shp")
  # z <- sf::st_transform(z, 4326)
  # z <- sf::st_make_valid(z)
  # z <- z[sf::st_is_valid(z), ]
  # z <- sf::st_filter(z, base_polygons$master_polygon)
  # z <- tibble::as_tibble(z)
  # z <- sf::st_as_sf(z)
  # remove_features <- c(568111, 954133, 954164, 954168, 954140, 953470, 580226,
  #                      269201, 269194, 269193, 269413, 269497, 568031, 567951,
  #                      565755, 567471, 567551, 567631, 567711, 567791, 567871)
  # z <- z[!z$OBJECTID %in% remove_features, ]
  # z <- z[c("OBJECTID", "PROVIDER", "ZONE_CODE", "ICI_ZONE", "ZONE_DESC", "BYLAW_URL")]
  # names(z) <- c("ID", "provider", "zone_code", "zoning", "zone_desc", "url", "geometry")
  # z$ID <- as.character(z$ID)
  # 
  # # Rename zoning, remove marginal
  # z$zoning[z$zoning %in% c("UNKNOWN", "OTHER", "NOT SPECIFIED")] <- "OTHER"
  # z <- z[!z$zoning %in% "TRANSPORTATION", ]
  # 
  # # Add location
  # z$address <- progressr::with_progress({
  #   z_cent <- sf::st_centroid(z)
  #   pb <- progressr::progressor(nrow(z_cent))
  #   future.apply::future_sapply(z_cent$geometry, \(x) {
  #     pb()
  #     cc.data::rev_geocode_localhost(x)
  #     })
  # })
  # qs::qsave(z, "dev/data/zoning/zoning.qs")
  z <- qs::qread("dev/data/zoning/zoning.qs")
  
  zoning_df <- z
  
  
  
  # # Upload to mapbox --------------------------------------------------------
  # 
  # tile_id <- "com_zoning"
  # # Reset
  # cc.buildr::tileset_delete_tileset_source(
  #   tile_id, username = username, access_token = access_token
  # )
  # cc.buildr::tileset_delete_tileset(
  #   tile_id, username = username, access_token = access_token
  # )
  # tryCatch(cc.buildr::tileset_upload_tile_source(
  #   df = zoning_df,
  #   id = tile_id,
  #   username = username,
  #   access_token = access_token
  # ), error = function(e) {
  #   cc.buildr::tileset_upload_tile_source_large(
  #     df = zoning_df,
  #     id = tile_id,
  #     username = username,
  #     access_token = access_token
  #   )
  # })
  # # Create the recipe
  # recipe <- cc.buildr::tileset_create_recipe(
  #   layer_names = tile_id,
  #   source = paste0("mapbox://tileset-source/", username, "/", tile_id),
  #   minzoom = 0,
  #   maxzoom = 16,
  #   recipe_name = tile_id
  # )
  # # Publish tileset
  # cc.buildr::tileset_create_tileset(
  #   tileset = tile_id,
  #   recipe = recipe,
  #   username = username,
  #   access_token = access_token
  # )
  # cc.buildr::tileset_publish_tileset(
  #   tileset = tile_id,
  #   username = username,
  #   access_token = access_token
  # )
  # 
  # 
  
  # Text and graph ----------------------------------------------------------
  
  zoning_df$area <- cc.buildr::get_area(zoning_df)
  zoning_df <- sf::st_drop_geometry(zoning_df)
  zoning_df$text <- 
    sprintf("<p><b>%s</b></p><p>Zone %s is classified as '%s' and covers an area of %s square meters.",
            zoning_df$address, zoning_df$ID, tolower(zoning_df$zoning),
            prettyNum(round(zoning_df$area, digits = 2), big.mark = ","))
  
  zoning_df$text <- sapply(seq_along(zoning_df$text), \(n) {
    sprintf("%s The zone description is <b>'%s'</b>.", 
            zoning_df$text[n], 
            zoning_df$zone_desc[n])
  })
  
  # Add further residential information. Town of Comox and Courtenay have difference
  # way of categorizing their residential use in the DB (Comox as sentence, Courtenay
  # all caps.)
  zoning_df$text <- sapply(seq_along(zoning_df$text), \(n) {
    if (zoning_df$zoning[n] != "RESIDENTIAL") return(zoning_df$text[n])
    
    df <- zoning_df[n, ]
    city <- if (df$provider == "COURTENAY") "City of Courtenay" else "Town of Comox"
    bylaw_url <- if (df$provider == "COURTENAY") {
      paste0("https://www.courtenay.ca/assets/Departments/Development~Services",
             "/OCP~Update/OCP-DPAs-Zoning~July~2022/Zoning%20Bylaw%202500,%202",
             "007%20-May2023.pdf")
    } else {
      paste0("https://www.comox.ca/sites/default/files/2023-05/Consolidated%",
             "20Bylaw%201850_Zoning_at%201Mar2023.pdf")
    }
    # Usage
    usage <- (\(x) {
      if (df$zone_desc == "RESIDENTIAL ONE") return(" is single residential")
      if (grepl("RESIDENTIAL ONE (A|B|C|D)", df$zone_desc)) return(" is single residential")
      if (grepl("RESIDENTIAL ONE (E|S)", df$zone_desc)) return("s are single residential, carriage house and secondary residential")
      if (grepl("RESIDENTIAL TWO", df$zone_desc)) return("s are duplex, carriage house and secondary residential")
      if (grepl("RESIDENTIAL THREE", df$zone_desc)) return(" is low density multi residential")
      if (grepl("RESIDENTIAL FOUR", df$zone_desc)) return(" is medium and high density multi residential")
      if (grepl("RESIDENTIAL FIVE", df$zone_desc)) return(" is low density multi residential")
      if (grepl("RURAL RESIDENTIAL", df$zone_desc)) return(" is rural single residential")
      if (grepl("MOBILE HOME", df$zone_desc)) return(" is mobile home residential")
      " is unknown"
    })()
    
    out <- sprintf(paste0("<p>The permitted use%s. To know more, visit the %s's ",
                   "<a href='%s' target = '_blank'>zoning bylaw</a>."),
            usage, city, bylaw_url)
    
    paste0(df$text, out)
  })
  
  zones_t <- unique(zoning_df$zoning)
  zones <- lapply(zones_t, \(type) {
    size_km2 <- sum(zoning_df$area[zoning_df$zoning == type])
    c(prettyNum(round(size_km2  / 1e6, digits = 2), big.mark = ","),
    scales::percent(size_km2 / sum(zoning_df$area), accuracy = 0.1))
  })
  zones <- tibble::tibble(zoning = zones_t, area_km2 = sapply(zones, `[[`, 1),
                          area_pct = sapply(zones, `[[`, 2))
  
  qs::qsavem(zoning_df, zones, file = "data/zoning.qsm")
  
  # Modules table -----------------------------------------------------------

  modules <-
    scales_variables_modules$modules |>
    add_module(
      id = "zoning",
      theme = "Land use",
      nav_title = "Zoning",
      title_text_title = "Zoning",
      title_text_main = paste0(""),
      title_text_extra = paste0(""),
      metadata = FALSE,
      dataset_info = ""
    )
  
  
  # Return ------------------------------------------------------------------
  
  return(list(
    scales = scales_variables_modules$scales,
    variables = scales_variables_modules$variables,
    modules = modules,
    data = scales_variables_modules$data
  ))
  
}
