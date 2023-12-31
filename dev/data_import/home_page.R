## HOME PAGE TIBBLES ###########################################################

home_page <- function(modules, stories = NULL, data_path = "data/") {
  ### READ ?cc.landing::landing_input DOCUMENTATION TO CONSTRUCT CORRECTLY
  # EACH OF THESE OBJECTS.
  
  # Encode images to base64 for the input
  base64 <- function(x) {
    # Read the JPG image as raw binary data
    image_data <- readBin(x, "raw", file.info(x)$size)
    
    # Encode the image data to base64
    paste0("data:image/jpeg;base64,", base64enc::base64encode(image_data))
  }
  
  
  # Path to the top-left corner SVG image of  -------------------------------
  c_city_svg <- "www/landing/c-comox.svg"
  
  
  # Tibble for the news section ---------------------------------------------
  
  news_cards <- tibble::tibble(id = character(),
                               icon = character(),
                               title_en = character(),
                               title_fr = character(),
                               text_en = character(),
                               text_fr = character(),
                               link = character())
  
  news_cards <- 
    news_cards |> 
    tibble::add_row(id = "greenness", 
                    icon = "ecology", 
                    title_en = "Explore urban greenery", 
                    title_fr = "Explorez la verdure urbaine", 
                    text_en = paste0(
                      "How green is your neighbourhood? Curbcut’s new Vegetation page uses the",
                      " Normalized Difference Vegetation Index (NDVI) to explore plant health",
                      " and density in urban areas. NDVI plays a significant role in various ",
                      "applications, including analyzing urban greenness, monitoring agricult",
                      "ural growth, and assessing wildfire risks."
                    ),
                    link = "ndvi") |> 
    tibble::add_row(id = "lst", 
                    icon = "climat", 
                    title_en = "Land surface temperature", 
                    title_fr = "Température au sol", 
                    text_en = paste0(
                      "Curbcut now has land surface temperature (LST) analytics! LST measures",
                      " the maximum mean warm-season temperature at a specific location, and ",
                      "it is a crucial indicator of urban heat islands and ecological balance",
                      " within a region."
                    ),
                    link = "lst") |> 
    tibble::add_row(id = "alp", 
                    icon = "health", 
                    title_en = "Active living potential", 
                    title_fr = "Potentiel de vie active", 
                    text_en = paste0(
                      "Curbcut has developed its own Active Living Potential index. This inde",
                      "x quantifies which areas provide walkable environments to their reside",
                      "nts based on street connectivity, building density and points of inter",
                      "est. We developed the index by building a travel time matrix for the e",
                      "ntire country, using a 15-minute walk buffer on the street network."
                    ),
                    link = "alp")
  
  
  # Tibble for the discover section -----------------------------------------
  
  # Function to remove all HTML tags from a given string
  remove_html_tags <- function(input_vector) {
    # Use gsub to replace all HTML tags with an empty string
    output_vector <- gsub("<[^>]*>", "", input_vector)
    return(output_vector)
  }
  
  # Pages from the modules
  disc_modules <- modules[c("id", "title_text_title", "title_text_main", "theme")]
  names(disc_modules) <- c("id", "en", "preview_en", "theme")
  disc_modules$img <- sprintf("%s.png", disc_modules$id)
  disc_modules$theme <- gsub(" .*", "", disc_modules$theme) |> tolower()
  disc_modules$preview_en <- remove_html_tags(disc_modules$preview_en)
  disc_modules <- disc_modules[c("id", "img", "theme", "en", "preview_en")]
  disc_modules$type <- "page"
  disc_modules$select_id <- NA
  disc_modules$var_left <- NA
  disc_modules$var_right <- NA
  disc_modules$page <- NA
  disc_modules$date <- NA
  disc_modules$df <- NA
  
  # Stories formatting for the discover_cards
  if (!is.null(stories)) {
    disc_stories <- stories[c("name_id", "short_title", "preview_en", "preview_fr", "ID")]
    names(disc_stories) <- c("id", "en", "preview_en", "preview_fr", "select_id")
    disc_stories$img <- sprintf("%s.png", disc_stories$id)
    disc_stories$theme <- "urban"
    disc_stories$fr <- sapply(disc_stories$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
    disc_stories$type <- "stories"
    disc_stories$var_left <- NA
    disc_stories$var_right <- NA
    disc_stories$page <- NA
    disc_stories$date <- NA
    disc_stories$df <- NA
    # disc_stories <- disc_stories[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type", "select_id")]
    
  }
  
  # DYK for discovers
  dyk_discover_fun <- function(theme, page, var_right = " ", var_left = NULL, date, type, en) {
    
    # Filter page, var_right, type
    this <- dyk[dyk$module == page & dyk$var_right == var_right & dyk$dyk_type == type, ]
    
    # Filter var_left if supplied
    if (!is.null(var_left)) {
      this <- dyk[dyk$var_left == var_left, ]
    }
    
    # Filter date over the list column
    this <- this[unlist(sapply(this$date, identical, as.character(date))), ]
    
    # Filter region and scale. Use of `identical` for when scale is NA.
    this <- this[this$region == this$region[1] & unlist(sapply(this$scale, identical, this$scale[1])), ]
    
    # Grab last row
    this <- this[nrow(this), ]
    
    # Discover columns
    this$id <- sprintf("%s_dyk1", page)
    this$img <- sprintf("%s.png", this$id)
    this$theme <- theme
    this$en <- en
    names(this)[names(this) == "dyk_text_en"] <- "preview_en"
    names(this)[names(this) == "module"] <- "page"
    names(this)[names(this) == "select_ID"] <- "select_id"
    this$type <- "dyk"
    this$df <- if (type %in% c("highest", "lowest")) sprintf("%s_%s", this$region, this$scale) else NA
    
    # Return
    this[c("id", "img", "theme", "en", "preview_en", "type", 
           "page", "var_left", "var_right", "select_id", "date", "df")]
  }
  
  disc_dyk <- 
    tibble::tibble() |> 
    rbind(dyk_discover_fun(theme = "health", 
                           page = "alp", 
                           var_right = "housing_single_detached",
                           date = 2021,
                           type = "compare", 
                           en = "Dense, walkable neighbourhoods")) |> 
    rbind(dyk_discover_fun(theme = "climate", 
                           page = "lst", 
                           var_right = " ",
                           date = 2021,
                           type = "lowest", 
                           en = "The coolest town in the region")) |> 
    rbind(dyk_discover_fun(theme = "ecology", 
                           page = "ndvi", 
                           var_right = "housing_tenant",
                           date = 2023,
                           type = "compare", 
                           en = "Tenants lack green space")) |> 
    rbind(dyk_discover_fun(theme = "housing", 
                           page = "housing", 
                           var_left = "housing_rent",
                           date = c("1996", "2021"),
                           type = "change", 
                           en = "Skyrocketing housing costs")) |> 
    rbind(dyk_discover_fun(theme = "transport", 
                           page = "canbics", 
                           var_right = " ",
                           date = c("2021"),
                           type = "highest", 
                           en = "The best bikelanes"))
  
  
  # Bindthe modules with the stories and the DYK
  discover_cards <- rbind(disc_modules, if (exists("disc_stories")) disc_stories, disc_dyk)
  
  # Filter out missing photos and warn!
  present_img <- discover_cards$img %in% list.files("www/landing/discover/")
  missing_img <- discover_cards[!present_img, ]
  if (nrow(missing_img) > 0){
    warning(paste0("Missing images for ", missing_img$id, "\n"))
  }
  
  discover_cards <- discover_cards[present_img, ]
  discover_cards$img <- paste0("www/landing/discover/", discover_cards$img)
  
  if (length(unique(discover_cards$id)) != nrow(discover_cards)) {
    stop("Discover cards do not have unique ids")
  }
  
  discover_cards$img <- sapply(discover_cards$img, base64)
  
  # Tibble for team members -------------------------------------------------
  team_cards <- tibble::tibble(
    id = c("davidw", "kevinm", "maxbdb", "dominiqueb"),
    img = c(
      "www/landing/team/david_wachsmuth.jpeg",
      "www/landing/team/kevin_manaugh.jpg",
      "www/landing/team/maxime_belanger_de_blois.jpg",
      "www/landing/team/dominique_boulet.jpg"
    ),
    name = c("David Wachsmuth", "Kevin Manaugh", "Maxime Bélanger De Blois", "Dominique Boulet"),
    role_en = c("Co-founder & Co-CEO", "Co-founder & Co-CEO", "Head of Technology and Data", "Qualitative Research Lead"),
    role_fr = c("Co-fondateur et co-PDG", "Co-fondateur et co-PDG", "Responsable technologie et données", "Responsable de la recherche qualitative"),
    # bio_en = c("David is one of the world’s leading experts on the impacts of short-term rental platforms, such as Airbnb, on cities around the world and consults widely with municipalities and community organizations on designing appropriate regulations. In addition to his work at Curbcut, David is the Canada Research Chair in Urban Governance at McGill University, where he is also an Associate Professor in the School of Urban Planning.",
    #            "Kevin is one of the leading experts on the intersection between urban transport systems and social and environmental justice. In addition to his work at Curbcut, Kevin is also an associate professor at McGill University jointly appointed in the Department of Geography and the Bieler School of Environment.",
    #            "Maxime is a skilled, resourceful and forward-thinking data scientist, adept at developing and transforming intricate datasets into actionable intelligence. With a master's degree in Urban Planning from McGill University, his extensive understanding of data analysis and geovisualization enables him to extract valuable insights and provide innovative solutions.",
    #            "Dominique is driven to create qualitative work that complements quantitative information. She has a master’s degree in Urban Planning from McGill University and a master’s degree in Anthropology from Aarhus University, Copenhagen."),
    # bio_fr = c("David est l'un des plus grands experts mondiaux sur les impacts des plateformes de location à court terme, telles que Airbnb, sur les villes du monde entier et consulte largement les municipalités et les organisations communautaires sur la conception de réglementations appropriées. En plus de son travail chez Curbcut, David est titulaire de la Chaire de recherche du Canada en gouvernance urbaine à l'Université McGill, où il est également professeur associé à l'École d'urbanisme.",
    #            "Kevin est l'un des principaux experts de l'intersection entre les systèmes de transport urbain et la justice sociale et environnementale. En plus de son travail chez Curbcut, Kevin est également professeur associé à l'Université McGill, nommé conjointement au département de géographie et à l'école d'environnement Bieler.",
    #            "Maxime est un scientifique de données habile, ingénieux et avant-gardiste, capable de concevoir et de transformer des ensembles de données complexes en renseignements exploitables. Diplômé d'une maîtrise en urbanisme de l'Université McGill, sa connaissance approfondie de l'analyse des données et de la géovisualisation lui permet d'extraire des informations précieuses et de proposer des solutions innovantes.",
    #            "Dominique est motivée par la nécessité de produire des études qualitatives qui complètent les informations quantitatives. Elle est titulaire d'une maîtrise en urbanisme de l'Université McGill et d'une maîtrise en anthropologie de l'Université d'Aarhus, à Copenhague."),
    theme = c("housing", "transport", "health", "urban")
  )
  
  team_cards$img <- sapply(team_cards$img, base64)
  
  # Character vector for contributors ---------------------------------------
  contributors <- c(" ")
  
  
  # Tibble for collaborators ------------------------------------------------
  collabs <- tibble::tibble(
    id = c("MSSI"),
    img = c(
      "www/landing/collab/mcgill-logo.png"
    ),
    name = c("The McGill Sustainability Systems Initiative")
  )
  
  collabs$img <- sapply(collabs$img, base64)
  
  
  # Save home page information as qsm ---------------------------------------
  if (!exists("data_path")) data_path <- "data/"
  
  qs::qsavem(c_city_svg, news_cards, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )
}