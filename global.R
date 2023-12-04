#### CC GLOBALS ################################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(curbcut)
  
  library(DBI)
  library(RSQLite)
  
  library(cc.landing)
  library(cc.map)
})


# Data --------------------------------------------------------------------

curbcut::load_data(site_name = "Curbcut Comox",
                   site_url = "https://comox.curbcut.ca",
                   stories_page = "Comox stories",
                   tileset_prefix = "com",
                   mapbox_username = "curbcut",
                   default_random_address = "207 4th Street, Courtenay, BC",
                   map_zoom = 10.5,
                   map_loc = c(lat = -124.98, lon = 49.68))


# Shiny options -----------------------------------------------------------

# options(shiny.fullstacktrace = TRUE)
# options(shiny.useragg = TRUE)
# shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "cache")))


# Declare temporary folder ------------------------------------------------

temp_folder <- tempdir()
addResourcePath("temp_folder_shortcut", temp_folder)


# Create the UI and server functions for basic modules --------------------

curbcut::create_ui_server_mods(modules = modules)


# Set up fonts ------------------------------------------------------------

systemfonts::register_font(
  name = "acidgrotesk-book",
  plain = list("www/fonts/acidgrotesk-book.woff", 0)
)


# Source the R folder -----------------------------------------------------

# Curbcut works with global environment. Must source to the current global env
lapply(list.files("R", full.names = TRUE), source, verbose = FALSE)
