### ZONING MODULE #########################################################
zoning_colour_pal <- tibble::tibble(group = c("COMMERCIAL", "RESIDENTIAL", "OTHER", "OPEN SPACE",
                                              "PUBLIC AND INSTITUTIONAL", "AGRICULTURAL / RURAL",
                                              "COMPREHENSIVE DEVELOPMENT", "INDUSTRIAL"),
                                    group_short = c("Commer.", "Resid.", "Other", "Open",
                                                    "Public", "Rural", "Compr.", "Industr."),
                                    fill = c("#A3B0D1", "#E08565", "#73AD80", "#A3B0D1", 
                                             "#F5D574", "#ADB033", 
                                             "#CD718C", "#9E9090"))
# Sorting in alphabetical order of 'group'
zoning_colour_pal <- zoning_colour_pal[order(zoning_colour_pal$group_short), ]


# Alley special functions
zoning_info_table <- function(select_id) {
  
  if (!is.na(select_id)) {
    return(zoning_df$text[zoning_df$ID == select_id])
  }
  
  # Introduction paragraph
  intro <- "<p>The zoning distribution across different areas is as follows:"
  
  # Creating bullet points for each zoning type
  bullet_points <- apply(zones, 1, function(row) {
    sprintf("<li>'<b>%s</b>' zones cover approximately %s square kilometers, or %s of the zoned areas.", 
            row["zoning"], row["area_km2"], row["area_pct"])
  })
  
  # Combine bullet points into a single text
  bullet_text <- paste(bullet_points, collapse = "\n")
  
  # Final message
  return(paste(intro, "<br><ul>", bullet_text, sep = ""))
  
}

# Grab when on borough summary
zoning_graph <- function(select_id) {
  
  theme_default <- list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
      legend.position = "none",
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  )
  
  zoning_for_gg <- zones
  zoning_for_gg$zoning_short <- sapply(zoning_for_gg$zoning, \(x) {
    zoning_colour_pal$group_short[zoning_colour_pal$group == x]
  }, USE.NAMES = FALSE)
  zoning_for_gg$area_km2 <- as.numeric(zoning_for_gg$area_km2)
  
  plot <-
    zoning_for_gg |>
    ggplot2::ggplot(ggplot2::aes(x = zoning_short, y = area_km2, fill = zoning_short)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(breaks = zoning_colour_pal$group_short,
                               values = zoning_colour_pal$fill) +
    ggplot2::labs(title = NULL,
                  x = NULL,
                  y = "Area (km2)") +
    theme_default +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
  
  if (!is.na(select_id)) {
    val <- zoning_df$zoning[zoning_df$ID == select_id]
    val <- zoning_colour_pal$group_short[zoning_colour_pal$group == val]
    if (!any(is.na(val))) {
      plot <-
        plot +
        ggplot2::geom_vline(
          xintercept = val,
          colour = "black", linewidth = 1.5
        )
    }
  }
  
  plot
}

# Fill when on observational data
scale_fill_zoning <- function(...) {
  cc.map::map_choropleth_fill_fun(
    df = zoning_colour_pal[c("group", "fill")],
    get_col = "zoning",
    fallback = "#B3B3BB")
}

zoning_legend <- function() {
  
  # Grab labels
  breaks <- zoning_colour_pal$group_short
  
  # Construct the dataframe
  df <- tibble::tibble(group = 1:8, y = 1,
                       fill = zoning_colour_pal$fill,
                       half = c(1,1,1,1,2,2,2,2))
  
  # Make the plot
  df |>
    ggplot2::ggplot(ggplot2::aes(
      xmin = group - 1, xmax = group, ymin = y - 1,
      ymax = y, fill = fill
    )) +
    ggplot2::geom_rect() +
    ggplot2::scale_x_continuous(
      breaks = df$group - 0.5,
      labels = breaks
    ) +
    ggplot2::scale_y_continuous(labels = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      df$fill, df$fill
    )) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "acidgrotesk-book", size = 12),
                   legend.position = "none",
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::facet_wrap(~half, scales = "free_x", ncol = 1, 
                        labeller = ggplot2::as_labeller(function(half) ""))
}


# Special zoning servers --------------------------------------------------

legend_zoning_server <- function(id, r, legend_fun) {
  shiny::moduleServer(id, function(input, output, session) {
    # Output legend
    output$legend_render <- shiny::renderUI({
      output$legend <- shiny::renderPlot(legend_fun()())
      # Weird hack to get legend plot to inherit full namespace
      shiny::plotOutput(session$ns("legend"),
                        height = 120,
                        width = "100%"
      )
    })
  })
}

explore_zoning_server <- function(id, r, select_id, table_fun, table_args,
                                  graph_fun, graph_args) {
  
  shiny::moduleServer(id, function(input, output, session) {
    # Make info table. If fails, returns NULL
    table_out <- shiny::reactive(
      tryCatch(
        do.call(table_fun(), table_args()),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    )
    
    # Display info table
    output$info_table <- shiny::renderUI(shiny::HTML(table_out()))
    
    # Make graph
    graph_out <- shiny::reactive(
      tryCatch(
        do.call(graph_fun(), graph_args()),
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    )
    
    # Display graph
    output$explore_graph <- shiny::renderPlot(graph_out())
    
    # # Show/hide components
    # shiny::observe({
    #   shinyjs::toggle("info_table", condition = !is.null(table_out()))
    #   # shinyjs::toggle("explore_graph", condition = !is.null(graph_out()))
    #   shinyjs::toggle("clear_selection", condition = !is.na(select_id()))
    # })
    # 
    # # Clear selection on button click
    # shiny::observeEvent(input$clear_selection,
    #                     {
    #                       r[[id]]$select_id(NA)
    #                       cc.map::map_choropleth_update_selection(
    #                         session = session,
    #                         map_ID = "map",
    #                         select_id = NA
    #                       )
    #                     },
    #                     ignoreInit = TRUE
    # )
  })
}


# UI ----------------------------------------------------------------------

zoning_UI <- function(id) {
  page <- modules[modules$id == id, ]
  theme_lowercased <- gsub(" .*", "", tolower(page$theme))
    
  shiny::tagList(
    shiny::div(
      `data-theme` = theme_lowercased,
      # Sidebar
      curbcut::sidebar_UI(
        id = shiny::NS(id, id),
        bottom = shiny::tagList(
          curbcut::legend_UI(shiny::NS(id, id)),
        )
      ),
      
      # Map
      curbcut::map_js_UI(shiny::NS(id, id)),
      
      # Right panel
      curbcut::right_panel(
        id = id,
        curbcut::explore_UI(shiny::NS(id, id))
      )
    )
  )
}


# server ------------------------------------------------------------------

zoning_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observe({
      shinyjs::hide("zoning-left_widgets")
    })

    output[[shiny::NS(id, "map_ph")]] <- shiny::renderUI({
      cc.map::map_input(
        map_ID = shiny::NS(id, shiny::NS(id, "map")),
        username = mapbox_username,
        token = map_token,
        longitude = map_loc[1],
        latitude = map_loc[2],
        zoom = 13,
        map_style_id = map_base_style,
        tileset_prefix = tileset_prefix,
        stories = NULL,
        stories_min_zoom = 13
      )
    })
    
    # Zoom and POI reactives when the view state of the map changes.
    shiny::observeEvent(map_viewstate(), {
      r[[id]]$zoom(zoom_get(zoom = map_viewstate()$zoom))
      r[[id]]$poi(update_poi(
        id = id, poi = r[[id]]$poi(),
        map_viewstate = map_viewstate()
      ))
    }, ignoreInit = TRUE)

    # Update selected ID
    curbcut::update_select_id(id = id, r = r, data = data)
    
    tile <- shiny::reactive("zoning")
    
    # Sidebar
    curbcut::sidebar_server(id = id, r = r)
    
    # Legend
    legend_zoning_server(
      id = id,
      r = r,
      legend_fun = shiny::reactive(zoning_legend)
    )

    # Update map in response to variable changes or zooming
    map_viewstate <- curbcut::map_js_server(
      id = id,
      r = r,
      tile = shiny::reactive("zoning"),
      select_id = r[[id]]$select_id,
      zoom = r[[id]]$zoom,
      coords = r[[id]]$coords,
      vars = shiny::reactive(NULL),
      fill_fun = shiny::reactive(scale_fill_zoning), 
      fill_fun_args = shiny::reactive(list()),
      outline_color = shiny::reactive("white"),
      outline_width = shiny::reactive(0.1),
      stories = stories
    )

    # Explore panel
    explore_zoning_server(
      id = id,
      r = r,
      table_fun = shiny::reactive(zoning_info_table),
      table_args = shiny::reactive(list(select_id = r[[id]]$select_id())),
      graph_fun = shiny::reactive(zoning_graph),
      graph_args = shiny::reactive(list(select_id = r[[id]]$select_id()))
    )

    # # Bookmarking
    # bookmark_server(
    #   id = id,
    #   r = r,
    #   select_id = r[[id]]$select_id,
    #   map_viewstate = map_viewstate
    # )
    
  })
}
