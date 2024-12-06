# Load required libraries
library(shiny)
library(rsconnect)
library(shinyjs)
library(conflicted)
library(shinythemes)
library(leaflet)
library(bslib)
library(sf)
library(tidyverse)
library(DT)
library(leaflet.extras)
library(shinyWidgets)

conflicts_prefer(dplyr::filter)
conflicts_prefer(shinyjs::show)

#### UI ####
# Defines the overall layout and visual elements of the application.
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700&display=swap", rel = "stylesheet"),
    includeCSS("www/styles.css")
  ),
  div(id = "app-header", h1("🏡 Philadelphia Neighborhood Explorer")),
  div(
    id = "app-container",
    div(
      id = "welcome_panel",
      class = "welcome-panel",
      h1("Do you have a Housing Choice Voucher?"),
      h4("Philadelphia is a big city made up of smaller areas and neighborhoods.
          It can be hard to know where to look for a home with a housing voucher.
          This tool helps you narrow down your housing search and determine your rent limit in different neighborhoods."),
      br(),
      actionButton("start_button", "Start", class = "btn-custom"),
      br(),
      p("This will take about 5 minutes."),
      br()
    ),
    hidden(div(
      id = "main_content", class = "main-content", uiOutput("question_ui")
    ))
  )
)

#### DATA ####
nb <- st_read("panel_2024.geojson") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG:4326") %>%
  mutate(across(starts_with("cost"), as.numeric)) %>%
  filter(tract != 42101006500)

neigh_bounds <- st_read("phl_neighs_2024.geojson") %>%
  st_as_sf() %>%
  st_transform(crs = "EPSG:4326") %>%
  st_make_valid() %>%
  mutate(neighborhood = as.character(MAPNAME))

zip_centroids <- st_read("phl_zips_2024.geojson") %>%
  st_centroid() %>%
  st_transform(crs = "EPSG:4326") %>%
  st_make_valid()

# Compute median vouchers
vouchers_median <- median(nb$vouchers, na.rm = TRUE)

# Define neighborhoods by region
# (unchanged definitions)
north_neighborhoods <- c("Juniata Park", "Northwood", "Upper Kensington", "Hunting Park", "Nicetown", "Tioga", "Ludlow", "Fairhill",
                         "Feltonville", "Logan", "Spring Garden", "Fairmount", "North Central", "Franklinville", "West Kensington", "Hartranft",
                         "Brewerytown", "Northern Liberties", "Strawberry Mansion", "Allegheny West", "Olney", "Stanton", "Glenwood", "McGuire", "Yorktown",
                         "Melrose Park Gardens", "Ogontz", "Fern Rock", "East Poplar", "Francisville", "Sharswood", "Old Kensington", "West Poplar")

northwest_neighborhoods <- c("West Oak Lane", "East Oak Lane", "Chestnut Hill", "East Germantown", "Southwest Germantown", "Roxborough",
                             "Manayunk", "West Mount Airy", "East Mount Airy", "Andorra", "Cedarbrook", "East Falls", "Wissahickon", "Germany Hill",
                             "East Park", "Dearnley Park", "Upper Roxborough", "West Central Germantown", "Germantown - Westside", "Germantown - Penn Knox", "Wister",
                             "Germantown - Morton", "Wissahickon Park", "Roxborough Park", "Wissahickon Hills", "Blue Bell Hill", "West Park")

northeast_neighborhoods <- c("Mayfair", "Tacony", "Holmesburg", "Fox Chase", "Bustleton", "Somerton", "Oxford Circle", "Rhawnhurst",
                             "Bridesburg", "Fishtown - Lower Kensington", "East Kensington", "Crescentville", "Riverfront", "Lawndale", "Modena",
                             "Millbrook", "Wissinoming", "Franklin Mills", "Parkwood Manor", "Byberry", "Burholme", "Lexington Park", "Pennypack",
                             "Academy Gardens", "Morrell Park", "Pennypack Woods", "Aston-Woodbridge", "Torresdale", "Northeast Phila Airport", "Normandy Village",
                             "Harrowgate", "Richmond", "Frankford", "Pennypack Park", "Winchester Park", "West Torresdale", "Crestmont Farms", "Port Richmond",
                             "Summerdale", "Mechanicsville")

west_neighborhoods <- c("University City", "Wynnefield", "Overbrook", "Carroll Park", "Cobbs Creek", "Walnut Hill", "Spruce Hill", "Southwest Schuylkill",
                        "Wynnefield Heights", "East Parkside", "West Parkside", "Belmont", "Mantua", "Haverford North", "Woodland Terrace", "Cedar Park",
                        "Powelton", "West Powelton", "Dunlap", "Haddington", "Mill Creek", "Garden Court")

center_city_neighborhoods <- c("Rittenhouse", "Logan Square", "Chinatown", "Callowhill",
                               "Society Hill", "Washington Square West", "Old City", "Graduate Hospital",
                               "Center City East", "Fitler Square")

south_neighborhoods <- c("Point Breeze", "Girard Estates", "Passyunk Square", "Whitman", "Lower Moyamensing", "Packer Park", "Stadium District",
                         "Airport", "Navy Yard", "Bartram Village", "Industrial", "Dickinson Narrows", "Pennsport", "Newbold", "East Passyunk",
                         "Queen Village", "Hawthorne", "Bella Vista", "West Passyunk", "Greenwich")

southwest_neighborhoods <- c("Kingsessing", "Elmwood", "Eastwick", "Penrose", "Paschall", "Grays Ferry", "Clearview")

region_list <- list(
  "North" = north_neighborhoods,
  "Northwest" = northwest_neighborhoods,
  "Northeast" = northeast_neighborhoods,
  "West" = west_neighborhoods,
  "Center City" = center_city_neighborhoods,
  "South" = south_neighborhoods,
  "Southwest" = southwest_neighborhoods
)

features <- c(
  "safety" = "Safety",
  "transit_density" = "Transit",
  "downtown_prox" = "Close to Center City",
  "shopping" = "Shops and Businesses",
  "grocery" = "Grocery stores",
  "parks" = "Parks",
  "healthcare" = "Healthcare",
  "same_house_pct2022" = "Longtime residents"
)

question_info_list <- list(
  "safety" = list(question = "🤝 Safety", info = "A safe neighborhood has less crime and can help you feel more secure."),
  "transit_density" = list(question = "🚎 Transit", info = "Living near transit can make it easier to get around the city without a car."),
  "downtown_prox" = list(question = "🌆 Close to Center City", info = "Neighborhoods close to Center City may have more amenities and opportunities."),
  "shopping" = list(question = "🛍 Shops and Businesses", info = "Living close to shops and businesses can be convenient for shopping and activities."),
  "grocery" = list(question = "🥕 Grocery stores", info = "Being near grocery stores makes it easier to buy fresh food and other essentials."),
  "parks" = list(question = "🌳 Parks and green space", info = "Living near parks gives you a place to relax, exercise, and enjoy fresh air."),
  "healthcare" = list(question = "🩺 Healthcare facilities", info = "Living near healthcare providers makes it easier to see a doctor when you need to."),
  "same_house_pct2022" = list(question = "🏘️ Longtime residents", info = "Residents in these neighborhoods have been living here for longer.")
)

#### FUNCTIONS ####
renderProgressBar <- function(percent) {
  tags$div(
    class = "progress",
    tags$div(
      class = "progress-bar",
      role = "progressbar",
      style = paste0("width: ", percent, "%;"),
      `aria-valuenow` = percent,
      `aria-valuemin` = "0",
      `aria-valuemax` = "100"
    )
  )
}

#### SERVER ####
server <- function(input, output, session) {
  current_question <- reactiveVal(1)
  observe({ cat("Current question is:", current_question(), "\n") })
  
  total_steps <- 6
  preferred_neighborhoods <- reactiveVal(character())
  household_size <- reactiveVal(NULL)
  annual_income <- reactiveVal(NULL)
  
  income_limits <- c(
    "1" = 64250,
    "2" = 73400,
    "3" = 82600,
    "4" = 91750,
    "5" = 99100,
    "6" = 106450,
    "7" = 113800,
    "8" = 121150
  )
  
  matched_5_neighs <- reactiveVal(NULL)
  
  update_in_progress <- reactiveValues(region = FALSE, neighborhood = FALSE)
  
  observeEvent(input$start_button, {
    hide("welcome_panel")
    show("main_content")
    current_question(1)
  })
  
  renderPreferredList <- function() {
    cat("Rendering preferred list\n")
    neighborhoods <- preferred_neighborhoods()
    if (length(neighborhoods) == 0) {
      p("No neighborhoods selected yet.")
    } else {
      div(class = "preferred-list", lapply(neighborhoods, function(nbh) {
        div(class = "preferred-item",
            span(nbh),
            actionButton(paste0("remove_", gsub(" ", "_", nbh)), "Remove", icon = icon("trash")))
      }))
    }
  }
  
  renderRecommendedList <- function(neighborhoods) {
    cat("Rendering recommended list\n")
    if (length(neighborhoods) == 0) {
      p("No areas match your preferences.")
    } else {
      div(class = "recommended-list", lapply(neighborhoods, function(nbh) {
        div(class = "recommended-item", nbh)
      }))
    }
  }
  
  output$question_ui <- renderUI({
    current_q <- current_question()
    progress_percent <- ((current_q - 1) / (total_steps)) * 100
    
    if (current_q == 1) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Getting started"),
        br(),
        h4("We'll start by learning what neighborhood features are important to you.
            After you've rated all the features, we'll show you neighborhoods that match your preferences."),
        br(),
        actionButton("next_info_1", "I'm ready", class = "btn-custom")
      )
    } else if (current_q == 2) {
      tagList(
        renderProgressBar(progress_percent),
        h1("How important is it to have the following where you live?"),
        br(),
        tagList(lapply(names(features), function(feature_key) {
          current_question_text <- question_info_list[[feature_key]]$question
          current_info_text <- question_info_list[[feature_key]]$info
          feature_input_id <- paste0("importance_", gsub(" ", "_", feature_key))
          
          tagList(
            h3(current_question_text),
            p(id = "question-info", current_info_text),
            sliderTextInput(
              inputId = feature_input_id,
              label = NULL,
              choices = c("Less", "Somewhat", "More", "Most"),
              grid = FALSE,
              selected = "Somewhat"
            ),
            br()
          )
        })),
        br(),
        actionButton("back_feature", "Back", class = "btn-custom"),
        actionButton("next_feature", "Next", class = "btn-custom")
      )
    } else if (current_q == 3) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Neighborhood Preferences"),
        br(),
        h4("Select places you'd prefer to live by using the menu on the left or the map on the right.
            You can also select all or clear all."),
        div(
          actionButton("next_info_2", "I'm ready", class = "btn-custom")
        )
      )
    } else if (current_q == 4) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Preferred Neighborhoods"),
        h4("Use the menu or click on the map."),
        actionButton("select_all", "Select All", class = "btn-custom"),
        actionButton("clear_all", "Clear All", class = "btn-custom"),
        br(),
        fluidRow(
          column(width = 6, uiOutput("region_neighborhood_selection_ui")),
          column(width = 6, leafletOutput("philly_map_selection", height = "900px"))
        ),
        br(),
        div(
          actionButton("back_neigh_sel", "Back", class = "btn-custom"),
          actionButton("next_neigh_sel", "Next", class = "btn-custom")
        )
      )
    } else if (current_q == 5) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Household Size"),
        br(),
        h4("How many people will live in this unit?"),
        h4("This helps determine your voucher's rent limit."),
        br(),
        numericInput("household_size", label = NULL, value = 3, min = 1, max = 8, step = 1),
        br(),
        actionButton("back_hhsize", "Back", class = "btn-custom"),
        actionButton("next_hhsize", "Next", class = "btn-custom")
      )
    } else if (current_q == 6) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Annual Income"),
        br(),
        h4("Enter your estimated annual income."),
        h4("This helps calculate your monthly contribution."),
        br(),
        numericInput("annual_income", label = NULL, value = 30000, min = 0, max = 150000, step = 5000),
        br(),
        actionButton("back_income", "Back", class = "btn-custom"),
        actionButton("next_income", "Next", class = "btn-custom")
      )
    } else if (current_q == 7) {
      tagList(
        renderProgressBar(progress_percent),
        h1("Neighborhoods for you"),
        br(),
        h4("Here are neighborhoods that match your preferences:"),
        uiOutput("neighborhood_details_table"),
        br(),
        h4("Also consider these neighborhoods:"),
        uiOutput("neighborhood_recs_table"),
        br(),
        h4("Click on a neighborhood on the map for more details."),
        br(),
        leafletOutput("results_map", height = "600px"),
        actionButton("start_over", "Start Over", class = "btn-custom")
      )
    }
  })
  
  monthly_payment <- reactive({
    income <- annual_income()
    household_size_val <- household_size()
    if (is.null(income) || is.null(household_size_val) || is.na(income) || is.na(household_size_val)) {
      return(0)
    }
    if (income <= 167) {
      50
    } else if (household_size_val <= 2) {
      round(0.28 * income / 12)
    } else if (household_size_val <= 5) {
      round(0.27 * income / 12)
    } else {
      round(0.26 * income / 12)
    }
  })
  
  observeEvent(input$next_info_1, {
    current_question(2)
  })
  
  observeEvent(input$back_info_1, {
    hide("main_content")
    show("welcome_panel")
  })
  
  observeEvent(input$next_info_2, {
    current_question(4)
  })
  
  observeEvent(input$back_info_2, {
    current_question(2)
  })
  
  label_to_value <- c("Less" = 0, "Somewhat" = 1, "More" = 2, "Most" = 3)
  
  observeEvent(input$next_feature, {
    current_q <- current_question()
    feature_index <- current_q - 1
    feature_keys <- names(features)
    current_feature_key <- feature_keys[feature_index]
    selected_label <- input[[paste0("importance_", gsub(" ", "_", current_feature_key))]]
    selected_value <- label_to_value[selected_label]
    cat("Selected value for", current_feature_key, "is", selected_value, "\n")
    if (is.null(selected_label)) {
      showModal(modalDialog(
        title = "Please select an answer.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      current_question(3)
    }
  })
  
  observeEvent(input$back_feature, {
    current_question(1)
  })
  
  observeEvent(input$next_neigh_sel, {
    current_question(5)
  })
  
  observeEvent(input$back_neigh_sel, {
    current_question(3)
  })
  
  observeEvent(input$next_hhsize, {
    if (is.null(input$household_size) || input$household_size < 1) {
      showModal(modalDialog(
        title = "Input Required",
        "Please enter a valid number of people living in the unit.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      household_size(input$household_size)
      current_question(6)
    }
  })
  
  observeEvent(input$back_hhsize, {
    current_question(4)
  })
  
  observeEvent(input$next_income, {
    if (is.null(input$annual_income) || input$annual_income < 0) {
      showModal(modalDialog(
        title = "Input Required",
        "Please enter a valid annual income.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      household_size_val <- household_size()
      annual_income_val <- input$annual_income
      max_income <- income_limits[as.character(household_size_val)]
      
      if (!is.null(max_income) && annual_income_val > max_income) {
        showModal(modalDialog(
          title = "Income Limit Exceeded",
          paste0("Your annual income exceeds the maximum allowed income of $",
                 formatC(max_income, format = "f", digits = 0, big.mark = ","),
                 " for a household size of ", household_size_val, "."),
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        annual_income(input$annual_income)
        current_question(7)
      }
    }
  })
  
  observeEvent(input$back_income, {
    current_question(5)
  })
  
  observeEvent(input$start_over, {
    current_question(1)
    show("welcome_panel")
    hide("main_content")
    preferred_neighborhoods(c())
    household_size(NULL)
    annual_income(NULL)
  })
  
  #### Neighborhood Selection Logic ####
  
  observeEvent(input$select_all, {
    all_neighborhoods <- unlist(region_list)
    preferred_neighborhoods(all_neighborhoods)
    
    lapply(names(region_list), function(region_name) {
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      updateCheckboxGroupInput(session, neighborhood_input_id, selected = region_list[[region_name]])
    })
    
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      updateCheckboxInput(session, region_id, value = TRUE)
    })
    
    showNotification("All neighborhoods have been selected.", type = "message")
  })
  
  observeEvent(input$clear_all, {
    preferred_neighborhoods(character())
    
    lapply(names(region_list), function(region_name) {
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      updateCheckboxGroupInput(session, neighborhood_input_id, selected = character(0))
    })
    
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      updateCheckboxInput(session, region_id, value = FALSE)
    })
    
    showNotification("All selected areas have been cleared.", type = "message")
  })
  
  output$philly_map_selection <- renderLeaflet({
    leaflet(data = neigh_bounds, options = leafletOptions(minZoom = 11, maxZoom = 12)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = -75.13406, lat = 40.00761, zoom = 12) %>%
      setMaxBounds(
        lng1 = -75.28027, lat1 = 39.867,
        lng2 = -74.95576, lat2 = 40.13799
      ) %>%
      addPolygons(
        group = "neighborhoods",
        layerId = ~neighborhood,
        fillColor = ~ifelse(neighborhood %in% preferred_neighborhoods(), "cyan3", "darkcyan"),
        color = "white", weight = 1, fillOpacity = 0.5,
        label = ~neighborhood,
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  observeEvent(input$philly_map_selection_shape_click, {
    clicked_neighborhood <- input$philly_map_selection_shape_click$id
    current_preferences <- preferred_neighborhoods()
    
    if (clicked_neighborhood %in% current_preferences) {
      new_preferences <- setdiff(current_preferences, clicked_neighborhood)
    } else {
      new_preferences <- c(current_preferences, clicked_neighborhood)
    }
    preferred_neighborhoods(new_preferences)
  })
  
  output$region_neighborhood_selection_ui <- renderUI({
    lapply(names(region_list), function(region_name) {
      region_id <- paste0("region_", gsub(" ", "_", region_name))
      neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
      
      selected_neighborhoods <- intersect(region_list[[region_name]], preferred_neighborhoods())
      all_selected <- length(selected_neighborhoods) == length(region_list[[region_name]])
      
      div(
        class = "region-container",
        checkboxInput(region_id, label = region_name, value = all_selected),
        checkboxGroupInput(
          inputId = neighborhood_input_id,
          label = NULL,
          choices = region_list[[region_name]],
          selected = selected_neighborhoods,
          inline = TRUE
        )
      )
    })
  })
  
  lapply(names(region_list), function(region_name) {
    region_id <- paste0("region_", gsub(" ", "_", region_name))
    
    observeEvent(input[[region_id]], {
      current_preferences <- preferred_neighborhoods()
      region_neighs <- region_list[[region_name]]
      selected_in_region <- intersect(region_neighs, current_preferences)
      
      if (isTRUE(input[[region_id]])) {
        preferred_neighborhoods(unique(c(current_preferences, region_neighs)))
      } else {
        if (length(selected_in_region) == length(region_neighs)) {
          preferred_neighborhoods(setdiff(current_preferences, region_neighs))
        } else {
          # Do nothing if not all were selected
        }
      }
    }, ignoreInit = TRUE)
  })
  
  lapply(names(region_list), function(region_name) {
    region_id <- paste0("region_", gsub(" ", "_", region_name))
    neighborhood_input_id <- paste0("neighborhoods_", gsub(" ", "_", region_name))
    
    observeEvent(input[[neighborhood_input_id]], {
      all_current_selections <- unlist(lapply(names(region_list), function(r_name) {
        input[[paste0("neighborhoods_", gsub(" ", "_", r_name))]]
      }))
      
      preferred_neighborhoods(unique(all_current_selections))
      
      # Region checkbox updated via renderUI reactivity
    }, ignoreInit = TRUE)
  })
  
  # Compute vouchers usage category
  classify_vouchers <- function(vouchers_val, median_val) {
    if (vouchers_val == 0) {
      "None"
    } else if (vouchers_val < median_val) {
      "Few"
    } else {
      "Many"
    }
  }
  
  #### Matched Neighborhoods Logic ####
  neighs_matched <- reactive({
    preference_weights <- sapply(names(features), function(feature_key) {
      as.numeric(input[[paste0("importance_", gsub(" ", "_", feature_key))]])
    }, simplify = TRUE)
    names(preference_weights) <- names(features)
    
    preferred_neighborhoods_current <- preferred_neighborhoods()
    if (length(preferred_neighborhoods_current) > 0) {
      recommended_data <- nb[nb$neighborhood %in% preferred_neighborhoods_current, ]
    } else {
      recommended_data <- nb
    }
    
    missing_features <- setdiff(names(features), colnames(recommended_data))
    if (length(missing_features) > 0) {
      stop(paste(
        "The following feature columns are missing in recommended_data:",
        paste(missing_features, collapse = ", ")
      ))
    }
    recommended_data_no_geom <- st_drop_geometry(recommended_data)
    feature_data <- recommended_data_no_geom[, names(features), drop = FALSE]
    
    for (feature in names(features)) {
      feature_data[[feature]] <- as.numeric(as.character(feature_data[[feature]]))
      feature_data[[feature]][is.na(feature_data[[feature]])] <- 0
    }
    
    weighted_features <- sweep(feature_data, 2, preference_weights[colnames(feature_data)], `*`)
    recommended_data$score <- rowMeans(weighted_features)
    
    recommended_data <- arrange(recommended_data, desc(score))
    top_neighborhoods <- head(recommended_data, 5)
    top_neighborhoods_centroids <- st_centroid(top_neighborhoods)
    top_neighborhoods_coords <- st_coordinates(top_neighborhoods_centroids)
    top_neighborhoods$lon <- top_neighborhoods_coords[, 1]
    top_neighborhoods$lat <- top_neighborhoods_coords[, 2]
    top_neighborhoods
  })
  
  #### Recommended Neighborhoods Logic ####
  neighs_rec <- reactive({
    preference_weights <- sapply(names(features), function(feature_key) {
      as.numeric(input[[paste0("importance_", gsub(" ", "_", feature_key))]])
    }, simplify = TRUE)
    names(preference_weights) <- names(features)
    
    matched_neighborhoods <- neighs_matched()$neighborhood
    recommended_data <- nb[!nb$neighborhood %in% matched_neighborhoods, ]
    if (nrow(recommended_data) == 0) {
      recommended_data <- nb
    }
    
    missing_features <- setdiff(names(features), colnames(recommended_data))
    if (length(missing_features) > 0) {
      stop(paste(
        "The following feature columns are missing in recommended_data:",
        paste(missing_features, collapse = ", ")
      ))
    }
    
    recommended_data_no_geom <- st_drop_geometry(recommended_data)
    feature_data <- recommended_data_no_geom[, names(features), drop = FALSE]
    
    for (feature in names(features)) {
      feature_data[[feature]] <- as.numeric(as.character(feature_data[[feature]]))
      feature_data[[feature]][is.na(feature_data[[feature]])] <- 0
    }
    
    weighted_features <- sweep(feature_data, 2, preference_weights[colnames(feature_data)], `*`)
    recommended_data$score <- rowMeans(weighted_features)
    
    if (nrow(recommended_data) > 0 && any(recommended_data$neighborhood %in% matched_neighborhoods)) {
      recommended_data <- recommended_data[!recommended_data$neighborhood %in% matched_neighborhoods, ]
    }
    
    recommended_data <- recommended_data %>% arrange(desc(score))
    top_neighborhoods <- head(recommended_data, 5)
    top_neighborhoods_centroids <- st_centroid(top_neighborhoods)
    top_neighborhoods_coords <- st_coordinates(top_neighborhoods_centroids)
    top_neighborhoods$lon <- top_neighborhoods_coords[, 1]
    top_neighborhoods$lat <- top_neighborhoods_coords[, 2]
    top_neighborhoods
  })
  
  output$results_map <- renderLeaflet({
    rec_neigh <- neighs_rec()
    matched_neigh <- neighs_matched()
    hh_size <- household_size()
    
    max_rent_match = case_when(
      hh_size == 1 ~ matched_neigh$cost_1br,
      hh_size == 2 ~ matched_neigh$cost_2br,
      hh_size == 3 ~ matched_neigh$cost_3br,
      hh_size == 4 ~ matched_neigh$cost_4br,
      hh_size == 5 ~ matched_neigh$cost_5br,
      hh_size == 6 ~ matched_neigh$cost_6br,
      hh_size == 7 ~ matched_neigh$cost_7br,
      hh_size == 8 ~ matched_neigh$cost_8br
    )
    
    max_rent_rec = case_when(
      hh_size == 1 ~ rec_neigh$cost_1br,
      hh_size == 2 ~ rec_neigh$cost_2br,
      hh_size == 3 ~ rec_neigh$cost_3br,
      hh_size == 4 ~ rec_neigh$cost_4br,
      hh_size == 5 ~ rec_neigh$cost_5br,
      hh_size == 6 ~ rec_neigh$cost_6br,
      hh_size == 7 ~ rec_neigh$cost_7br,
      hh_size == 8 ~ rec_neigh$cost_8br
    )
    
    monthly_payment_value <- monthly_payment()
    pal <- colorFactor(palette = c("darkcyan", "darkslategray"), domain = c("Matched", "Recommended"))
    req(current_question() == 7)
    monthly_payment <- monthly_payment()
    
    map <- leaflet(data = nb, options = leafletOptions(minZoom = 11, maxZoom = 13)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = -75.13406, lat = 40.00761, zoom = 12) %>%
      setMaxBounds(lng1 = -75.28027, lat1 = 39.867, lng2 = -74.95576, lat2 = 40.13799) %>%
      addPolygons(
        fillColor = "black",
        color = "transparent",
        weight = 0.1,
        fillOpacity = 0.1,
        highlight = highlightOptions(weight = 2, color = "white", fillOpacity = 0.2, bringToFront = TRUE)
      ) %>%
      addPolygons(
        data = neighs_matched(),
        fillColor = "darkcyan",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~ tract_neigh,
        popup = ~ paste0(
          "<h2>", tract_neigh, "</h2><br/><strong>Neighborhood:</strong> ", neighborhood,
          "<br/><strong>Max rent:</strong> $", max_rent_match,
          "<br/><strong>You pay:</strong> $", monthly_payment_value,
          "<br/><strong>HUD pays:</strong> $", as.numeric(max_rent_match) - as.numeric(monthly_payment_value),
          "<br/><strong>Elementary School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", es_catchment, "</a>",
          "<br/><strong>Middle School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", ms_catchment, "</a>",
          "<br/><strong>High School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", hs_catchment, "</a>",
          "<br/><strong>SEPTA Lines:</strong> <a href='https://plan.septa.org/#/' target='_blank'>", transit_line_names, "</a>"
        ),
        highlight = highlightOptions(weight = 2, color = "white", fillOpacity = 0.9, bringToFront = TRUE),
        group = "matched"
      ) %>%
      addPolygons(
        data = neighs_rec(),
        fillColor = "darkslategray",
        color = "white",
        dashArray = "3",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = ~ tract_neigh,
        popup = ~ paste0(
          "<h2>", tract_neigh, "</h2><br/><strong>Neighborhood:</strong> ", neighborhood,
          "<br/><strong>Max rent:</strong> $", max_rent_rec,
          "<br/><strong>You pay:</strong> $", monthly_payment_value,
          "<br/><strong>HUD pays:</strong> $", as.numeric(max_rent_rec) - as.numeric(monthly_payment_value),
          "<br/><strong>Elementary School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", es_catchment, "</a>",
          "<br/><strong>Middle School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", ms_catchment, "</a>",
          "<br/><strong>High School:</strong> <a href='https://philasd.explore.avela.org/' target='_blank'>", hs_catchment, "</a>",
          "<br/><strong>SEPTA Lines:</strong> <a href='https://plan.septa.org/#/' target='_blank'>", transit_line_names, "</a>"
        ),
        highlight = highlightOptions(weight = 2, color = "white", fillOpacity = 0.9, bringToFront = TRUE),
        group = "recommended"
      ) %>%
      addLabelOnlyMarkers(
        data = neighs_matched(),
        lng = ~ lon, lat = ~ lat, label = ~ tract_neigh,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list("font-weight" = "bold", "color" = "gray15")
        )
      ) %>%
      addLabelOnlyMarkers(
        data = neighs_rec(),
        lng = ~ lon, lat = ~ lat, label = ~ tract_neigh,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list("font-weight" = "bold", "color" = "gray15")
        )
      ) %>%
      addLegend(position = "bottomleft", pal = pal, values = c("Matched", "Recommended"))
    map
  })
  
  output$neighborhood_details_table <- renderUI({
    matched_neigh <- neighs_matched()
    hh_size <- household_size()
    max_rent = case_when(
      hh_size == 1 ~ matched_neigh$cost_1br,
      hh_size == 2 ~ matched_neigh$cost_2br,
      hh_size == 3 ~ matched_neigh$cost_3br,
      hh_size == 4 ~ matched_neigh$cost_4br,
      hh_size == 5 ~ matched_neigh$cost_5br,
      hh_size == 6 ~ matched_neigh$cost_6br,
      hh_size == 7 ~ matched_neigh$cost_7br,
      hh_size == 8 ~ matched_neigh$cost_8br
    )
    monthly_payment_value <- monthly_payment()
    
    # Classify vouchers usage
    vouchers_values <- matched_neigh$vouchers
    vouchers_used <- ifelse(vouchers_values == 0, "None",
                            ifelse(vouchers_values < vouchers_median, "Few", "Many"))
    
    details_table <- data.frame(
      Neighborhood = matched_neigh$tract_neigh,
      ZIP_Code = matched_neigh$zip_code,
      Max_Rent = paste0("$", formatC(max_rent, big.mark = ",")),
      HUD_Pays = paste0("$", formatC(max_rent - monthly_payment_value, big.mark = ",")),
      Your_Contribution = paste0("$", formatC(as.numeric(monthly_payment_value), big.mark = ",")),
      Vouchers_used_here = vouchers_used
    )
    
    table_html <- paste(
      "<table style='width:50%; border-collapse: collapse;'>",
      "<thead><tr style='background-color: darkcyan; color: white;'>",
      "<th>Neighborhood</th><th>ZIP Code</th><th>Max Rent</th><th>HUD pays</th><th>You Pay</th><th>Vouchers used here</th>",
      "</tr></thead>",
      "<tbody>",
      paste(apply(details_table, 1, function(row) {
        paste0(
          "<tr style='border: transparent; padding: 8px;'>",
          "<td>", row[1], "</td>",
          "<td>", row[2], "</td>",
          "<td>", row[3], "</td>",
          "<td>", row[4], "</td>",
          "<td>", row[5], "</td>",
          "<td>", row[6], "</td>",
          "</tr>"
        )
      }), collapse = ""),
      "</tbody></table>"
    )
    HTML(
      paste0(
        "<div style='max-height: 200px; overflow-y: auto;'>",
        table_html,
        "</div>"
      )
    )
  })
  
  output$neighborhood_recs_table <- renderUI({
    rec_neigh <- neighs_rec()
    hh_size <- household_size()
    if (nrow(rec_neigh) == 0) {
      return(h4("No additional recommendations available."))
    }
    max_rent = case_when(
      hh_size == 1 ~ rec_neigh$cost_1br,
      hh_size == 2 ~ rec_neigh$cost_2br,
      hh_size == 3 ~ rec_neigh$cost_3br,
      hh_size == 4 ~ rec_neigh$cost_4br,
      hh_size == 5 ~ rec_neigh$cost_5br,
      hh_size == 6 ~ rec_neigh$cost_6br,
      hh_size == 7 ~ rec_neigh$cost_7br,
      hh_size == 8 ~ rec_neigh$cost_8br
    )
    monthly_payment_value <- monthly_payment()
    
    # Classify vouchers usage
    vouchers_values <- rec_neigh$vouchers
    vouchers_used <- ifelse(vouchers_values == 0, "None",
                            ifelse(vouchers_values < vouchers_median, "Few", "Many"))
    
    details_table <- data.frame(
      Neighborhood = rec_neigh$tract_neigh,
      ZIP_Code = rec_neigh$zip_code,
      Max_Rent = paste0("$", formatC(max_rent, big.mark = ",")),
      HUD_Pays = paste0("$", formatC(max_rent - monthly_payment_value, big.mark = ",")),
      Your_Contribution = paste0("$", formatC(as.numeric(monthly_payment_value), big.mark = ",")),
      Vouchers_used_here = vouchers_used
    )
    
    table_html <- paste(
      "<table style='width:50%; border-collapse: collapse;'>",
      "<thead><tr style='background-color: darkcyan; color = white;'>",
      "<th>Neighborhood</th><th>ZIP Code</th><th>Max Rent</th><th>HUD pays</th><th>You Pay</th><th>Vouchers used here</th>",
      "</tr></thead>",
      "<tbody>",
      paste(apply(details_table, 1, function(row) {
        paste0(
          "<tr style='border: transparent; padding: 8px;'>",
          "<td>", row[1], "</td>",
          "<td>", row[2], "</td>",
          "<td>", row[3], "</td>",
          "<td>", row[4], "</td>",
          "<td>", row[5], "</td>",
          "<td>", row[6], "</td>",
          "</tr>"
        )
      }), collapse = ""),
      "</tbody></table>"
    )
    HTML(
      paste0(
        "<div style='max-height: 200px; overflow-y: auto;'>",
        table_html,
        "</div>"
      )
    )
  })
}

shinyApp(ui, server)
