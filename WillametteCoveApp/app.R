library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(grid)
library(png)
library(readxl)
library(base64enc)

# These are the shapefiles that we use for mapping the decision units.
wc_data <- st_read("320001056-18 (App. C Grids)5.dxf")
st_crs(wc_data) <- 2261

# This is the data from the study, includes all concentrations of metals and other chemicals.
metals <- read_excel("WillametteCoveData.xlsx", sheet = "Metals by EPA Method 6020B")
dioxins_furans <- read_excel("WillametteCoveData.xlsx", sheet = "Dioxins Furans by EPA Method 16")
PAHs <- read_excel("WillametteCoveData.xlsx", sheet = "PAHs and Dibenzofuran by EPA Me")
PCBs <- read_excel("WillametteCoveData.xlsx", sheet = "PCBs by EPA Method 8082A")

# Hot spot levels
hot_spot <- read_excel("WillametteCoveData.xlsx", sheet = "Hot Spot Levels")

# Filters out one 3D LINESTRING that broke the graphic.
# The 00 Grid Areas layer includes boundaries of decision units.
decision_units <- filter(wc_data, Layer == "00 Grid Areas" & EntityHandle != "17ADF")

decision_units <- decision_units %>%
  mutate(Text = recode(EntityHandle,
                       "B66B" = "DU-1",
                       "B654" = "DU-2",
                       "B66C" = "DU-3",
                       "B653" = "DU-4",
                       "B66A" = "DU-5",
                       "B655" = "DU-6",
                       "B669" = "DU-7",
                       "B668" = "DU-8",
                       "B667" = "DU-9",
                       "B664" = "DU-10",
                       "B662" = "DU-11",
                       "B65E" = "DU-12",
                       "B663" = "DU-13",
                       "B66E" = "DU-14",
                       "B65F" = "DU-15",
                       "17B12" = "DU-16",
                       "B657" = "DU-17",
                       "B674" = "DU-18",
                       "B66D" = "DU-19",
                       "14FF5" = "DU-20",
                       "B673" = "DU-21",
                       "14FFD" = "DU-22",
                       "15005" = "DU-23",
                       "B661" = "DU-24",
                       "B666" = "DU-25",
                       "B665" = "DU-26",
                       "B660" = "DU-27",
                       "D1E8" = "DU-28",
                       "D1F1" = "DU-29",
                       "D1F0" = "DU-30",
                       "17E12" = "DU-31",
                       "17E18" = "DU-31",
                       "D1F7" = "DU-32",
                       "B676" = "DU-33",
                       "B65A" = "DU-34",
                       "B65B" = "DU-35",
                       "1802C" = "DU-36",
                       "B66F" = "DU-36",
                       "B670" = "DU-37",
                       "B658" = "DU-38",
                       "B60F" = "DU-38",
                       "BE09" = "DU-39",
                       "EA3A" = "DU-39",
                       "EA3B" = "DU-40",
                       "D1EA" = "DU-41",
                       "17E0A" = "DU-43",
                       "18029" = "DU-44",
                       "BE89" = NA_character_))

linestrings <- decision_units$geometry

polygons <- lapply(linestrings, function(ls) {
  
  coords <- st_coordinates(ls)
  
  # Close the LINESTRING
  closed_coords <- rbind(coords, coords[1, ])
  
  # Create POLYGON
  st_polygon(list(closed_coords))
  
})

# Set Coordinate Reference System to be able to plot correctly
polygons_sf <- st_sfc(polygons, crs = 2261)

concentrations_polygons <- decision_units %>%
  mutate(geometry = polygons_sf)

# Removes unnecessary Z coordinate of geometries
concentrations_polygons$geometry <- st_zm(concentrations_polygons$geometry)

# Constants of columns for cleaning data.
dioxin_furan_cols <- c(
  "2,3,7,8-TCDD",
  "1,2,3,7,8-PeCDD",
  "1,2,3,4,7,8-HxCDD",
  "1,2,3,6,7,8-HxCDD",
  "1,2,3,7,8,9-HxCDD",
  "1,2,3,4,6,7,8-HpCDD",
  "OCDD",
  "2,3,7,8-TCDF",
  "1,2,3,7,8-PeCDF",
  "2,3,4,7,8-PeCDF",
  "1,2,3,4,7,8-HxCDF",                     
  "1,2,3,6,7,8-HxCDF",                     
  "2,3,4,6,7,8-HxCDF",                     
  "1,2,3,7,8,9-HxCDF",                      
  "1,2,3,4,7,8-HpCDF",                       
  "1,2,3,4,7,8,9-HpCDF",                    
  "OCDF",                                             
  "Total TCDD",                                       
  "Total PeCDD",                                  
  "Total HxCDD",                               
  "Total HpCDD",                                  
  "Total TCDF",                          
  "Total PeCDF",                                   
  "Total HxCDF",                                    
  "Total HpCDF",                                     
  "Total D/F TEQ (Mammal)")

PAHs_cols <- c(
  "Acenaphthene",
  "Acenaphthylene",
  "Anthracene",
  "Benz(a) anthracene",
  "Benzo(a) pyrene",
  "Benzo(b) fluoranthene",
  "Benzo(k) fluoranthene",
  "Benzo(g,h,i) perylene",
  "Chrysene",
  "Dibenz(a,h) anthracene",              
  "Fluoranthene",                                                       
  "Fluorene",                                                                 
  "Indeno(1,2,3-cd)pyrene",                                 
  "1-Methylnaphthalene",                                          
  "2-Methylnaphthalene",                                     
  "Naphthalene",                                                       
  "Phenanthrene",                                                   
  "Pyrene",                                                               
  "Dibenzofuran",
  "Total HPAH",                                                          
  "Total LPAH",                                                   
  "cPAHs (BaP Eq.)")

PCBs_cols <- c(
  "Aroclor 1016",
  "Aroclor 1221",
  "Aroclor 1232",
  "Aroclor 1242",
  "Aroclor 1248",
  "Aroclor 1254",
  "Aroclor 1260",
  "Aroclor 1262",
  "Aroclor 1268",
  "Total PCB Aroclors")

metals_long <- metals %>%
  select(-"Metals by EPA Method 6020B") %>%
  pivot_longer(
    cols = c(Antimony, Arsenic, Chromium, Copper, Lead, Mercury, Nickel, Selenium, Zinc),  
    names_to = "Chemical",                      
    values_to = "Concentration"                    
  ) %>%
  pivot_longer(
    cols = ends_with("_detection"),         
    names_to = "Chemical_detection",              
    values_to = "Detection"                       
  ) %>%
  mutate(
    Chemical_detection = str_replace(Chemical_detection, "_detection", "")
  ) %>%
  filter(Chemical == Chemical_detection) %>%
  filter(`Sample Type...6` != "Mean") %>% # Getting rid of aggregate calculations
  select(-Chemical_detection, -`Sample Type...6`) %>%
  rename("Sample Type" = "Sample Type...3")

dioxins_furans_long <- dioxins_furans %>%
  select(-"Dioxins/Furans by EPA Method 1613B") %>%
  pivot_longer(
    cols = all_of(dioxin_furan_cols),
    names_to = "Chemical",                      
    values_to = "Concentration"                    
  ) %>%
  pivot_longer(
    cols = ends_with("_detection"),         
    names_to = "Chemical_detection",              
    values_to = "Detection"                       
  ) %>%
  mutate(
    Chemical_detection = str_replace(Chemical_detection, "_detection", "")
  ) %>%
  filter(Chemical == Chemical_detection) %>%      
  select(-Chemical_detection)

PAHs_long <- PAHs %>%
  select(-"PAHs and Dibenzofuran by EPA Method 8270E-SIM") %>%
  pivot_longer(
    cols = all_of(PAHs_cols),
    names_to = "Chemical",                      
    values_to = "Concentration"                    
  ) %>%
  pivot_longer(
    cols = ends_with("_detection"),         
    names_to = "Chemical_detection",              
    values_to = "Detection"                       
  ) %>%
  mutate(
    Chemical_detection = str_replace(Chemical_detection, "_detection", "")
  ) %>%
  filter(Chemical == Chemical_detection) %>%      
  select(-Chemical_detection)

PCBs_long <- PCBs %>%
  select(-"PCBs by EPA Method 8082A") %>%
  pivot_longer(
    cols = all_of(PCBs_cols),
    names_to = "Chemical",                      
    values_to = "Concentration"                    
  ) %>%
  pivot_longer(
    cols = ends_with("_detection"),         
    names_to = "Chemical_detection",              
    values_to = "Detection"                       
  ) %>%
  mutate(
    Chemical_detection = str_replace(Chemical_detection, "_detection", "")
  ) %>%
  filter(Chemical == Chemical_detection) %>%      
  select(-Chemical_detection)

concentrations <- rbind(metals_long, dioxins_furans_long, PAHs_long, PCBs_long) %>%
  # Removing Field Replicates and keeping original data.
  filter(`Sample Depth (feet bgs)` %in% c("0-1", "1-2", "2-3")) %>%
  # Filtering out mean samples (ends with M)
  filter(!str_detect(`Sample ID`, "M$")) %>%
  mutate(`Decision Unit` = substr(`Decision Unit`, 1, 5)) # Need first 5 characters (i.e. DU-44)

concentrations_geometries <- left_join(concentrations_polygons,
                                       concentrations,
                                       by = c("Text" = "Decision Unit"))

# Recoding values for later join
hot_spot$Page23RDIWorkPlan <- recode(hot_spot$Page23RDIWorkPlan,
                                     "Dioxin/Furan TEQ" = "Total D/F TEQ (Mammal)",
                                     "Total PCBs" = "Total PCB Aroclors")

# Manually mutating values of some CoCs that were in different units in the work plan
hot_spot_accurate <- hot_spot %>%
  mutate(
    `ISM PRG` = case_when(
      Page23RDIWorkPlan == "Total HPAH" ~ `ISM PRG` * 1000,
      Page23RDIWorkPlan == "Total LPAH" ~ `ISM PRG` * 1000,
      Page23RDIWorkPlan == "Dibenzofuran" ~ `ISM PRG` * 1000,
      Page23RDIWorkPlan == "Total PCB Aroclors" ~ `ISM PRG` * 1000,
      Page23RDIWorkPlan == "Total D/F TEQ (Mammal)" ~ `ISM PRG` * 1000000,
      TRUE ~ `ISM PRG`
    ),
    `ISM Hot Spot Level` = case_when(
      Page23RDIWorkPlan == "Total HPAH" ~ `ISM Hot Spot Level` * 1000,
      Page23RDIWorkPlan == "Total LPAH" ~ `ISM Hot Spot Level` * 1000,
      Page23RDIWorkPlan == "Dibenzofuran" ~ `ISM Hot Spot Level` * 1000,
      Page23RDIWorkPlan == "Total PCB Aroclors" ~ `ISM Hot Spot Level` * 1000,
      Page23RDIWorkPlan == "Total D/F TEQ (Mammal)" ~ `ISM Hot Spot Level` * 1000000,
      TRUE ~ `ISM Hot Spot Level`
    )
  )

# Defining CoCs for later click drill down graphic
cocs <- c(
  "Antimony", "Arsenic", "Chromium", "Copper", "Lead",
  "Mercury", "Nickel", "Selenium", "Zinc", "Total HPAH",
  "Total LPAH", "Dibenzofuran", "Total PCB Aroclors",
  "Total D/F TEQ (Mammal)"
  )

ui <- fluidPage(
  titlePanel("Heavy Metals Concentration Plot"),
  
  # dropdown menu for chemicals
  selectInput(
    inputId = "chemical",
    label = "Select Chemical",
    choices = unique(concentrations_geometries$Chemical[!is.na(concentrations_geometries$Chemical)])
  ),
  
  # dropdown menu for depth
  selectInput(
    inputId = "depth",
    label = "Select Depth (feet bgs)",
    choices = unique(concentrations_geometries$`Sample Depth (feet bgs)`[!is.na(concentrations_geometries$`Sample Depth (feet bgs)`)])
  ),
  
  actionButton("btn1", "Concentration", class = "btn-primary active"),
  actionButton("btn2", "Exceedance", class = "btn-secondary"),
  
  # Use fluidRow to create a row with two columns
  fluidRow(
    # First column for heavy_metals_plot
    column(width = 7,
           div(style = "width: 100%; height: 348px;",
               plotlyOutput("heavy_metals_plot")
           )
    ),
    # Second column for du_plot
    column(width = 5,
           div(style = "width: 100%; height: 600px;",
               plotOutput("du_plot")
           )
    )
  )
)

# server logic
server <- function(input, output, session) {
  # Reactive value to store the clicked region
  clicked_region <- reactiveVal(NULL)
  
  # Reactive value to track which button is active
  fill_state <- reactiveVal("concentration_binned")
  
  # Update active state when buttons are clicked
  observeEvent(input$btn1, {
    updateActionButton(session, "btn1", label = "Concentration", icon = icon("chart-bar"))
    updateActionButton(session, "btn2", label = "Exceedance", icon = icon("exclamation-triangle"))
    fill_state("concentration_binned")
  })
  
  observeEvent(input$btn2, {
    updateActionButton(session, "btn1", label = "Concentration", icon = icon("chart-bar"))
    updateActionButton(session, "btn2", label = "Exceedance", icon = icon("exclamation-triangle"))
    fill_state("exceedance")
  })
  
  observeEvent(input$btn1, {
    removeUI("#btn1-style")
    removeUI("#btn2-style")
    insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tags$style(HTML("
      #btn1 { background-color: #007bff; color: white; }
      #btn2 { background-color: #6c757d; color: white; }
    "))
    )
  })
  
  observeEvent(input$btn2, {
    removeUI("#btn1-style")
    removeUI("#btn2-style")
    insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tags$style(HTML("
      #btn1 { background-color: #6c757d; color: white; }
      #btn2 { background-color: #007bff; color: white; }
    "))
    )
  })
  
  output$heavy_metals_plot <- renderPlotly({
    # Filter data based on selected chemical and depth
    filtered_data <- concentrations_geometries %>%
      filter(Chemical == input$chemical,
             `Sample Depth (feet bgs)` == input$depth) %>%
      mutate(
        concentration_binned = {
          # breaks by quantile groups adjust to each input
          breaks <- quantile(Concentration, probs = seq(0, 1, 0.25), na.rm = TRUE)
          cut(Concentration, 
              breaks = breaks,
              labels = c(
                paste0("Q1 (< ", round(breaks[2], 2), ")"),
                paste0("Q2 (", round(breaks[2], 2), " - ", round(breaks[3], 2), ")"),
                paste0("Q3 (", round(breaks[3], 2), " - ", round(breaks[4], 2), ")"),
                paste0("Q4 (> ", round(breaks[4], 2), ")")
              ),
              include.lowest = TRUE)
        }
      )
    
    # Calculate categories for thresholds
    # Joining with hotspot data
    map_thresholds <- left_join(filtered_data,
                                hot_spot_accurate,
                                by = c("Chemical" = "Page23RDIWorkPlan"))
    map_thresholds <- mutate(map_thresholds, exceedance = case_when(
      Concentration > `ISM Hot Spot Level` ~ "Exceeds Hot Spot Level",
      Concentration > `ISM PRG` ~ "Exceeds PRG",
      TRUE ~ "Does not Exceed"
    ))
    
    # Adding centroids for easier tooltip navigation
    centroids <- st_centroid(map_thresholds)
    
    # plot
    heavy_metals_plot <- ggplot() +
      # key = Text referring to Decision Units
      geom_sf(data = map_thresholds,
              aes_string(fill = fill_state(), key = "Text"),
              color = "black",
              linewidth = 0.25) +
      # Adding better tooltip UI
      geom_sf(data = centroids,
              aes(text = paste("Decision Unit:",
                               Text,
                               "<br>Concentration:",
                               round(Concentration, 2),
                               "mg/kg"),
                  key = Text),
              size = 0,
              alpha = 0) +
      theme_minimal() +
      scale_fill_manual(values = c("#ADD8E6", "#FA8072", "#DE6055", "#C24039")) +
      labs(title = paste(input$chemical, "Concentration Plot"),
           subtitle = paste("Higher concentrations of", input$chemical),
           caption = "Source: Willamette Cove RDI Evaluation Report",
           fill = paste("Concentration of\n", input$chemical, "(mg/kg)")) +
      theme(
        panel.spacing = unit(-3, "lines"),
        panel.grid = element_blank(),
        strip.text = element_text(margin = margin(t = 10, b = 75)),
        strip.text.y.left = element_text(size = 16, angle = 0),
        plot.title = element_text(size = 18),
        axis.text = element_blank()
      )
    
    # Convert plot to plotly
    plotly_plot <- ggplotly(heavy_metals_plot,
                            width = 892,
                            height = 348,
                            source = "heavy_metals_plot",
                            tooltip = "text")
    
    plotly_plot <- plotly_plot %>%
      layout(
        images = list(
          list(
            # Encodes image file directly; was having problems getting image to show up otherwise.
            source = base64enc::dataURI(file = "WC_sat.png"),
            xref = "paper",
            yref = "paper",
            x = 0,
            y = 1,
            sizex = 1,
            sizey = 1,
            sizing = "stretch",
            layer = "below"
          )
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        autosize = FALSE,
        hovermode = "closest"
      ) %>%
      config(responsive = FALSE,
             displayModeBar = FALSE)
    
    # Register click event
    event_register(plotly_plot, "plotly_click")
  })
  
  # Observe click events on the plot
  observeEvent(event_data("plotly_click", source = "heavy_metals_plot"), {
    click_data <- event_data("plotly_click", source = "heavy_metals_plot")
    if (!is.null(click_data)) {
      clicked_region(click_data$key)
    }
  })
  
  # Decision Unit Plot Based on Click
  output$du_plot <- renderPlot({
    req(clicked_region())

    # Filter data for region
    decision_unit_clicked <- concentrations %>%
      filter(`Decision Unit` == clicked_region() & Chemical %in% cocs)
    
    # Check if we have data for the clicked region
    if (nrow(decision_unit_clicked) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for this region") +
               theme_void())
    }
    
    # Joining with hotspot data
    du_clicked_HS <- left_join(decision_unit_clicked, hot_spot_accurate, by = c("Chemical" = "Page23RDIWorkPlan"))
    du_clicked_HS <- mutate(du_clicked_HS, exceedance = case_when(
      Concentration > `ISM Hot Spot Level` ~ "Exceeds Hot Spot Level",
      Concentration > `ISM PRG` ~ "Exceeds PRG",
      TRUE ~ "Does not Exceed"
    ))
    
    # Ordering chemicals for the graphic
    ordering <- du_clicked_HS %>%
      group_by(Chemical, exceedance) %>%
      summarize(weight = n()) %>%
      mutate(weight = case_when(
        exceedance == "Exceeds Hot Spot Level" ~ weight * 3,
        exceedance == "Exceeds PRG" ~ weight * 2,
        TRUE ~ weight
      )) %>%
      group_by(Chemical) %>%
      summarize(weight = sum(weight))
    
    # Joining again to order chemicals
    du_clicked_HS <- left_join(du_clicked_HS, ordering)
    du_clicked_HS$exceedance <- factor(du_clicked_HS$exceedance,
                                       levels = c("Exceeds Hot Spot Level",
                                                  "Exceeds PRG",
                                                  "Does not Exceed"))
    
    # Create the detailed decision unit plot
    ggplot(du_clicked_HS) +
      geom_tile(aes(`Sample Depth (feet bgs)`, reorder(Chemical, weight), fill = exceedance),
                color = "black") +
      theme_minimal() +
      labs(title = paste("Which chemicals exceed ecological\nhot spot values in ", clicked_region(), "?", sep = ""),
           subtitle = "PRG = Preliminary Remediation Goal",
           y = "",
           fill = "") +
      scale_fill_manual(values = c("Does not Exceed" = "#deebf7",
                                   "Exceeds PRG" = "#9ecae1",
                                   "Exceeds Hot Spot Level" = "#3182bd")) +
      theme(
        plot.title = element_text(size = 28, hjust = 1, margin = margin(r = 22.5)),
        plot.subtitle = element_text(size = 18, hjust = 1, margin = margin(r = 22.5)),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.box.margin = margin(r = 50)
      )
  }, height = 600)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
