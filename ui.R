# Sources to add
# ESRI Imagery
# http://bl.ocks.org/d3noob/8663620

# Canada EWLAT
# https://www.bio.gc.ca/science/data-donnees/can-ewlat/index3-en.php




ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
  
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "/css/appstyle.css")
  # ),
  # 
  
  HTML('<div class="jumbotron">
    <h2 class="maintitle">MNAI Coastal Protection Benefit Tool (CPBT)</h2>
    <h1>The Single Profile Sandbox Explorer</h1>
    <p></p>
    </div>'),
  
  sidebarLayout(
    
  
  sidebarPanel(
    
  
  HTML('<h3 class="maintitle">Input Elevation Profile</h3>'),
    
  fileInput("shoreline_profile", "Choose CSV File", accept = ".csv"),  
    
  numericInput(inputId = "SmoothParameter",
              label = "Profile Smoothing (%)",
              value = 10,
              min = 0.1,
              max = 100),
  
  fluidRow(
  
    HTML('<h3 class="maintitle">Offshore Wave Parameters</h3>'),
    #HTML('<p>Enter offshore wave height and period or estimate these values from wind speed and fetch distance.</p>'),
    
    column(6,
           
      numericInput(inputId = "Ho",
                   label = "Wave Height - Ho (m)",
                   value = 2.5,
                   min = 0.01,
                   max = 10),
    ),
    column(6,
    
      numericInput(inputId = "To",
                   label = "Wave Period - To (s)",
                   value = 7,
                   min = 0.01,
                   max = 30)
    )
    
  ), # end of fluid row
  
  
  
  
  fluidRow(
    

    column(4,
           
           numericInput(inputId = "Us",
                        label = "Wind Speed (m/s)",
                        value = 15,
                        min = 0.01,
                        max = 10),
    ),
    column(4,
           
           numericInput(inputId = "Ft",
                        label = "Fetch (km)",
                        value = 50,
                        min = 1,
                        max = 1000)
    ),
    
    column(4,
          
           textOutput("windwave_result")
    )
    
    
  ), # end of fluid row
  
  
  
  

  
  
  
  
  fluidRow(
    
    HTML('<h3 class="maintitle">Tides and Water Levels</h3>'),
    
    column(6,
           
           numericInput(inputId = "mean_sea_level",
                        label = "Mean Sea Level (m)",
                        value = 2,
                        min = 0.001,
                        max = 15),
    ),
    column(6,
           
           numericInput(inputId = "mean_high_water",
                        label = "Mean High-high Water (m)",
                        value = 2,
                        min = 0.001,
                        max = 20),
    )
    
  ), # end of fluid row
  
  
  
  
  
  
  
  
  
  
  
  fluidRow(
    

    column(6,
           
           numericInput(inputId = "surge_elevation",
                        label = "Surge Elevation (m)",
                        value = 0,
                        min = 0.01,
                        max = 15),
    ),
    column(6,
           
           numericInput(inputId = "sea_level_rise",
                        label = "Sea-Level Rise (m)",
                        value = 0,
                        min = 0.001,
                        max = 50),
    )
    
  ), # end of fluid row
  
  
  
  
  
  
  
  
  
  
  
  fluidRow(
    
    HTML('<h3 class="maintitle">Foreshore Schematics</h3>'),
    
    column(6,
           
           numericInput(inputId = "foreshore_slope", ############# rename !!!!
                        label = "Foreshore Slope (%)",
                        value = 4,
                        min = 0.01,
                        max = 999)
    ),
    column(6,
           
           # Value between   0.08 - 2
           numericInput(inputId = "sed_size",
                        label = "Sediment Grain Size (mm)",
                        value = 0.2,
                        min = 0.001,
                        max = 10),
    )
    
  ), # end of fluid row
  
  



  fluidRow(
    
    column(4,
           
           numericInput(inputId = "berm_lengt",
                        label = "Berm Width (m)",
                        value = 10,
                        min = 0.01,
                        max = 999),
    ),
    
    column(4,

           numericInput(inputId = "berm_heigh",
                        label = "Berm Height (m)",
                        value = 1,
                        min = 0.01,
                        max = 999),
    ),
    
    column(4,
           
           numericInput(inputId = "dune_heigh",
                        label = "Dune Height (m)",
                        value = 2,
                        min = 0.01,
                        max = 999),    
    )
    
  ), # end of fluid row
  
  fluidRow(
    
    HTML('<h3 class="maintitle">Temporal Storm Erosion Parameters</h3>'),
    
    column(6,
           
           numericInput(inputId = "storm_duration",
                        label = "Storm Duration (hrs)",
                        value = 3,
                        min = 0.01,
                        max = 50),
    ),
    column(6,
           
           numericInput(inputId = "Tr",
                        label = "Return Period (yrs)",
                        value = 50,
                        min = 0.5,
                        max = 1500),
           
    )

  ), # end of fluid row
  
  
  
  
  
  
  
  
  
  
  fluidRow(
    column(6,
           numericInput(inputId = "Longshore",
                        label = "Longshore Distance (m)",
                        value = 1000,
                        min = 1,
                        max = 10000000000),
    ),
    column(6,
           numericInput(inputId = "PropValue",
                        label = "Land Value ($/m2)",
                        value = 200,
                        min = 0,
                        max = 10000000000),
           
    )
  ), # end of fluid row
  fluidRow(
    column(6,
           numericInput(inputId = "disc",
                        label = "Annual Discount Rate (0-1)",
                        value = 0.05,
                        min = 0,
                        max = 1),
    ),
    column(6,
           numericInput(inputId = "TimeHoriz",
                        label = "Time Horizon (yrs)",
                        value = 100,
                        min = 0,
                        max = 10000000000),
           
    )
  ), # end of fluid row

  
  
  
  HTML('<h3 class="maintitle">Add Vegetation Patch</h3>'),
  
  
  fluidRow(
    
    column(4,
           numericInput(inputId = "veg_start",
                        label = "Vegetation Start",
                        value = 300,
                        min = 900,
                        max = 900), 
    ),
    
    column(4,
           
           numericInput(inputId = "veg_end",
                        label = "Vegetation End",
                        value = 20,
                        min = -900,
                        max = -900),

    ),
    
    column(4,
           
           numericInput(inputId = "veg_cd",
                        label = "Species Cd",
                        value = 0.1,
                        min = 0.0001,
                        max = 999),
   
    )
    
  ), # end of fluid row
  

  


  fluidRow(
    
    column(4,
           numericInput(inputId = "veg_height",
                        label = "Blade Height (m)",
                        value = 1.5,
                        min = 0.001,
                        max = 15),
           
    ),
    
    column(4,
           
           numericInput(inputId = "veg_diam",
                        label = "Blade Diameter (m)",
                        value = 0.02,
                        min = 0.0001,
                        max = 20),
           
    ),
    
    column(4,
           
           numericInput(inputId = "veg_density",
                        label = "Blade Density (#/m2)",
                        value = 300,
                        min = 0.001,
                        max = 20000),
    )
    
  ), # end of fluid row
  
  
  

  fluidRow(
    column(6, align="center",
           actionButton('insertBtn', 'Add Veg.')
           ), 
    column(6, align="center",
           actionButton('removeBtn', 'Remove Veg.')
    )
  ),
  fluidRow(
    column(10, #align="center",
    tags$div(id = 'veg_patch_placeholder') 
    )
  ),

    
  
  


  HTML('<h3 class="maintitle">Run Simulation</h3>'),
  

  fluidRow(
    column(10, align="center",
           actionButton("wave_gen_button", "Update Predictions", class="btn btn-primary btn-lg")
    )
  ),
  
  

    ), # end of sidebarPanel
  
  
  
  
  
  
  mainPanel(
    #plotOutput("distPlot")
    
    
    fluidRow(
      column(12, align="center",
             HTML('<h3 class="maintitle">Wave Attenuation</h3>')
      )
    ),
    
    dygraphs::dygraphOutput("result_dygraph1"),
    dygraphs::dygraphOutput("result_dygraph2"),
    dygraphs::dygraphOutput("result_dygraph3", height = "200px"),
    
    
    HTML('<hr>'),

    fluidRow(
      column(12, align="center",
             HTML('<h3 class="maintitle">Horizontal Beach Retreat (m)</h3>')
      )
    ),
    
    fluidRow(
      column(6, align="center",
             htmlOutput("retreat_noveg")
      ),
      column(6, align="center",
             htmlOutput("retreat_veg")
      )
    ),
    
    
    HTML('<hr>'),

    fluidRow(
      column(12, align="center",
             HTML('<h3 class="maintitle">Vertical Wave Runup (m)</h3>')
      )
    ),
    
    fluidRow(
      column(6, align="center",
             htmlOutput("runup_noveg")
      ),
      column(6, align="center",
             htmlOutput("runup_veg")
      )
    ),
    
    
    fluidRow(
      column(12, align="center",
             HTML('<h3 class="maintitle">Erosion Damage ($)</h3>')
      )
    ),
    
    fluidRow(
      column(6, align="center",
             htmlOutput("edamage_noveg")
      ),
      column(6, align="center",
             htmlOutput("edamage_veg")
      )
    ),
    
    
    
    HTML('<hr>'),
    HTML('<br>'),

    
    
    fluidRow(
      column(12, align="center",
        HTML('<h3 class="maintitle">Overland Flooding Extent</h3>'),
        leaflet::leafletOutput("mymap", width = "800px", height = "600px")
      )
    ),
    
    HTML('<br>')
    

    
    
  )
  
  
  ) # end of sidebarLayout
  
  
)
  
  
  