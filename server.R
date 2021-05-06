# Load Libraries
library(sp)
library(sf)
library(mapview)
library(geosphere)
library(raster)
library(pracma)
library(dplyr)
library(fields)
library(rootSolve)
library(e1071)
library(gstat)
library(jsonlite)
library(rgdal)
library(dygraphs)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(shinythemes)
library(MNAI.CPBT)
library(shinyBS)



server <- function(input, output) {
  
  #===================================
  # START DEBUGGING - Default params
  #===================================
  
  # Set Default Values For All Inputs - Debugging
  BufferDist = 300.0         # Buffer distance in meters for verticle line
  RadLineDist = 2          # Line dist in km from point
  SmoothParameter = 10
  ShorelinePointDist = 100 # where to sample points on shoreline - dist between
  PointResolution = 1      # resolution (in meters) of point sampling.
  MaxOnshoreDist = 10       # Max distance onshore in kms
  
  # WAVE INPUTS
  Ho = 2.5                # 'offshore height'  - originally set to 2.5m
  To = 7                  # 'period'  -- 7
  Us = 10                 # 'Wind Speed'
  Ft = 50                 # Fetch distance
  storm_duration = 3      # hours
  surge_elevation = 1    # Surge elevation (m)
  dS = 0.1                # Surge reduction
  dx = 1                  # Model Spatial Resolution (dx)
  EconCP = "Yes"          # Compute Economic Valuation?
  Longshore = 100         # Longshore Extent
  PropValue = 200         # Property Value
  Tr = 50                 # Return Period
  disc = 0.15             # Discount Rate
  TimeHoriz = 50          # Time Horizon
  
  # Flooding
  mean_sea_level = 2
  mean_high_water = 4
  sea_level_rise = 1.5
  
  # Sandy beach
  XelVal = 1
  sand = 1
  
  # foreshore profile
  sed_size <- 0.15 
  berm_lengt <- 20
  berm_heigh <- 0.9
  dune_heigh <- 2
  fore_slp <- 0.04
  
  
  #===================================
  # SIMULATE WAVE AND EROSION
  #===================================
  

  observeEvent(input$wave_gen_button, {
    
    print(paste("Button wave_gen_button was triggerd by user"))
    
    #===================================
    # Gather All User Inputs
    #===================================
    
    
    #...................................
    # Vegetation List
    # print("Getting veg list")
    vegdf <- data.frame()
    if(length(veg_patches)>0){
      for(vv in 1:length(veg_patches)){
        tpath <- veg_patches[vv]
        nvv <- as.numeric(strsplit(tpath, "_")[[1]])
        add_rowv <- data.frame(id=nvv[1], start=nvv[2], end=nvv[3],
                              cd=nvv[4], height=nvv[5], diam=nvv[6], dens=nvv[7])
        vegdf <- rbind(vegdf, add_rowv)
      }
    }
    print(vegdf)
    print("Veg list - ok")
    
    
    
    #...................................
    # Input Profile
      file <- input$shoreline_profile
      ext <- tools::file_ext(file$datapath)
      req(file)
      print(head(read.csv(file$datapath),1))
      this_transect <- read.csv(file$datapath)
      print("1. Profile upload successful")
      
    #...................................
    # Other Params
      SmoothParameter <- input$SmoothParameter
      Ho <- input$Ho  # 'offshore height'  - originally set to 2.5m
      To <- input$To  # 'period'  -- 7
      storm_duration <- input$storm_duration      # hours
      surge_elevation <- input$surge_elevation    # Surge elevation (m)
      Tr <- input$Tr                 # Return Period
      mean_sea_level <- input$mean_sea_level
      mean_high_water <- input$mean_high_water
      sea_level_rise <- input$sea_level_rise
      
      # Erosion calculation
      Longshore <- input$Longshore  #Longshore dist
      PropValue <- input$PropValue  # Land value per meter squared
      disc <- input$disc            # Discount rate
      TimeHoriz <- input$TimeHoriz  # Time horizon
      
      
      sed_size <- input$sed_size
      berm_lengt <- input$berm_lengt
      berm_heigh <- input$berm_heigh
      dune_heigh <- input$dune_heigh
      fore_slp <- input$foreshore_slope/100

      print("2. All other user input parameters were gathered successfully")
      
      
    #...................................
    # Ensure transect is in proper format
      this_transect$line_id <- 999
      if(!("longitude" %in% colnames(this_transect)))
      {
        this_transect$longitude <- -123
      }
      if(!("latitude" %in% colnames(this_transect)))
      {
        this_transect$latitude <- 49
      }
      
      #--------------------------------------------------
      # Fill in gaps in dataset
      #--------------------------------------------------
      Xposseq <- seq(min(this_transect$Xpos, na.rm=TRUE),
                     max(this_transect$Xpos, na.rm=TRUE),
                     by = 1)
      elevinterp <- stats::approx(
        x = this_transect$Xpos,
        y = this_transect$elev,
        xout = Xposseq
      )
      einter <- round(elevinterp$y, 2)
      
      latterp <- stats::approx(
        x = this_transect$Xpos,
        y = this_transect$latitude,
        xout = Xposseq
      )
      latterp <- round(latterp$y, 9)
      
      longterm <- stats::approx(
        x = this_transect$Xpos,
        y = this_transect$longitude,
        xout = Xposseq
      )
      longterm <- round(longterm$y, 9)
      
      new_transect <- data.frame(
        Xpos = Xposseq,
        elev = einter,
        latitude = latterp,
        longitude = longterm
        )
      
      
      this_transect_sf = st_as_sf(new_transect, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
      
      point_elev <- this_transect_sf
      point_elev$line_id <- 999
      
      #mapview(point_elev)
        
    #...................................
    # Smooth signal by percentage - copies SignalSmooth.smooth
      point_elev <- SignalSmooth(
        point_elev = point_elev,
        SmoothParameter = SmoothParameter
      )
      #plot(point_elev$Xpos, point_elev$elev_smooth, type='l')
      print(paste0("4. Elevation profile smoothed by ", SmoothParameter))
      
      
    #...................................
    # Run CleanTransect to confrim good format
      pt_exp <- point_elev
      # no action - just rename object
      

    #...................................
    # Add on vegetation
      
      elev_veg <- pt_exp
      elev_veg$hc <- NA
      elev_veg$d <- NA
      elev_veg$N <- NA
      elev_veg$Cd <- NA
      elev_veg$Type <- 'Eelgrass'
      # Next object to reference
      dat <- elev_veg
      

    #===================================
    # Add vegetation (if any)
    #===================================
      if(nrow(vegdf)>0){
        print("Getting veg...")
        
        for(vv in 1:nrow(vegdf)){
          vr <- vegdf[vv,]
          print(vr)
          p1 <- which(dat$Xpos <= vr$start)
          p1 <- max(p1)
          p2 <- which(dat$Xpos >= vr$end)
		      
          if(length(p2) == 0){
        			print("Veg Error - too far off transect upland...")
        			next
          }
          
          p2 <- min(p2)
          gp <- sort(c(p1,p2))
          p1 <- gp[1]
          p2 <- gp[2]
          
          print(p1)
          print(p2)
          print(nrow(dat))
          
        
          # update values
          dat$hc[p1:p2] <- vr$height
          dat$d[p1:p2] <- vr$diam
          dat$N[p1:p2] <- vr$dens
          dat$Cd[p1:p2] <- vr$cd
          dat$Type[p1:p2] <- 'Eelgrass'
        }
      }
      print(paste0("Max veg height is ",max(dat$hc, na.rm=TRUE)))
      print(paste0("Max veg dens is ",max(dat$N, na.rm=TRUE)))
      print("Veg list - ok")  
      
      
    #...................................
    # Run wave model
    total_wsl_adj = surge_elevation + mean_high_water + sea_level_rise
    print(paste0("5. Total static water is ", total_wsl_adj, " m"))
    
    dat$StemDensty <- dat$N
    dat$StemDiam <- dat$d
    dat$StemHeight <- dat$hc
    
    # run wave attenuation model
    wave_dat <- WaveModel(dat, total_wsl_adj, Ho=Ho, To=To, tran_force=TRUE, print_debug = TRUE)
  
    # all(wave_dat$H_veg == wave_dat$H_noveg)
    print(paste0("6. WaveModel ran successfully"))
    
    # Need to deal with flood water
    
    #...................................
    # Run erosion model
    # Load sediment size lookup
    # ssf_lookup = MNAI.CPBT::getSSF()
    
    # Foreshore params
    fs_dat <- data.frame(line_id = 999,
                         sed_size = sed_size,
                         berm_lengt = berm_lengt,
                         berm_heigh = berm_heigh,
                         dune_heigh = dune_heigh,
                         fore_slp = fore_slp)
    print(paste0("7. Foreshore params gathered"))
    
    
    # Calculate Runup and Erosion
    erosion <- ErosionTransectsUtil(Ho=Ho,
                                   To=To,
                                   total_wsl_adj = total_wsl_adj,
                                   linkbeach = fs_dat,
                                   wave_dat = wave_dat,
                                   storm_duration = storm_duration,
                                   Longshore = Longshore, 
                                   PropValue = PropValue, 
                                   Tr = Tr,
                                   disc = disc,
                                   TimeHoriz = TimeHoriz,
                                   mean_sea_level = mean_sea_level,
                                   mean_high_water = mean_high_water
                                   )
    print(paste0("8. Erosion model success"))
    
    total_wsl_adj_runup <- erosion$runup_Veg + total_wsl_adj
    print(paste0("8B. Runup with veg is ", round(total_wsl_adj_runup, 2)))
    
	
	  #===================================
    # Fix Wave Model output to include points
	  # on shoreline - need to clean transect with FloodPlot
    #===================================
	
	
    # Run Flood Estimates
     fo <- FloodPlot(
       dat = dat,
       erosion = erosion,
       mean_high_water = mean_high_water,
       path_output = 'path_output',
	     export_plot = FALSE,
       total_wsl_adj = total_wsl_adj,
       TrimOnshoreDist = FALSE
     )
    
  	fixd <- ExportProfiles(
  	  fo = fo,
  	  path_output = path_output,
  	  wave_dat = wave_dat,
  	  export_csv = FALSE
  	)
  	
  	if(FALSE){
  	  par(mfrow=c(2,1))
  	  plot(fixd$Xpos, fixd$elev, type='l')
  	  plot(fixd$Xpos, fixd$H_veg, type='l')
  	}
  	
  	print(paste0("8C. Fix success - final profile n rows: ", nrow(fixd)))
  	

      

    
    
    #===================================
    # Adjust XPos to postive for Dygraph
    #===================================
    
    fdat <- fixd # final data from flood
    Xpos_rev <- fdat$Xpos*-1
    pxmin <- min(Xpos_rev, na.rm=TRUE)
    pxmax <- max(Xpos_rev, na.rm=TRUE)+(abs(pxmin))
    

    print("8D. Adj Xpos")
    print(pxmin)
    print(pxmax)
    
    #===================================
    # Start Assembling Dygraph Output plots
    #===================================
    
    # Build Plot Series as ts objects for dygraph
    With_Veg <- ts(data = rev(fdat$H_veg), start = 1, end = pxmax, frequency = 1)
    #dygraph(With_Veg)
    
    print("8E. Set")
    
    Without_Veg <- ts(data = rev(fdat$H_noveg), start = 1, end = pxmax, frequency = 1)
    Elevation <- ts(data = rev(fdat$elev), start = 1, end = pxmax, frequency = 1)
    
    clean_veg <- ifelse(is.na(fdat$hc), 0 , fdat$hc)
    vegts <- ts(data = rev(clean_veg), start = 1, end = pxmax, frequency = 1)
    

    # Wave Plot
    ta <- cbind(With_Veg,Without_Veg)
    

    d1 <- dygraphs::dygraph(ta,
                  main = "Wave Attenuation",
                  xlab = "Cross Shore Distance (m)",
                  ylab = "Wave Height (m)",
                  group = "wave-erosion") %>%
      dyOptions(colors=c("green","red"), fillGraph=TRUE, fillAlpha = 0.1) %>%
      dySeries("With_Veg", label = "With Vegetation (m)") %>%
      dySeries("Without_Veg", label = "Without Vegetation  (m)")
  

      
    d2 <- dygraphs::dygraph(Elevation,
                  main = "Flooding",
                  xlab = "Cross Shore Distance (m)",
                  ylab = "Wave Level (m)",
                  group = "wave-erosion") %>%
      dyOptions(fillAlpha = 0.04, colors=c("black","black"),
                #fillGraph = TRUE, 
                
                #strokeBorderColor = 'brown',
                #strokeBorderWidth = 1,
                drawGrid = FALSE) %>%
      dySeries("V1", label = "Elevation (m)") %>%
      #dyShading(from = from_flood, to = to_flood, color = "#f7e6f6") %>%
      dyShading(from = -999, to = total_wsl_adj_runup, axis = "y", color="#f7e6f6") %>%
      dyShading(from = 0, to = mean_high_water, axis = "y", color="#f0eac2") %>%
      dyShading(from = -999, to = 0, axis = "y", color="#dae4f5") %>%
      dyLimit(0, "MLLW", strokePattern = "dashed", color = "lightgrey") %>%
      dyLimit(mean_sea_level, "MTL", strokePattern = "dotdash", color = "lightgrey") %>%
      dyLimit(mean_high_water, "MHHW", strokePattern = "dashed", color = "lightgrey") %>%
      dyLimit((mean_high_water + sea_level_rise), "SLR", strokePattern = "dotdash", color = "blue") %>%
      dyLimit((mean_high_water + sea_level_rise + surge_elevation), "Storm Surge", strokePattern = "solid", color = "blue") %>%
      dyLimit((mean_high_water + sea_level_rise + surge_elevation + erosion$runup_Veg), "Runup", strokePattern = "solid", color = "blue")
    
    
    
    #--------------------------------
    # Add on flood bands
    if(nrow(fo$flood_marks) > 0){
      marks <- fo$flood_marks
      for(f in 1:nrow(marks)){
        
        # Create flood markers on new scale
        flood_start_veg <- (marks$from[f]*-1)+(abs(pxmin))
        
		my_ind <- which(abs(fdat$elev - total_wsl_adj_runup) == min(abs(fdat$elev - total_wsl_adj_runup), na.rm=TRUE))
        
		flood_end_veg_org <- fdat$Xpos[my_ind[1]]
        flood_end_veg <- (flood_end_veg_org*-1)+(abs(pxmin))
        from_flood <- paste0(flood_start_veg, "-1-1")
        to_flood <- paste0(flood_end_veg, "-1-1")
        to_flood_v2 <- (marks$to[f]*-1)+(abs(pxmin))
        to_flood_v2 <- paste0(to_flood_v2, "-1-1")
        #d1 <- d1 %>% dyShading(from = from_flood, to = to_flood_v2, color = "#f7e6f6")
        #d2 <- d2 %>% dyShading(from = from_flood, to = to_flood_v2, color = "#f7e6f6")
        
      }
    } else {
      my_ind <- which(abs(fdat$elev - total_wsl_adj_runup) == min(abs(fdat$elev - total_wsl_adj_runup), na.rm=TRUE))
	  flood_end_veg_org <- fdat$Xpos[my_ind[1]]
      
    }
    
    
    
    
    
    
    
    d3 <- dygraphs::dygraph(vegts,
                  main = "Vegetation",
                  xlab = "Cross Shore Distance (m)",
                  ylab = "Vegetation Height (m)",
                  group = "wave-erosion") %>%
      dyOptions(fillAlpha = 0.4, colors=c("#419e4f"),
                fillGraph = TRUE,
                drawGrid = FALSE) %>%
      dySeries("V1", label = "Vegetation Height (m)")# %>%
      #dyShading(from = from_flood, to = to_flood, color = "#f7e6f6") %>%
      
    
    print(paste0("9. Dygraph objects assembled"))
    
    # htmltools::browsable(htmltools::tagList(dy_graph))
    
 
    output$result_dygraph1 <- renderDygraph({
      d1
    })
    output$result_dygraph2 <- renderDygraph({
      d2
    })
    
    output$result_dygraph3 <- renderDygraph({
      d3
    })
    
    print(paste0("10. Dygraph objects updated"))
    
    
    output$retreat_veg <- renderText({
      rout <- round(erosion$retreat_Veg, 1)
      paste0("<h3 style='color: green;'>With Vegetation<br>", rout, " m</h3>")
      #rout
    })
    
    output$retreat_noveg <- renderText({
      rout2 <- round(erosion$retreat_NoVeg, 1)
      paste0("<h3 style='color: red;'>Without Vegetation<br>", rout2, "m </h3>")
    })
    
    
    output$runup_veg <- renderText({
      rout3 <- round(erosion$runup_Veg, 1)
      paste0("<h3 style='color: green;'>With Vegetation<br>", rout3, " m</h3>")
      #rout
    })
    
    output$runup_noveg <- renderText({
      rout4 <- round(erosion$runup_NoVeg, 1)
      paste0("<h3 style='color: red;'>Without Vegetation<br>", rout4, "m </h3>")
    })
    
    
    output$edamage_veg <- renderText({
      rout5 <- round(erosion$damage_Veg, 0)
      rout5 <- format(round(as.numeric(rout5), 0), nsmall=1, big.mark=",")
      paste0("<h3 style='color: green;'>With Vegetation<br>$", rout5, "</h3>")
      #rout
    })
    output$edamage_noveg <- renderText({
      rout6 <- round(erosion$damage_NoVeg, 0)
      rout6 <- format(round(as.numeric(rout6), 0), nsmall=1, big.mark=",")
      paste0("<h3 style='color: red;'>Without Vegetation<br>$", rout6, "</h3>")
    })
    
    
    
    
    
    
    output$mymap <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).

      
      s1 <- fdat[which(fdat$Xpos == max(fdat$Xpos, na.rm=TRUE)),]
      s1 <- s1[1,]
      
      xget <- flood_end_veg_org
      s2 <- fdat[which(fdat$Xpos == xget),]
      s2 <- s2[1,]
      print(paste0("11. Leaflet line to Xpos ", xget))
      
      
      bc <- data.frame(lon=c(s1$longitude, s2$longitude),
                                lat=c(s1$latitude, s2$latitude))
      ls <- st_linestring(as.matrix(bc))
      lls <- list()
      lls[[1]] <- ls
      fdat_sp <- st_sfc(lls, crs = "+proj=longlat +datum=WGS84")
      st_crs(fdat_sp) <- 4326
      leaflet(fdat_sp) %>% addProviderTiles(providers$Esri.WorldImagery) %>%
        addPolylines() %>%
        addScaleBar() %>%
        addFullscreenControl()
      
    })
    
  
  })
  
  
  
  
  
  
  
  #===================================
  # Show Wind-Wave Estimates
  #===================================
  
  windwave_input <- reactive({
    Us = input$Us
    Ft = input$Ft
    Ft = Ft*1000 # convert to m
    ww <- windWave(wind_speed = Us, fetch_distance = Ft, water_depth=10)
  })
  
  output$windwave_result <- renderText({ 
    ww <- windwave_input()
    #updateNumericInput(session, inputId = "Ho", value = round(ww[1], 2))
    paste0("Suggested Wind-Wave Height: ", round(ww[1], 2), "m; Period: ", round(ww[2], 2), "s.")
  })
  
  
  
  
  
  
  
  #===================================
  # Add or remove vegetation patches
  #===================================
  ## keep track of veg patches
  ltre_inserted <- c()
  ltre_obj <- c()
  veg_patches <- c()
  
  observeEvent(input$insertBtn, {
    btn <- input$insertBtn
    id <- paste0('txt', btn)
    veg_start <- input$veg_start 
    veg_end <- input$veg_end
    veg_cd <- input$veg_cd
    veg_height <- input$veg_height
    veg_diam <- input$veg_diam
    veg_density <- input$veg_density
    
  
    build_str <- paste0('Veg. Patch ', btn,': From ', veg_start, "m to ", veg_end, "m; Veg. Cd:", veg_cd,
                        "; Blade Height: ",veg_height,"m; Blade Diameter: ",veg_diam,"m; Shoot Density: ",veg_density,"#/m2.")
    
    build_veg <- paste0(btn,'_', veg_start, "_", veg_end, "_", veg_cd,
                        "_",veg_height,"_",veg_diam,"_",veg_density)
    
    insertUI(
      selector = '#veg_patch_placeholder',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        build_str,
        id = id
      )
    )
    ltre_inserted <<- c(id, ltre_inserted)
    ltre_obj <<- c(ltre_obj, build_str)
    veg_patches <<- c(veg_patches, build_veg)
    print(veg_patches)
    print(length(ltre_obj))
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', ltre_inserted[length(ltre_inserted)])
    )
    ltre_inserted <<- ltre_inserted[-length(ltre_inserted)]
    ltre_obj <<- ltre_obj[-length(ltre_obj)]
    veg_patches <<- veg_patches[-length(veg_patches)]
  })
  
  
  

  
}