FloodPlot <- function(
  dat = NA,
  erosion = NA,
  mean_high_water = NA,
  path_output = NA,
  export_plot = FALSE,
  total_wsl_adj = NA,
  TrimOnshoreDist = FALSE
){
  
  uids <- unique(dat$line_id)
  eids <- unique(erosion$transect_id)
  
  exp_data <- list()
  exp_flood <- list()
  exp_flood_marks <- list()
  
  
  for(i in 1:length(uids)){
  
    this_id <- uids[i]
    # this_id = 17
      
    # Current profile
    dats <- dat[which(dat$line_id == this_id),]
    

    
    #plot(dats$elev, type='l')
    
    # Reverse Xpos to match style
    # dats$Xpos <- dats$Xpos
    
    
    # If a ridge filter out past max elev
    max_elev <- max(dats$elev, na.rm=TRUE)
    
    # Find ridge crest
    ds1 <- dats %>% filter(elev > 0) %>% 
      filter(abs(elev - max_elev) == min(abs(elev - max_elev))) %>% 
      dplyr::select(Xpos)
    x_peak <- ds1$Xpos[1]
    
    
    if(TrimOnshoreDist){
      # Filter past peak
      dats <- dats[which(dats$Xpos >= x_peak),] #adj
      
      # Filter anything beyond 200m Xpos horizontal distance
      dats <- dats[which(dats$Xpos >= -200),] #adj
      #plot(dats$elev, type='l')
    }

    
    # Filter out any land areas that are over a elevation of 30m
    elev_30 <- dats$Xpos[which(dats$elev > 30)]
    if(length(elev_30 > 0)){
      # Get closest shoreward distance
      ref <- max(elev_30)
      dats <- dats[which(dats$Xpos >= ref),] #adj
    }

    
    # Interpolate over any NA values
    interp_elev <- approx(x=dats$Xpos, y=dats$elev, xout=dats$Xpos)
    # replace with interpolated values
    dats$elev <- interp_elev$y
    #plot(interp_elev$x, interp_elev$y, type='l')
 
    
    
    
    
    
  
    #=========================================
    # Current runup 
    erosion_t <- erosion[which(erosion$transect_id == this_id),]
    
    if(nrow(erosion_t) == 0){
      ru_veg <- 0
      ru_noveg <- 0
      print("no erosion data")
      next
    } else {
      ru_veg <- erosion_t$runup_Veg
      ru_noveg <- erosion_t$runup_NoVeg
    }
    

    # total_wsl_adj
    total_wsl_adj_t <- total_wsl_adj + ru_noveg
    total_wsl_adj_t_veg <- total_wsl_adj + ru_veg
    
    
    myylim <- ceiling(total_wsl_adj_t) + 2
    
    # Get xlim for plot boundaries
    ds1 <- dats %>% filter(elev <0) %>% 
      mutate(b = abs(elev)) %>%
      filter(abs(b - 2) == min(abs(b - 2))) %>% 
      dplyr::select(Xpos)
    x1 <- ds1$Xpos[1]
   
    if(x1 >= 200){
        x1 <- 200
    }
    

    
    # Get xlim for plot boundaries
    ds1 <- dats %>% filter(elev >0) %>% 
      filter(abs(elev - myylim) == min(abs(elev - myylim))) %>% 
      dplyr::select(Xpos)
    x2 <- ds1$Xpos[1]
    
    
    
    
    
    
    #====================================================
    # Check flooding without vegetation
    #====================================================
    
    # Get flooded land
    fl <- dats %>% filter(elev >= mean_high_water & elev <= total_wsl_adj_t) 
    #plot(fl$Xpos, fl$elev)
    
    if(nrow(fl)>0){

      # Identify breakpoints
      xx <- fl$Xpos
      
      bps <- unname(tapply(xx, cumsum(c(1, diff(xx)) != 1), range))
      
      bp <- data.frame()
      for(b in 1:length(bps)){
        row <- bps[b][[1]]
        dist = abs(row[1] - row[2])
        addr <- data.frame(sec=b,from=row[1], to=row[2], dist=dist)
        bp <- rbind(addr, bp)
      }
      
      flood_dist <- sum(bp$dist, na.rm=TRUE)
      msub_title_noveg <- paste0(flood_dist, "m of shoreline flooded above high water (without Veg).")
      
    } else {
      flood_dist <- 0
      msub_title_noveg <- paste0(flood_dist, "m of shoreline flooded above high water (without Veg).")
      bp <- data.frame()
      
    }
    
    
    
    
    
    
    
    #====================================================
    # Check flooding with vegetation
    #====================================================
    
    # Get flooded land
    fl <- dats %>% filter(elev >= mean_high_water & elev <= total_wsl_adj_t_veg) 
    #plot(fl$Xpos, fl$elev)
    
    if(nrow(fl)>0){
      
      # Identify breakpoints
      xx <- fl$Xpos
      
      bps <- unname(tapply(xx, cumsum(c(1, diff(xx)) != 1), range))
      
      bpv <- data.frame()
      for(b in 1:length(bps)){
        row <- bps[b][[1]]
        dist = abs(row[1] - row[2])
        addr <- data.frame(sec=b,from=row[1], to=row[2], dist=dist)
        bpv <- rbind(addr, bpv)
      }
      
      flood_dist_veg <- sum(bpv$dist, na.rm=TRUE)
      msub_title_veg <- paste0(flood_dist_veg, "m of shoreline flooded above high water (with Veg).")
    } else {
      flood_dist_veg <- 0
      msub_title_veg <- paste0(flood_dist_veg, "m of shoreline flooded above high water (with Veg).")
      bpv <- data.frame()
      
    }
    
    
    
    msub_title <- paste0(msub_title_noveg, "\r\n", msub_title_veg)
    
    
    
    
    
    #=======================================
    # START PLOT FOR PROFILE
    #=======================================
    
 
    
    

    
    
    
    #=============================================
    #=============================================
    #=============================================
    dats$Type <- 'Eelgrass'
    exp_dat <- dats[,c("line_id", "Xpos", "elev", "elev_smooth",
                       "Type", "hc", "d", "N", "Cd")]
    longs <- as.data.frame(st_coordinates(st_transform(dats, 4326)))[,1]
    lats <- as.data.frame(st_coordinates(st_transform(dats, 4326)))[,2]
    exp_dat$latitude <- lats
    exp_dat$longitude <- longs
    st_geometry(exp_dat) <- NULL
    
    # write csv
    # write.csv(exp_dat,
    #           file=paste0(path_output,
    #                       '/www/data/profile_csv/',
    #                       'profile_', this_id, '.csv'),
    #           row.names = FALSE)
    # 
    
    # add flood data to other object
    
    flood_dat <- data.frame(line_id= this_id,
                            flood_dist=flood_dist,
                            flood_dist_veg=flood_dist_veg)
    
    flood_marks <- bp
    
    
    
    exp_data[[i]] <- exp_dat
    exp_flood[[i]] <- flood_dat
    exp_flood_marks[[i]] <- flood_marks
    
    
    
    print(this_id)
  
  }
  
  
  print("Merging Object")
  exp_data_out <- do.call("rbind", exp_data)
  exp_flood_out <- do.call("rbind", exp_flood)
  exp_flood_marks_out <- do.call("rbind", exp_flood_marks)
  
  ret_obj                <- list()
  ret_obj$transect_data  <- exp_data_out
  ret_obj$flood_data     <- exp_flood_out
  ret_obj$flood_marks    <- exp_flood_marks_out
  
  
  return(ret_obj)
  
  
}
