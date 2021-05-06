# Export wave profiles with all data
ExportProfiles <- function(
  fo = NA,
  path_output = NA,
  wave_dat = NA,
  export_csv = TRUE
) {
  
  
  tdat_all = fo$transect_data
  uids <- unique(wave_dat$line_id)
  
  
  
  for(i in 1:length(uids)){
    
    this_id <- uids[i]
    
    twave <- wave_dat[which(wave_dat$line_id == this_id),]
    tdat <- tdat_all[which(tdat_all$line_id == this_id),]
    
    if(nrow(twave) ==0){
      next
    }
    
    st_geometry(twave) <- NULL
    twave <- twave[,c("Xpos", "Eta","Etas","Ubot","H_veg","H_noveg")]
    twave[is.na(twave)] <- 0
    
    
    mdat <- merge(tdat, twave, by.x="Xpos", by.y="Xpos", all.x=TRUE, all.y=FALSE)
    mdat$Eta[is.na(mdat$Eta)] <- 0
    mdat$Etas[is.na(mdat$Etas)] <- 0
    mdat$Ubot[is.na(mdat$Ubot)] <- 0
    mdat$H_veg[is.na(mdat$H_veg)] <- 0
    mdat$H_noveg[is.na(mdat$H_noveg)] <- 0
    
    
    
    # FOr QA only
    if(FALSE){
      par(mfrow=c(2,1), mar=c(2,4,2,0))
      plot(mdat$Xpos, mdat$H_veg, type='l', xlim=c(min(mdat$Xpos), max(mdat$Xpos)))
      plot(mdat$Xpos, mdat$elev, type='l', xlim=c(min(mdat$Xpos), max(mdat$Xpos)), main=this_id)
      #Sys.sleep(1)
    }
    
    
    if(export_csv){
    write.csv(mdat,
              file=paste0(path_output,
                          '/www/data/profile_csv/',
                          'profile_', this_id, '.csv'),
              row.names = FALSE)
      
    } else {
      return(mdat)
    }
    
    print(i)
    
  } # end of loop
  
} # end of function