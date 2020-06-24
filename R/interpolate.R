function (setup, method = "l") 
{
  AP <- roundAP(c(setup$first_AP, setup$internal_ref_AP, setup$last_AP))
  z <- c(setup$first_z, setup$internal_ref_z, setup$last_z)
  
  #Added if statement to get APs of all atlas plates between first and last AP specified
  if(is.numeric(setup$regi_step) == FALSE){
  AP_interp <- atlasIndex$mm.from.bregma[which(atlasIndex$mm.from.bregma==setup$first_AP):which(atlasIndex$mm.from.bregma==setup$last_AP)]
  }else{
  AP_interp <- roundAP(seq(setup$first_AP, setup$last_AP, by = -setup$regi_step))
  }
  AP_interp <- unique(AP_interp[!(AP_interp %in% AP)])
  if (method == "l") {
    my_function <- approxfun(AP, z, method = "linear")
    z_interp <- round(my_function(AP_interp))
  }
  else if (method == "s") {
    my_function <- splinefun(AP, z, method = "monoH.FC")
    z_interp <- round(my_function(AP_interp))
  }
  else {
    stop("Not a valid argument for 'method'")
  }
  setup$regi_AP <- sort(c(AP_interp, setup$internal_ref_AP, 
                          setup$first_AP, setup$last_AP), decreasing = TRUE)
  setup$regi_z <- sort(c(z_interp, setup$internal_ref_z, setup$first_z, 
                         setup$last_z))
  return(setup)
}
