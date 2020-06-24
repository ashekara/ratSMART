function (setup = NULL) 
{
  user_prompt <- vector(mode = "list", length = 13)
  user_prompt[[1]] <- "1) Enter your Animal ID: \n"
  user_prompt[[2]] <- "2) Enter your Initials: \n"
  user_prompt[[3]] <- paste0("3) Enter the path to the registration channel folder: ", 
                             "\n No need to enter quotes.\n")
  user_prompt[[4]] <- paste0("4) Enter the path to the segmentation channel folder: ", 
                             "\n No need to enter quotes.\n")
  user_prompt[[5]] <- paste0("5) Enter the path to output folder. If it doesn't exist, please create it: ", 
                             "\n No need to enter quotes.\n")
  user_prompt[[6]] <- "6) What is your spacing (mm) between adjacent z images? \n"
  user_prompt[[7]] <- "7) What is your spacing between registrations (mm)\n"
  user_prompt[[8]] <- "8) What is your segmentation step size (integer)?\n"
  user_prompt[[9]] <- paste0("9) What is your most anterior AP coordinate (mm)? ", 
                             "\nPlease enter at least 2 decimal digits.\n")
  user_prompt[[10]] <- paste0("10) What is the z image number corresponding to most anterior AP coordinate? ", 
                              "\nPlease enter an integer number.\n")
  user_prompt[[11]] <- paste0("11) What is your most posterior AP coordinate (mm)? ", 
                              "\nPlease enter at least 2 decimal digits.\n")
  user_prompt[[12]] <- paste0("12) What is the z image number corresponding to most posterior AP coordinate? ", 
                              "\nPlease enter an integer number.\n")
  user_prompt[[13]] <- paste0("13) What are your internal reference coordinates? ", 
                              "\nNOTE: The default coordinates are 1.91, 1.10, -0.42, -0.93 , -1.94 , -2.95, -3.96", 
                              "\nThey correspond to PFC, NAc, antHyp, start of Hip, posHyp, VTA, PAG.\n")
  user_prompt2 <- vector(mode = "list", length = 7)
  user_prompt2[[1]] <- "1) Enter your Animal ID: \n"
  user_prompt2[[2]] <- "2) Enter your Initials: \n"
  user_prompt2[[3]] <- paste0("3) Enter the path to the registration channel folder: ", 
                              "\n No need to enter quotes.\n")
  user_prompt2[[4]] <- paste0("4) Enter the path to the segmentation channel folder: ", 
                              "\n No need to enter quotes.\n")
  user_prompt2[[5]] <- paste0("5) Enter the path to output folder. If it doesn't exist, please create it: ", 
                              "\n No need to enter quotes.\n")
  user_prompt2[[6]] <- paste0("6) Enter all known AP coordinates in order from anterior to posterior (mm): ", 
                              "\nPlease enter at least 2 decimal digits per coordinate and separate values by a ','.\n")
  user_prompt2[[7]] <- paste0("7) Enter all z image number in order corresponding to entered AP coordinates: ", 
                              "\nPlease enter integer numbers and separate values by a ','.\n")
  if (!exists(deparse(substitute(setup)))) {
    done <- FALSE
    while (!done) {
      inp <- readline(paste0("Do you want to register a partial brain (P) or a whole brain (W): P/W? ", 
                             "\nNote: partial brains must have KNOWN z and AP values!\n"))
      if (inp == "P" || inp == "p") {
        setup <- vector(mode = "list", length = 9)
        setup[[1]] <- readline(user_prompt2[[1]])
        setup[[2]] <- readline(user_prompt2[[2]])
        setup[[3]] <- convertpath(readline(user_prompt2[[3]]))
        setup[[4]] <- convertpath(readline(user_prompt2[[4]]))
        setup[[5]] <- convertpath(readline(user_prompt2[[5]]))
        setup[[6]] <- readline(user_prompt2[[6]])
        setup[[6]] <- roundAP(as.numeric(unlist(strsplit(setup[[6]], 
                                                         ","))))
        setup[[7]] <- readline(user_prompt2[[7]])
        setup[[7]] <- round(as.numeric(unlist(strsplit(setup[[7]], 
                                                       ","))))
        names(setup) <- c("anim_ID", "user_init", "regi_channel", 
                          "seg_channel", "output", "regi_AP", "regi_z", 
                          "savepaths", "image_paths")
        done <- TRUE
      }
      else if (inp == "W" || inp == "w") {
        setup <- vector(mode = "list", length = 18)
        setup[[7]] <- "Default (All plates between first and last AP)"   #changed from 0.1 in original SMART
        setup[[8]] <- 1
        setup[[13]] <- roundAP(c(1.91, 1.1, -0.42, -0.93, 
                                 -1.94, -2.95, -3.96))
        setup[[1]] <- readline(user_prompt[[1]])
        setup[[2]] <- readline(user_prompt[[2]])
        setup[[3]] <- convertpath(readline(user_prompt[[3]]))
        setup[[4]] <- convertpath(readline(user_prompt[[4]]))
        setup[[5]] <- convertpath(readline(user_prompt[[5]]))
        setup[[6]] <- as.numeric(readline(user_prompt[[6]]))
        setup[[9]] <- roundAP(as.numeric(readline(user_prompt[[9]])))
        setup[[10]] <- as.numeric(readline(user_prompt[[10]]))
        setup[[11]] <- roundAP(as.numeric(readline(user_prompt[[11]])))
        setup[[12]] <- as.numeric(readline(user_prompt[[12]]))
        names(setup) <- c("anim_ID", "user_init", "regi_channel", 
                          "seg_channel", "output", "z_space", "regi_step", 
                          "seg_step", "first_AP", "first_z", "last_AP", 
                          "last_z", "internal_ref_AP", "internal_ref_z", 
                          "regi_AP", "regi_z", "savepaths", "image_paths")
        done <- TRUE
      }
    }
  }
  if (length(setup) == 9) {
    change_done <- FALSE
    while (!change_done) {
      cat("\nYour animal ID         : ", setup$anim_ID)
      cat("\nYour initials          : ", setup$user_init)
      cat("\nYour registration path : ", setup$regi_channel)
      cat("\nYour segmentation path : ", setup$seg_channel)
      cat("\nYour output path       : ", setup$output)
      cat("\nYour AP coordinates    : ", round(setup$regi_AP, 
                                               digits = 2))
      cat("\nYour z numbers         : ", setup$regi_z)
      cat("\nPlease review your setup information above: ")
      inp <- readline("Do you want to change any settings: Y/N?")
      if (inp == "Y" || inp == "y") {
        cat("\n1) Animal ID", "\n2) User's Initials", 
            "\n3) Registration channel", "\n4) Segmentation channel", 
            "\n5) Output folder", "\n6) AP coordinates (mm)", 
            "\n7) z numbers\n")
        num_done <- FALSE
        while (!num_done) {
          pts <- readline("Enter the number(s) of the setting(s) you want to change: ")
          pts <- unlist(strsplit(pts, ","))
          pts_col <- grep(":", pts, value = TRUE)
          pts_sing <- grep(":", pts, value = TRUE, invert = TRUE)
          if (length(pts_col) != 0) {
            colvec <- c()
            flag <- FALSE
            for (p in 1:length(pts_col)) {
              suppressWarnings(curvec <- as.integer(unlist(strsplit(pts_col[p], 
                                                                    ":"))))
              if (sum(is.na(curvec)) > 0) {
                flag <- TRUE
                break
              }
              else {
                colvec <- c(colvec, curvec[1]:curvec[2])
              }
            }
            pts_sing <- suppressWarnings(as.integer(pts_sing))
            pts <- c(colvec, pts_sing)
            suppressWarnings(if (sum(is.na(pts)) == 
                                 0 & flag == FALSE) {
              if (sum(pts > 7 | pts < 1) < 1) {
                pts <- sort(pts)
                num_done <- TRUE
              }
            })
          }
          else if (length(pts_sing) != 0) {
            pts <- suppressWarnings(as.integer(pts_sing))
            suppressWarnings(if (!sum(is.na(pts))) {
              if (sum(pts > 7 | pts < 1) < 1) {
                pts <- sort(pts)
                num_done <- TRUE
              }
            })
          }
        }
        for (s in pts) {
          if (sum(s == 1:2) >= 1) {
            setup[[s]] <- readline(user_prompt2[[s]])
          }
          else if (sum(s == 3:5) >= 1) {
            setup[[s]] <- convertpath(readline(user_prompt2[[s]]))
          }
          else if (s == 6) {
            setup[[s]] <- readline(user_prompt2[[s]])
            setup[[s]] <- roundAP(as.numeric(unlist(strsplit(setup[[s]], 
                                                             ","))))
          }
          else if (s == 7) {
            setup[[s]] <- readline(user_prompt2[[s]])
            setup[[s]] <- round(as.numeric(unlist(strsplit(setup[[s]], 
                                                           ","))), digits = 0)
          }
        }
      }
      else if (inp == "N" || inp == "n") {
        change_done <- TRUE
      }
    }
  }
  else {
    change_done <- FALSE
    while (!change_done) {
      cat("\nYour animal ID         : ", setup$anim_ID)
      cat("\nYour initials          : ", setup$user_init)
      cat("\nYour registration path : ", setup$regi_channel)
      cat("\nYour segmentation path : ", setup$seg_channel)
      cat("\nYour output path       : ", setup$output)
      cat("\nYour z spacing         : ", setup$z_space)
      cat("\nYour registration step : ", setup$regi_step)
      cat("\nYour segmentation step : ", setup$seg_step)
      cat("\nYour first AP          : ", round(setup$first_AP, 
                                               digits = 2))
      cat("\nYour first z           : ", setup$first_z)
      cat("\nYour last AP           : ", round(setup$last_AP, 
                                               digits = 2))
      cat("\nYour last z            : ", setup$last_z)
      cat("\nYour internal reference AP coordinates: ", 
          round(setup$internal_ref_AP, digits = 2))
      cat("\nPlease review your setup information above: ")
      inp <- readline("Do you want to change any settings: Y/N?")
      if (inp == "Y" || inp == "y") {
        cat("\n1) Animal ID", "\n2) User's Initials", 
            "\n3) Registration channel", "\n4) Segmentation channel", 
            "\n5) Output folder", "\n6) Z slice spacing (mm)", 
            "\n7) Registration step size (mm)", "\n8) Segmentation step size (integer)", 
            "\n9) First AP", "\n10) First z", "\n11) Last AP", 
            "\n12) Last z", "\n13) Internal reference AP coordinates\n")
        num_done <- FALSE
        while (!num_done) {
          pts <- readline("Enter the number(s) of the setting(s) you want to change: ")
          pts <- unlist(strsplit(pts, ","))
          pts_col <- grep(":", pts, value = TRUE)
          pts_sing <- grep(":", pts, value = TRUE, invert = TRUE)
          if (length(pts_col) != 0) {
            colvec <- c()
            flag <- FALSE
            for (p in 1:length(pts_col)) {
              suppressWarnings(curvec <- as.integer(unlist(strsplit(pts_col[p], 
                                                                    ":"))))
              if (sum(is.na(curvec)) > 0) {
                flag <- TRUE
                break
              }
              else {
                colvec <- c(colvec, curvec[1]:curvec[2])
              }
            }
            pts_sing <- suppressWarnings(as.integer(pts_sing))
            pts <- c(colvec, pts_sing)
            suppressWarnings(if (sum(is.na(pts)) == 
                                 0 & flag == FALSE) {
              if (sum(pts > 13 | pts < 1) < 1) {
                pts <- sort(pts)
                num_done <- TRUE
              }
            })
          }
          else if (length(pts_sing) != 0) {
            pts <- suppressWarnings(as.integer(pts_sing))
            suppressWarnings(if (!sum(is.na(pts))) {
              if (sum(pts > 13 | pts < 1) < 1) {
                pts <- sort(pts)
                num_done <- TRUE
              }
            })
          }
        }
        for (s in pts) {
          if (sum(s == 1:2) >= 1) {
            setup[[s]] <- readline(user_prompt[[s]])
          }
          else if (sum(s == 3:5) >= 1) {
            setup[[s]] <- convertpath(readline(user_prompt[[s]]))
          }
          else if (s == 9 | s == 11) {
            setup[[s]] <- roundAP(as.numeric(readline(user_prompt[[s]])))
          }
          # Adding else if statement here to change registration step from default value
          else if(s == 7){
            setup[[s]] <- as.numeric(readline(user_prompt[[s]]))
            if (is.na(setup[[s]]) == TRUE){
              setup[[s]] <- "Default (All plates between first and last AP)"
            }
          }
          else if (s == 10 | s == 12) {
            setup[[s]] <- round(as.numeric(readline(user_prompt[[s]])), 
                                digits = 0)
          }
          else if (s == 13) {
            setup[[s]] <- readline(user_prompt[[s]])
            setup[[s]] <- roundAP(sort(as.numeric(unlist(strsplit(setup[[s]], 
                                                                  ","))), decreasing = TRUE))
          }
          else {
            setup[[s]] <- as.numeric(readline(user_prompt[[s]]))
          }
        }
      }
      else if (inp == "N" || inp == "n") {
        change_done <- TRUE
      }
    }
  }
  setwd(setup$output)
  return(setup)
}
