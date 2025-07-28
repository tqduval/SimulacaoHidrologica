fun_run_hymod <- function(par, df_in, area, par_limits, warmup, valid, optim = "nm"){
  
  # Filter data based on df_in data
  date_min <- min(df_in$date)
  date_max <- max(df_in$date)
  calib_start <- date_min + years(warmup)
  calib_end <- date_max - years(valid)
  df_in_calib <- df_in[df_in$date > calib_start & df_in$date <= calib_end,] # calibration data
  df_in_valid <- df_in[df_in$date >= calib_end,]                            # validation data
  
  # Objective function
  run <- function(par, df_in, area){
    ep <- df_in[, c(3,2)]
    qobs <- df_in[["q_obs"]]
    qsim <- HyMod(x = par, EPQ = ep)*area*1e6/(1e3*86400) # run hymod
    res <- cbind(df_in, qsim)                             # join into same table
    qsim <- res$qsim
    qobs <- res$q_obs
    nse <- fun_nse(obs = qobs[!is.na(qobs)], sim = qsim[!is.na(qobs)]) # get nse
    return(-nse)                                                       # invert for minimazing
  }
  
  # Calibradores
  min <- par_limits[[2]]; max <- par_limits[[3]]
  
  # Escolher calibrador
  if(optim == "nm"){
    
    # Optim (Nelder-Mead)
    calib <- optim(par = par_in,
                   method = "Nelder-Mead",
                   fn = run,
                   df_in = df_in_calib,
                   area = area)
    
  } else if(optim == "sce"){
    
    # Suffle Complex Evolution (SCE)
    calib <- SoilHyP::SCEoptim(FUN = run, par = par_in,
                               lower = min, upper = max,
                               df_in = df_in_calib,
                               area = area,
                               control = list(maxit = 20,
                                              reltol = 0.001,
                                              ncomplex = 3,
                                              tolsteps = 6,
                                              trace = 1))
    
  }
  
  # Run HyMod for validation period
  par_final <- calib[["par"]]
  nse_valid <- -run(par_final, df_in_valid, area)
  qsim <- HyMod(par_final, df_in[, c(3,2)])*area*1e6/(1e3*86400)
  df_final <- cbind(df_in, qsim)
  
  res <- list("optim" = optim,
              "warmup" = warmup,
              "calibration" = list(nse = -calib[["value"]], par = par_final),
              "validation" = nse_valid,
              "results" = df_final,
              "optimization" = calib)
  
  return(res)
  
}