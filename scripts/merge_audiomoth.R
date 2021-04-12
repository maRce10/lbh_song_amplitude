merge_audiomoth <- function(path, dest.path, date, ID = NULL, location = NULL, start.time, clip.dur = 20, cl = 1){ 

  # error messages
  if(!dir.exists(dest.path)) stop("dest.path doesn't exist")
  
  if(!dir.exists(path)) stop("path doesn't exist")
  
  # load package
  library(chron)
    
  dt_tms <- chron(dates = date,
                  times = start.time, format = c(dates = "day mon year", times = "h:m:s")) 
  
  # list waves
  wvs <- list.files(path = path, full.names = TRUE, pattern = "\\.wav$", ignore.case = TRUE)
  
  #file size
  fs <- file.size(wvs)
  
  # remove empty files
  if(any(fs == 0))
  wvs <- wvs[fs > 0]
  
  # order by creation time
  wvs <- wvs[order(file.mtime(wvs))]
  
  # split in 20 min groups
  l_wvs <- split(x = wvs, f = rep(1:100, each = 20)[1:length(wvs)])
  
  # get duration
  drs <- sapply(l_wvs, length)
  
  # calculate start of each new 20 min clip
  for(i in 2:length(drs))
    dt_tms[i] <- dt_tms[i - 1] + times((drs[i - 1])/(24 * 60)) 
  
  # put track info in data frame
  df <- data.frame(drs, dt_tms)
  
  # extract date
  df$date <- gsub(" ", "-", substr(df$dt_tms, 2, 12))
  
  # start time
  df$tm_start <- substr(df$dt_tms, 14, 18)
  
  # end time
  df$tm_end <- df$dt_tms + times((df$drs)/(24 * 60))
  
  df$tm_end <- substr(df$tm_end, 14, 18)
  
  
  # name list of track groups
  names(l_wvs) <- paste(ID, location, df$date, df$tm_start, paste0(df$tm_end, ".wav"), sep = "_")
  
  # remove semicolom
  names(l_wvs) <- gsub(":", ".", names(l_wvs), fixed = TRUE)
  
  # concatenate, rename and save
  out <- pbapply::pblapply(1:length(l_wvs), cl = cl, function(x) {
    
    wvs <- l_wvs[[x]]
    
    fll_wv <- readWave(wvs[1])
    
    for(i in 2:length(wvs))
      fll_wv <- pastew(tuneR::readWave(wvs[i]), fll_wv, output = "Wave")
    
    tuneR::writeWave(fll_wv, filename = file.path(dest.path, names(l_wvs)[x]), extensible = FALSE)
  }
  )  
  
  
  warbleR::wav_dur(path = dest.path)

  }

