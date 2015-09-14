readSodar <- function(manufacturer = NULL,
                      file.type = NULL,
                      file.name = NULL,
                      tz = "GMT"){
  # see 
  if(manufacturer == "scintec"){
    mnd = read.scintec.mnd(file.name = file.name,
                           tz.in = tz)
  } else {
    printf('No read method for %s files from %s.',
           file.type,
           manufacturer)
  }
}

read.scintec.mnd <- function(file.name = NULL,
                             tz.in = "GMT"){
  # initalize the data fram
  data.out <- read.scintec.mnd.init()
  # read the file line-by-line
  conn = file(description = file.name, open="r")
  linn = readLines(conn)
  # work through the lines
  in_data = FALSE  
  for (i in 1:length(linn)){
    # look for the time stamp. Note that this is the end of the measurement period.
    # for grep help see http://www.endmemo.com/program/R/grep.php
    ts_found <- grepl(pattern="\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}\\s\\d{2}:\\d{2}:\\d{2}",
                      x = linn[i])
    if (ts_found){      
      t.raw <- unlist(strsplit(x = linn[i],
                               split = " "))
      t.end <- as.POSIXct(format(as.POSIXct(paste(t.raw[1], " ", t.raw[2]),
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = tz.in),
                                 tz = "GMT"),
                          tz = "GMT")
      t.duration <- as.POSIXct(t.raw[3], format = "%H:%M:%S", tz = "GMT") - 
        as.POSIXct(format(as.POSIXct(Sys.time(), tz = "GMT"), 
                          format = "%Y-%m-%d"))
      t.start <- t.end - t.duration
      print(sprintf('... found timestamp at %s', t.start))
      in_data = TRUE
    }
    if (in_data){
      # look for data
      data_found <- grepl(pattern = "\\s{2,}[0-9]{2,}\\s{2,}[.0-9]{2,}", 
                          x = linn[i])
      if (data_found){
        data.raw <- as.numeric(unlist(strsplit(x = gsub(pattern = "^\\s",
                                                        replacement = "",
                                                        x = gsub(pattern = "\\s{1,}",
                                                                 replacement = " ",
                                                                 x = linn[i])),
                                               split = " ")))
        data.in <- data.frame(t.start = t.start,
                              t.duration = t.duration,
                              t.end = t.end,
                              z = data.raw[1],
                              speed = data.raw[2],
                              dir = data.raw[3],
                              W = data.raw[4],
                              sigW = data.raw[5],
                              bck = data.raw[6],
                              error = data.raw[7])
        data.out <- rbind(data.out,
                          data.in)
      }
    }
  }
  close(conn)
  # clean the data
  data.out$speed[data.out$speed == 99.99] = NA
  data.out$dir[data.out$dir >= 999] = NA
  data.out$W[data.out$W == 99.99] = NA
  data.out$sigW[data.out$sigW == 99.99] = NA
  
  # and write it out
  return(data <- data.out)
}

read.scintec.mnd.init <- function(){
  # initalize the storage data frame
  data.out <- data.frame(t.start = as.Date(character()),
                         t.duration = as.Date(character()),
                         t.end = as.Date(character()),
                         z = numeric(),
                         speed = numeric(),
                         dir = numeric(),
                         W = numeric(),
                         sigW = numeric(),
                         bck = numeric(),
                         error = numeric())
}