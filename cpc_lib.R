# library of functions to download and analyze CPC precipitation data

# precipitation data from the Climate Prediction Center (CPC)
# ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/
# URL for the individual files is not exactly the same and changes from 
# CPC's retrospective analyses (< 2006) to real-time analyses (> 2006)
# below are example URLs, xxx = ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB
# xxx/V1.0/1979/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.19790101.gz
# xxx/RT/2006/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.20060101RT.gz
# xxx/RT/2007/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.20070101.RT.gz
# xxx/RT/2009/PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx.20090101.RT
#
# also file format is gzipped prior to 2008 and plain binary after 2008

#-------------------------------------------------------------------------------
# CPC data dimensions, from PRCP_CU_GAUGE_V1.0GLB_0.50deg_README.txt
cpcNumLat   <- 360 # number of lats
cpcNumLon   <- 720 # number of lons
cpcNumBytes <- cpcNumLat * cpcNumLon * 2 # 2 fields, precipitation and num gages
cpcRes      <- 0.5 # data resolution
cpcLatVec   <- -89.75 + (1:cpcNumLat)*cpcRes - cpcRes # latitudes
cpcLonVec   <- 0.25 + (1:cpcNumLon)*cpcRes - cpcRes # longitudes

#-------------------------------------------------------------------------------
# identify the file name quirks for each year
FnGetFileNameQuirks <- function(yr) {
  
  if (yr %in% seq(1979, 2005)) {
    urlTag  <- "V1.0/"
    fileTag <- ".gz"
  } else if (yr %in% c(2006)) {
    urlTag  <- "RT/"
    fileTag <- "RT.gz"
  } else if (yr %in% c(2007, 2008)) {
    urlTag  <- "RT/"
    fileTag <- ".RT.gz"
  } else if (yr %in% seq(2009, 2012)) {
    urlTag  <- "RT/"
    fileTag <- ".RT"
  } else {
    stop("year out of bounds! check!")
  }
  
  return(list(urlTag=urlTag, fileTag=fileTag))
}

#-------------------------------------------------------------------------------
# function to download single CPC data file from their ftp site
Fn_Download_CPC_Data_OneDay <- function(yr, mo, day) {
  
  # check year and month validity
  if (!(yr %in% seq(1979, 2012) & mo %in% seq(1, 12))) {
    stop("year should be between 1979 to 2012, and month should be between 1 to 12!")
  }
  # check day validity
  # for now leave it up to the user
  
  # url and file prefixes
  urlHead  <- "ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/"
  fileHead <- "PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx."
  
  # identify the file name quirks for each year
  quirks <- FnGetFileNameQuirks(yr)
    
  # date in the format yyyy-mm-dd
  dateLong <- as.Date(paste(yr, mo, day, sep="-")) 
  # date string used in the filenames below
  dateStr  <- paste0(substr(dateLong, 1, 4), 
                     substr(dateLong, 6, 7), 
                     substr(dateLong, 9, 10))
  
  # construct url
  fileUrl <- paste0(urlHead, 
                    quirks$urlTag, 
                    yr, 
                    "/", 
                    fileHead, 
                    dateStr, 
                    quirks$fileTag)
  
  # out file name; gzipped file prior to 2008 otherwise binary
  outFile <- ifelse(yr <= 2008, 
                    paste0("raw_", dateStr, ".gz"), 
                    paste0("raw_", dateStr, ".bin"))
  # download
  if (yr <= 2008) {
    download.file(url=fileUrl, destfile=outFile)
  } else {
    download.file(url=fileUrl, destfile=outFile, mode="wb")
  }

}


#-------------------------------------------------------------------------------
# function to download multiple CPC data files from their ftp site
Fn_Download_CPC_Data_ManyDays <- function(begYr, begMo, begDay, 
                                          endYr, endMo, endDay) {
  
  # check year and month validity
  if (!(begYr %in% seq(1979, 2012) & endYr %in% seq(1979, 2012) & 
          begMo %in% seq(1, 12) & endMo %in% seq(1, 12))) {
    stop("year should be between 1979 to 2012, and month should be between 1 to 12!")
  }
  # check day validity
  # for now leave it up to the user
  
  # url and file prefixes
  urlHead  <- "ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/"
  fileHead <- "PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx."
  
  for (eachYr in begYr:endYr) {
    # generate dates of all the days in the format yyyy-mm-dd
    allDates <- seq(as.Date(paste(eachYr, begMo, begDay, sep="-")), 
                    as.Date(paste(eachYr, endMo, endDay, sep="-")), 
                    by="day")
    # date string used in the filenames below
    dateStr  <- paste0(substr(allDates, 1, 4), 
                       substr(allDates, 6, 7), 
                       substr(allDates, 9, 10))
    yrStr    <- substr(allDates, 1, 4)

    # identify the file name quirks for each year
    quirks <- FnGetFileNameQuirks(eachYr)
    
    # download
    for (eachDay in 1:length(allDates)) {
      # construct url
      fileUrl <- paste0(urlHead, 
                       quirks$urlTag, 
                       yrStr[eachDay], 
                       "/", 
                       fileHead, 
                       dateStr[eachDay], 
                       quirks$fileTag)

      # out file name; gzipped file prior to 2008 otherwise binary
      outFile <- ifelse(eachYr <= 2008, 
                        paste0("raw_", dateStr[eachDay], ".gz"), 
                        paste0("raw_", dateStr[eachDay], ".bin")) 
      
      # internet connection could be "intermittent" - could be due to the CPC server?
      # hence files are not sometimes downloaded; hence the quieted while loop below
      fileError <- TRUE
      while (fileError) {
        if (eachYr <= 2008) {
          fileStatus <- try(download.file(url=fileUrl, destfile=outFile, quiet=TRUE), silent=TRUE)
        } else {
          fileStatus <- try(download.file(url=fileUrl, destfile=outFile, mode="wb", quiet=TRUE), silent=TRUE)    
        }
        fileError  <- ifelse(class(fileStatus) == "try-error", TRUE, FALSE)
      }
            
    }
  }
}

#-------------------------------------------------------------------------------
# function to read downloaded CPC data
Fn_Read_CPC_RawData <- function(yr, mo, day) {

  # file name
  dateStr <- paste0(yr, sprintf("%.2d", mo), sprintf("%.2d", day))
  if (yr <= 2008) {
    cpcFile <- paste0("raw_", dateStr, ".gz")
  } else {
    cpcFile <- paste0("raw_", dateStr, ".bin")
  }
  stopifnot(file.exists(cpcFile))
  
  # open file connection
  if (yr <= 2008) {
    fileCon <- gzcon(file(cpcFile, "rb"))
  } else {
    fileCon <- file(cpcFile, "rb")
  }
  
  # read data
  inData  <- readBin(con = fileCon, 
                     what = numeric(), 
                     n = cpcNumBytes, 
                     size = 4, 
                     endian = "little")
  close(fileCon)
  
  # extract precipitation (first field), ignore second field (num gages)
  inData <- inData[1:(cpcNumBytes/2)]
  
  # reshape, flip rows for proper North-South orientation
  # original data goes from South to North
  prcpData <- array(0, dim=c(cpcNumLat, cpcNumLon))
  for(eachRow in 1:cpcNumLat) {
    index1 <- 1 + (eachRow-1)*cpcNumLon
    index2 <- eachRow * cpcNumLon
#     prcpData[cpcNumLat-eachRow+1, ] <- inData[index1:index2]
    prcpData[eachRow, ] <- inData[index1:index2]
  }
  # remove -ve (missing) values
  prcpData[prcpData < 0] <- NA
  # convert tenths of mm to mm
  prcpData <- ifelse(prcpData > 0, prcpData*0.1, prcpData)
  
  # write data to file
  outCon <- file(paste0(dateStr, ".bin"), "wb")
  writeBin(as.numeric(prcpData), 
           con = outCon, 
           size = 4, 
           endian = "little")
  close(outCon)
    
  return (prcpData)
}

#-------------------------------------------------------------------------------
# function to flip a matrix upside down (change CPC orientation from S-N to N-S)
Fn_Flip_Matrix_Rows <- function(mat) {
  return (mat[nrow(mat):1,])
}

#-------------------------------------------------------------------------------
# function to rotate a matrix 90 degress clockwise for plotting only
# used to counteract the "image" function default behavior
Fn_Rotate_Matrix <- function(mat) {
  return (t(mat)[,nrow(mat):1])
}

#-------------------------------------------------------------------------------
# function to take global data and extract a regional subset and then convert
# it a form used by ggplot
Fn_Get_Region_Data_For_Plot <- function(inMat, maxLat, minLat, maxLon, minLon) {
  # row/col corresp to the region
  rowId <- which(cpcLatVec <= maxLat & cpcLatVec >= minLat)
  colId <- which(cpcLonVec <= (maxLon + 360) & cpcLonVec >= (minLon + 360))
  
  # subset for the region
  outMat <- inMat[rowId, colId]
  #lat-lon matrices for reference
  latMat <- matrix(rep(cpcLatVec[rowId], length(colId)), nrow = length(rowId))
  lonMat <- t(matrix(rep(cpcLonVec[colId], length(rowId)), ncol = length(rowId)))
  
  # melt and add lat-lon info
  outDf <- cbind(melt(outMat), melt(latMat), melt(lonMat))
  # discard irrelvant columns after cbind
  outDf <- outDf[, c(3, 6, 9)]
  colnames(outDf) <- c("value", "lat", "lon")
  outDf$lon <- outDf$lon - 360
  
  # add color scale info
  outDf$colorScale <- cut(outDf$value, 
                          breaks = seq(0, 150, 25), 
                          labels = c("0", "25", "50", "75", "100", "125"),
                          include.lowest=TRUE)
  
  return (outDf)
}
