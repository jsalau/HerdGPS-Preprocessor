##################################################################################
#PACKAGES
library(stringr)
library(tictoc)
library(ggplot2)
library(dlm)
library(futile.logger)
library(dplyr)
# Read and load configuration file.
Continue <- TRUE
bad_filtering <- FALSE
source(file.path(getwd(), "Config.R"))
if (!is.element("speed", tolower(header)) || !is.element("distance", tolower(header)) || !is.element("local.date", tolower(header)) || !is.element("local.time", tolower(header))) { Continue <- FALSE }
if (!Continue)
{
  flog.error("Cannot proceed: The data files need to contain the variables 'local.date', 'local.time', 'speed', and 'distance'.\nPlease rename the columns that hold the date and time variables.", name = 'HerdGPS')
  cat("\nCannot proceed: The data files need to contain the variables 'local.date', 'local.time', 'speed', and 'distance'.\nPlease rename the columns that hold the date and time variables.")
} else {
  flog.trace("Loading functions from 'Utils.R'.", name = 'HerdGPS')
  source(file.path(getwd(), "Utils.R"))
  d2m <- js_PositionTransform_degree2meter(ref_point_1, ref_point_2, reference_distance)
  #
  if ( calculate_list_of_squares )
  {
    flog.trace("'calculate_list_of_squares' was set TRUE.\nLoading GPS data of the boundary.", name = 'HerdGPS')
    # Load GPS data of boundary
    boundary <- read.csv(file.path(ppath, name_of_boundary_file), sep = ",", header = TRUE)
    names(boundary) <- tolower(names(boundary))
    # Filtering boundary data
    boundary <- js_PraeFiltering(boundary, end_of_the_world_N, end_of_the_world_S, end_of_the_world_E, end_of_the_world_W, max_speed_boundary, max_dist_boundary)
    # Filling holes in boundary data
    boundary <- js_RepairBoundaryData(boundary, side_length, d2m)
    # Stores repaired boundary data
    save(boundary, file = file.path(ppath, "RepairedBoundary.RData"))
    #######
    # Calculate grid in the first quadrant relativ to the given origin
    flog.trace("Calculating grid.", name = 'HerdGPS')
    grid <- js_MakeGrid_1stQuadrant(origin, side_length, total_width, d2m)
    # SPecify grid square that lie on the boundary
    flog.trace("Specifying grid squares on the boundary.", name = 'HerdGPS')
    output <- js_BoundarySquares(boundary, grid, FALSE, ppath)
    boundary_squares <- output$boundary_squares
    # Store boundary squares as Rdata.
    save(boundary_squares, file = file.path(ppath, paste("BoundarySquares_GridSidelength", as.character(side_length) ,".RData", sep = "") ))
    #######
    # Reduce grid to squares that lie within the boundary
    flog.trace("Reducing grid to squares within the boundary.", name = 'HerdGPS')
    output <- js_AreaSquares(InnerPoints, grid, boundary_squares)
    area_squares <- output$area_squares
    save(area_squares, file = file.path(ppath, paste("AreaSquares_GridSidelength", as.character(side_length) ,".RData", sep = "") ))
    #
    list_of_squares <- output$area_squares_names#union(area_squares_names, boundary_squares_names)
    save(list_of_squares, file = file.path(ppath, paste("ListOfCoveringSquares_Sidelength", as.character(side_length) ,".RData", sep = "") ))
    remove(output, boundary_squares)
    flog.trace("List of squares covering the area of observation was specified.", name = 'HerdGPS')
  } else  {
    # Calculate grid in the first quadrant relativ to the given origin
    flog.trace("Calculating grid.", name = 'HerdGPS')
    grid <- js_MakeGrid_1stQuadrant(origin, side_length, total_width, d2m)
    load(file = file.path(ppath, paste("ListOfCoveringSquares_Sidelength", as.character(side_length) ,".RData", sep = "") )) #list_of_squares
  }
  ##################################################################################
  #
  ####################################################################
  #Working with GPS data:
  ####################################################################
  #
  flog.trace(paste("Listing all raw data GPS files in given path: ", ppath, sep = ""), name = 'HerdGPS')
  list_of_files <- dir(path = ppath, pattern = ".csv", all.files = FALSE, full.names = FALSE, recursive = TRUE, ignore.case = FALSE)#, include.dirs = FALSE, no.. = FALSE)
  #
  ####################################################################
  tic.clearlog()
  tic("Total processing time")
  #
  flog.trace("Creating folder structure for the output files.", name = 'HerdGPS')
  dir.create(file.path(ppath, "Contact_data"), showWarnings = FALSE)
  dir.create(file.path(ppath, paste("Day_datafiles_", systemtime, sep = "")), showWarnings = FALSE)
  dir.create(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "Rdata"), showWarnings = FALSE)
  dir.create(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats"), showWarnings = FALSE)
  dir.create(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "Rdata"), showWarnings = FALSE)
  dir.create(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization"), showWarnings = FALSE)
  tic("Processing time for splitting up data into day files")
  #
  #start reading data files successively and splitting the data up into day files
  flog.trace("Looping over the raw data GPS files:", name = 'HerdGPS')
  for (li in seq(1, length(list_of_files)))
  {
    datafile_nme <- list_of_files[[li]]
    flog.trace(paste("------------------------------------------------\nProcessing file: ", datafile_nme, sep = ""), name = 'HerdGPS')
    tic(paste("Total processing time for file ", datafile_nme, sep = ""))
    #check filename
    flog.trace("Checking filename.", name = 'HerdGPS')
    file <- basename(datafile_nme)
	cat("\n")
    print(file)
    pos <- gregexpr('_', file)
    flog.trace("Trying to extract animal ID and sensor marker from filename.", name = 'HerdGPS')
    if (pos[[1]][1] == -1 || length(pos[[1]]) > 1)
    {
      animal_no <- c()
    } else {
      animal_no <- c()
      animal_no <- substr(file, pos[[1]][1] + 1, nchar(file) - 4)
      #
      marker <- substr(animal_no, nchar(animal_no), nchar(animal_no))
      animal_no <- substr(animal_no, 1, nchar(animal_no) - 1)
      if (is.element(substr(animal_no,1,1), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) && is.element(substr(animal_no,2,2), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) )
      {
        animal_no <- paste("_", animal_no, sep = "")
      } else {animal_no <- c()}
    }
    #
    if (is.null(animal_no))
    {
      cat(paste("\nInvalid animal number in file name. \n\nPLEASE CHECK YOUR FILE: ", datafile_nme, ".\n\n", sep = ""))
      flog.error("ERROR: Invalid animal number in file name. Breaking up.", name = 'HerdGPS')
    } else {
      tic(paste("Processing time for splitting up data in file ", datafile_nme, sep = ""))
      flog.trace(paste("Reading data from file: ", datafile_nme, sep = ""), name = 'HerdGPS')
      success <- tryCatch({
        data <- read.csv(file.path(ppath, datafile_nme), sep = ",", header = TRUE)
        data$X <- NULL
        success <- TRUE},
        warning = function(war) {
          flog.warn(war, name = 'HerdGPS')
          success <- TRUE },
        error = function(err) {
          flog.error(err, name = 'HerdGPS')
          success <- FALSE },
        finally = {})
      if (success)
      {
        flog.trace("Data was read... checking if data is valid:", name = 'HerdGPS')
        #check if a valid file hase been uploaded:
        if (is.null(data))
        {
          cat(paste("\nData is NULL. No data read from file!!\n\nPLEASE CHECK YOUR FILE: ", datafile_nme, ".\n\n", sep = ""))
          flog.error("ERROR: data NULL. Breaking up.", name = 'HerdGPS')
        } else {
          #Check header:
          if (!identical(names(data), header) )
          {
            print(names(data))
            print(header)
            cat(paste("\nHeader of loaded file not valid!!\n\nPLEASE CHECK YOUR FILE: ", datafile_nme, ".\nand/or remove files from path ", ppath, " that do not contain GPS sensor data.\n", sep = ""))
            flog.error("ERROR: Header of loaded file not valid. Breaking up.", name = 'HerdGPS')
          } else {
            names(data) <- tolower(header)
            # Filter the data for additional headers. Depending on the type of GPS sensor, stopping and re- starting of recording might lead to an additional header row  within the data.
            data <- js_RemoveAdditionalHeader(data)
            days <- unique(data$local.date)
            cat(paste("\nFile ", datafile_nme, " contains GPS-data from ", as.character(length(days)), " days. For each day a separate file will be generated. If a file already exists for a respective day, the new  GPS-data will be appended to the existing one.\n", sep = ""))
            flog.trace(paste("File contains GPS-data from ", as.character(length(days)), " days. Data will be written into one file per day.", sep = ""), name = 'HerdGPS')
            for (l in seq(1, length(days)))
            {
              days_listobject <- list()
              tic(paste("Processing time for data regarding animal '", animal_no, "', day ", as.character(days[l]), ". Side length: ", as.character(side_length), ".", sep = ""))
              DD <- strptime(as.character(days[l]), format = format_date)
              if (is.na(DD))
              {
                cat(paste("\nWrong data format in file ", datafile_nme, ": ", as.character(days[l]), sep = ""))
                flog.warn(paste("\nWrong data format in file ", datafile_nme, ": ", as.character(days[l]), sep = ""), name = 'HerdGPS')
              } else {
                day <- strftime(DD, format = "%Y%m%d")
                days_listobject$date <- day
                d_day <- data[which(data$local.date == days[l]),]
                d_day_prefiltered <- d_day
                ######## Filtering Data ############
                flog.trace(paste("Filtering the data for day ", day, " by the variables 'speed' and 'distance'.", sep = ""), name = 'HerdGPS')
                #prae-filtering by surrounding rectangle, max_speed and _max_dist (per 10 s)
                d_day_prefiltered <- js_PraeFiltering(d_day_prefiltered, end_of_the_world_N, end_of_the_world_S, end_of_the_world_E, end_of_the_world_W, max_speed, max_dist)
                #
                flog.trace(paste("Filtering the data for day ", day, " using a grid with side length ", as.character(side_length), ".", sep = ""), name = 'HerdGPS')
                L <- js_FilterGPSbyGrid(list_of_squares, grid, d_day_prefiltered)
                d_day_prefiltered <- L$filtered_GPS_df
                #Add marker to the data to specify the used sensor
                flog.trace(paste("Adding the marker of the used sensor to the data for day ", day, ".", sep = ""), name = 'HerdGPS')
                d_day_prefiltered <- jhh_DF_mark(d_day_prefiltered, marker)
                #
                dayfile_nme <- file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), paste("InterimResult_", day, "_", as.character(side_length), animal_no, ".csv", sep = ""))
                cat(paste("\nWriting filtered GPS-data of day ", day, " to file ", dayfile_nme, ".\n", sep = ""))
                if ( !file.exists(dayfile_nme) )
                {
                  file.create(dayfile_nme)
                  con <- file(dayfile_nme, "w")
                } else {con <- file(dayfile_nme, "a") }
                flog.trace(paste("Writing filtered GPS-data of day ", day, " to file ", dayfile_nme, ".", sep = ""), name = 'HerdGPS')
                write.table(d_day_prefiltered, file = dayfile_nme, sep = ",", dec = ".", append = TRUE, col.names = FALSE, row.names = FALSE)
                days_listobject$file <- dayfile_nme
                flog.trace(paste("File ", dayfile_nme, " ready.", sep = ""), name = 'HerdGPS')
                close(con)
                save(days_listobject, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "Rdata", paste("InterimResult_", day, "_", as.character(side_length), animal_no, ".RData", sep = "")))
                ####
                flog.trace(paste("Adding Information on when the animal was in which square to the file 'SquareInformation_rawtime_", as.character(side_length), "_", day, ".csv'.", sep = ""), name = 'HerdGPS')
                animal_in_square <- L$animal_in_square
                if (!is.null(animal_in_square))
                {
                  animal_in_square <- cbind.data.frame(rep(substr(animal_no, 2, nchar(animal_no)), nrow(animal_in_square)), animal_in_square)#as.data.frame(animal_in_square))
                  names(animal_in_square) <- c("animal", "date", "time", "square", "speed")
                  contacts_filenme <- file.path(ppath, "Contact_data", paste("SquareInformation_rawtime_", as.character(side_length), "_", day, ".csv", sep = ""))
                  if ( !file.exists(contacts_filenme) )
                  {
                    file.create(contacts_filenme)
                    con_contacts <- file(contacts_filenme, "w")
                  } else {con_contacts <- file(contacts_filenme, "a") }
                  write.table(animal_in_square, file = contacts_filenme, sep = ",", dec = ".", append = TRUE, col.names = FALSE, row.names = FALSE)
                  close(con_contacts)
                }
              }
            }
            toc(log = TRUE, quiet = TRUE)
          }
        }
      }
    }
    toc(log = TRUE, quiet = TRUE)
  }
  toc(log = TRUE, quiet = TRUE)
  flog.trace("All day-files written.\n------------------------------------------------\n------------------------------------------------", name = 'HerdGPS')
  #
  # now processing day files
  tic("Processing time for calculations on the day files")
  flog.trace("Listing all day-files.", name = 'HerdGPS')
  day_files <- dir(file.path(ppath, paste("Day_datafiles_", systemtime, sep = "")),'.csv')
  flog.trace(paste("Found ", as.character(length(day_files)), " day-files.", sep = ""), name = 'HerdGPS')
  cat(paste("Found ", as.character(length(day_files)), " day-files.", sep = ""))
  stats_allAnimals <- c()
  #
  file.create(file.path(ppath, paste("TimeGaps_duringSensorChanging_", systemtime, ".csv", sep = "")))
  con_timegap <- file(file.path(ppath, paste("TimeGaps_duringSensorChanging_", systemtime, ".csv", sep = "")), "w")
  write("animal, sensor_before_change, time_before_GAP, first_time_after_GAP, #minutes", file.path(ppath, paste("TimeGaps_duringSensorChanging_", systemtime, ".csv", sep = "")), append = TRUE)
  #
  for (l in seq(1, length(day_files)))
  {
    pos <- gregexpr('_', day_files[l])
    if (pos[[1]][1] == -1 | length(pos[[1]]) != 3)
    {
      cat(paste("Problem with the GPS day-file: ", day_files[l], ". Invalid filename.", sep = ""))
      flog.error(paste("ERROR: Invalid filename: ", day_files[l], ". Breaking up.", sep = ""), name = 'HerdGPS')
    } else {
      day <- substr(day_files[l], pos[[1]][1] + 1, pos[[1]][2] - 1)
      animal_no <- substr(day_files[l], pos[[1]][3] + 1, nchar(day_files[l]) - 4)
      cat(paste("\n...........\nWorking with GPSdata in ", day_files[l], sep = ""))
      flog.trace(paste("...........\nWorking with GPSdata in ", day_files[l], sep = ""), name = 'HerdGPS')
      flog.trace("Valid filename. loading data.", name = 'HerdGPS')
      #
      success2 <- tryCatch({
        d_day_prefiltered <- read.csv(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), day_files[l]), sep = ",", header = FALSE)
        d_day_prefiltered <- data.frame(d_day_prefiltered, rep(NA, nrow(d_day_prefiltered)))
        #add header and converted date
        names(d_day_prefiltered) <- c(tolower(header), "marker", "ConvertedDate")
        ConvertedDate <- jhhjs_ConvertDateTimeFormat(d_day_prefiltered)
        d_day_prefiltered$ConvertedDate <- ConvertedDate
        d_day_prefiltered <- d_day_prefiltered[order(d_day_prefiltered$ConvertedDate),]
        flog.trace(paste("Adding header and column with converted date-time to the data from file ", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), day_files[l]), sep = ""), name = 'HerdGPS')
        write.table(d_day_prefiltered, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), day_files[l]), sep = ",", dec = ".", col.names = TRUE, row.names = FALSE, append = FALSE)
        success2 <- TRUE},
        warning = function(war) {
          flog.warn(war, name = 'HerdGPS')
          success2 <- TRUE },
        error = function(err) {
          flog.error(err, name = 'HerdGPS')
          success2 <- FALSE },
        finally = {})
      if (success2)
      {
        flog.trace("Data successfully loaded.", name = 'HerdGPS')
        #Filtering the "double" GPS values produced by Frederik during the sensor changing procedure
        flog.trace("Filtering for GPS data of the sensor changing person:\nSpecifying the overlapping GPS data of the two sensors.", name = 'HerdGPS')
        cat("\nFiltering for GPS data of the sensor changing person:\nSpecifying the overlapping GPS data of the two sensors.")
        #
        d_day_filtered <- js_FilteringOverlappingGPSvalues(d_day_prefiltered, sensor_changing_distance, d2m, animal_no, file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), day_files[l]), ppath, systemtime)
#
        flog.trace(paste("Writing filtered data to file ", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), day_files[l]), sep = ""), name = 'HerdGPS')
        filtered_dayfile_nme <- file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), paste(substr(day_files[l], 9, 24), "_Sidelength", as.character(side_length), "_Animal", animal_no, ".csv", sep = ""))
        file.create(filtered_dayfile_nme)
        con_filtered <- file(filtered_dayfile_nme, "w")
        write.table(d_day_filtered, file = filtered_dayfile_nme, sep = ",", dec = ".", col.names = TRUE, row.names = FALSE, append = FALSE)
        close(con_filtered)
        ####
        tic(paste("Total processing time for GPSdata for animal ", animal_no, " of day ", day, sep = ""))
        days_listobject$animal <- animal_no
        days_listobject$data <- d_day_filtered
        #
        cat(paste("\nCalculating min, max, mean, median, standard deviation, and sum for the data columns latitude, longitude, speed, and distance for animal '", animal_no, "' day ", day, ".", sep = ""))
        flog.trace(paste("Basic table with descriptive statistics: Calculating min, max, mean, median, standard deviation, and sum for the data columns latitude, longitude, height, speed, distance for animal '", animal_no, "' and day ", day, ".", sep = ""), name = 'HerdGPS')
        #
        stats <- js_DescrStats_BasicTable(d_day_filtered)
        #
        #########
        # mx <- c(max(d_day_filtered$latitude), max(d_day_filtered$longitude), round(max(d_day_filtered$speed), digits = 4), round(max(d_day_filtered$distance), digits = 4))
        # mn <- c(min(d_day_filtered$latitude), min(d_day_filtered$longitude), round(min(d_day_filtered$speed), digits = 4), round(min(d_day_filtered$distance), digits = 4))
        # mw <- c(mean(d_day_filtered$latitude), mean(d_day_filtered$longitude), round(mean(d_day_filtered$speed), digits = 4), round(mean(d_day_filtered$distance), digits = 4))
        # md <- c(median(d_day_filtered$latitude), median(d_day_filtered$longitude), round(median(d_day_filtered$speed), digits = 4), round(median(d_day_filtered$distance), digits = 4))
        # sd <- c(sqrt(var(d_day_filtered$latitude)), sqrt(var(d_day_filtered$longitude)), round(sqrt(var(d_day_filtered$speed)), digits = 4), round(sqrt(var(d_day_filtered$distance)), digits = 4))
        # sum <- c(sum(d_day_filtered$latitude), sum(d_day_filtered$longitude), round(sum(d_day_filtered$speed), digits = 4), round(sum(d_day_filtered$distance), digits = 4))
        # stats <- data.frame(mx, mn, mw, md, sd, sum)
        # names(stats) <- c("max", "min", "mean", "median", "std.dev.", "sum")
        # row.names(stats) <- c("latitude", "longitude", "speed", "distance")
        #########
        days_listobject$stats <- stats
        save(days_listobject, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "Rdata", paste(day, "_", animal_no, ".RData", sep = "")))
        flog.trace(paste("Saving statistics as RData object ", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "Rdata", paste("STATS_", day, "_", animal_no, ".RData", sep = "")), ".", sep = ""), name = 'HerdGPS')
        save(stats, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "Rdata", paste("STATS_", day, "_", animal_no, ".RData", sep = "")))
        flog.trace(paste("Saving statistics as csv file ", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", paste("STATS_", day, "_", animal_no, ".csv", sep = "")), ".", sep = ""), name = 'HerdGPS')
        write.table(stats, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", paste("STATS_", day, "_", animal_no, ".csv", sep = "")), dec = ".", sep = ",", col.names = TRUE, row.names = TRUE)
        toc(log = TRUE, quiet = TRUE)
        #
        cat(paste("\nAdding line of descriptive statistics to a list containing statistics for animal '", animal_no, "' for all recorded days.", sep = ""))
        flog.trace(paste("Adding line of descriptive statistics to a list containing statistics for animal '", animal_no, "' for all recorded days.", sep = ""), name = 'HerdGPS')
        #
        animal_stats_line <- js_GenerateStatsLine4Animal(d_day_filtered, day, stats)
        #
        #########
        # w_5 <- which(d_day_filtered$speed < 5)
        # w_515 <- which(d_day_filtered$speed >= 5 && d_day_filtered$speed < 15)
        # w_15 <- which(d_day_filtered$speed > 15)
        # N <- length(d_day_filtered$speed)
        # animal_stats_line <- c(day, N, mx[1], mn[1], mx[2], mn[2], mx[4], mn[4], mw[4], md[4], sd[4], round(length(w_5)/N, digits = 4), round(length(w_515)/N, digits = 4), round(length(w_15)/N, digits = 4), mx[5], mn[5], mw[5], md[5], sd[5], sum[5])
        #########
        animalstats_filename <- file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", paste("STATS4ANIMAL_", animal_no, ".csv", sep = ""))
        if ( !file.exists(animalstats_filename) )
        {
          file.create(animalstats_filename)
          con_animal <- file(animalstats_filename, "w")
          write("date, N, max_LAT, min_LAT, max_LONG, min_LONG, max_speed, min_speed, mean_speed, median_speed, std.dev.speed, %_speed_<5, %_speed_5-15, %_speed_>15, max_dist, min_dist, mean_dist, median_dist, std.dev.dist, sum_dist", file = animalstats_filename, append = TRUE)
        } else {con_animal <- file(animalstats_filename, "a") }
        flog.trace(paste("Saving statistics as csv file '", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", animalstats_filename), "'.", sep = ""), name = 'HerdGPS')
        write.table(t(animal_stats_line), file = animalstats_filename, dec = ".", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
        close(con_animal)
        toc(log = TRUE, quiet = TRUE)
        #
        stats_allAnimals <- rbind(stats_allAnimals, c(animal_no, animal_stats_line))
      }
    }
  }
  close(con_timegap)
  toc(log = TRUE, quiet = TRUE)
  #
  stats_allAnimals <- as.data.frame(stats_allAnimals)
  names(stats_allAnimals) <- c("animal", "date", "N", "max_LAT", "min_LAT", "max_LONG", "min_LONG", "max_speed", "min_speed", "mean_speed", "median_speed", "std.dev.speed", "%_speed_<5", "%_speed_5-15", "%_speed_>15", "max_dist", "min_dist", "mean_dist", "median_dist", "std.dev.dist", "sum_dist")
  #
  flog.trace("\n........................\nLoop over day files has terminated.\n........................", name = 'HerdGPS')
  flog.trace(paste("Saving 'stats_allAnimals' as csv file '", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "STATS_ALL-ANIMALS.csv"), "'.", sep = ""), name = 'HerdGPS')
  con_allanimals <- file(file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "STATS_ALL-ANIMALS.csv"), "w")
  write.table(stats_allAnimals, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "STATS_ALL-ANIMALS.csv"), dec = ".", sep = ",", col.names = TRUE, row.names = FALSE, append = TRUE)
  close(con_allanimals)
  flog.trace(paste("Saving 'stats_allAnimals' as Rdata ", file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "STATS_ALL-ANIMALS.Rdata"), ".", sep = ""), name = 'HerdGPS')
  save(stats_allAnimals, file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "Rdata", "STATS_ALL-ANIMALS.Rdata"))
  # #### Do SOME DATA VISUALIZATION
  if (data_visualization)
  {
    stats_allAnimals <- stats_allAnimals %>% na.omit()
	cat("\nDo SOME DATA VISUALIZATION")
    flog.trace("Generating plots.", name = 'HerdGPS')
    #specify time interval of analysis /dates in the data set:
    min_date <- as.character(min(as.numeric(paste(stats_allAnimals$date))))
    min_date <- paste(substr(min_date, 1, 4), "-", substr(min_date, 5,6), "-", substr(min_date, 7,8), sep = "")
    max_date <- as.character(max(as.numeric(paste(stats_allAnimals$date))))
    max_date <- paste(substr(max_date, 1, 4), "-", substr(max_date, 5,6), "-", substr(max_date, 7,8), sep = "")
    ##boxplot max_speed
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Boxplot-maxspeed.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(x = animal, y = as.numeric(paste(max_speed)), group = animal, fill = animal)) + geom_boxplot() + labs(x = "Animals", y = "Maximal speed (km/h)") + ggtitle(paste("Maximal speed grouped after animals\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20), legend.title = element_text(size= 18), legend.text = element_text(size= 18))
    print(f1)
    dev.off()
    ##boxplot mean_speed
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Boxplot-meanspeed.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(x = animal, y = as.numeric(paste(mean_speed)), group = animal, fill = animal)) + geom_boxplot() + labs(x = "Animals", y = "Mean speed (km/h)") + ggtitle(paste("Mean speed grouped after animals\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20), legend.title = element_text(size= 18), legend.text = element_text(size= 18))
    print(f1)
    dev.off()
    ##boxplot max_distance
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Boxplot-maxdistance.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(x = animal, y = as.numeric(paste(max_dist)), group = animal, fill = animal)) + geom_boxplot() + labs(x = "Animals", y = "Maximal distance (m)") + ggtitle(paste("Maximal distance grouped after animals\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20), legend.title = element_text(size= 18), legend.text = element_text(size= 18))
    print(f1)
    dev.off()
    ##boxplot mean_distance
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Boxplot-meandistance.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(x = animal, y = as.numeric(paste(mean_dist)), group = animal, fill = animal)) + geom_boxplot() + labs(x = "Animals", y = "Mean distance (m)") + ggtitle(paste("Mean distance grouped after animals\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20), legend.title = element_text(size= 18), legend.text = element_text(size= 18))
    print(f1)
    dev.off()
    ##boxplot sum_distance
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Boxplot-sumdistance.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(x = animal, y = as.numeric(paste(sum_dist)), group = animal, fill = animal)) + geom_boxplot() + labs(x = "Animals", y = "Total distances (m)") + ggtitle(paste("Total distances grouped after animals\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20), legend.title = element_text(size= 18), legend.text = element_text(size= 18))
    print(f1)
    dev.off()
    ##histogram sum_dist
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Histogram-maxspeed.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(as.numeric(paste(max_speed)))) + geom_histogram(position = 'stack', stat = 'bin', bins = 20, boundary = 0.1) + labs(x = "Maximal speed (km/h)", y = "Frequency") + ggtitle(paste("Histogram for the recorded maximal speed\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20))
    print(f1)
    dev.off()
    ##histogram sum_dist
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "Histogram-sumdistance.pdf"), width = 1200, height = 600)
    f1 <- ggplot(stats_allAnimals, aes(as.numeric(paste(sum_dist)))) + geom_histogram(position = 'stack', stat = 'bin', bins = 20, boundary = 0.1) + labs(x = "Total distance (m)", y = "Frequency") + ggtitle(paste("Histogram for the total distances\nbetween ", min_date, " and ", max_date, sep = "") ) + theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20))
    print(f1)
    dev.off()
	##plot of the mean total distances with quartile h-lines
    #
    animals_meanTotaldistances_byAnimal <- aggregate(as.numeric(paste(sum_dist)) ~ animal, stats_allAnimals, mean)
    names(animals_meanTotaldistances_byAnimal) <- c("animal", "sum_dist")
    q <- quantile(animals_meanTotaldistances_byAnimal$sum_dist,  probs = c(0.25, 0.75))
    print(q)
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "MeanDailydistances_byAnimals.pdf"), width = 1200, height = 600)
    f1 <- ggplot(animals_meanTotaldistances_byAnimal, aes(x = animal, group = 1)) + geom_line(aes(y = as.numeric(paste(sum_dist)), color = "Mean of total distances")) + 
      geom_hline(aes(yintercept = mean(as.numeric(paste(sum_dist))), color = "Overall mean")) + 
      labs(x = "Animals", y = "Mean of total distances (m)") + 
      ggtitle(paste("Mean of total distances between ", min_date, " and ", max_date, ", grouped by animals" sep = "") ) + 
      theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20, legend.title  = element_blank())
    print(f1)
    dev.off()
    animals_meanTotaldistances_byDay <- aggregate(as.numeric(paste(sum_dist)) ~ date, stats_allAnimals, mean)
    names(animals_meanTotaldistances_byDay) <- c("animal", "sum_dist")
    q <- quantile(animals_meanTotaldistances_byDay$sum_dist,  probs = c(0.25, 0.75))
    print(q)
    pdf(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), "stats", "visualization", "MeanDailydistances_byAnimals_byDays.pdf"), width = 1200, height = 600)
    f1 <- ggplot(animals_meanTotaldistances_byDay, aes(x = animal, group = 1)) + 
      geom_line(aes(y = as.numeric(paste(sum_dist)), color = "Mean of total distances")) + 
      geom_hline(aes(yintercept = mean(as.numeric(paste(sum_dist))), color = "Overall mean")) + 
      labs(x = "Days", y = "Mean of total distances (m)") + 
      ggtitle(paste("Mean of total distances between ", min_date, " and ", max_date, ", grouped by days", sep = "")) + 
      theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16, angle = 45), 
             axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  
             axis.title.y  = element_text(face = "bold", size = 20), legend.title  = element_blank())
    print(f1)
    dev.off()
	###################
  #
  toc(log = TRUE, quiet = TRUE)
  log.txt <- tic.log(format = TRUE)
  flog.trace("\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nFiltering of GPS data done.\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-", name = 'HerdGPS')
  flog.info(unlist(log.txt), name = 'HerdGPS')
  cat("\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nFiltering of GPS data done.\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-")
  #
  # now processing contact files
  tic("Processing time for calculations on the contact files")
  flog.trace("Listing all files with SquareInformation_rawtime.", name = 'HerdGPS')
  contact_files <- dir(file.path(ppath, "Contact_data"), 'SquareInformation_rawtime_')
  flog.trace(paste("Found ", as.character(length(contact_files)), " files with SquareInformation_rawtime.", sep = ""), name = 'HerdGPS')
  cat(paste("\n\nNow processing data 'animals in squares'.\nFound ", as.character(length(contact_files)), " raw 'animals in squares' lists.", sep = ""))
  for (l in seq(1, length(contact_files)))
  {
    flog.trace(paste("Working with file '", contact_files[l], "'.", sep = ""), name = 'HerdGPS')
    animal_in_square <- read.csv(file.path(ppath, "Contact_data", contact_files[l]), sep = ",", header = FALSE, colClasses = c("character", "character", "character", "character", "numeric"))
    #Removing entries based on person GPS data from the square information:
    animal_in_square <- js_RemovingPersonsSquareInformation(animal_in_square)
    ##########
    # animals <- paste(unique(animal_in_square[, 1]))
    # #
    # H <- c()
    # for (h in animals)
    # {
    #   dd <- strftime(strptime(paste(animal_in_square[2, 2]),format = "%Y/%m/%d"), format = "%Y%m%d")
    #   D <- read.csv(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), paste("GPSdata_", dd, "_Sidelength", as.character(side_length), "_Animal", h, ".csv", sep = "")))#GPSdata_20180601_10
    #   H_animal <- animal_in_square[which(animal_in_square[,1] == h), ]
    #   H_animal <- H_animal[which(is.element(paste(H_animal[, 3]), paste(D$local.time)) == TRUE), ]
    #   H <- rbind.data.frame(H, H_animal)
    # }
    # animal_in_square <- H
    # names(animal_in_square) <- c("animal", "date", "time", "square", "speed")
    ##########
    write.table(animal_in_square, file = file.path(ppath, "Contact_data", contact_files[l]), sep = ",", dec = ".", col.names = TRUE, row.names = FALSE, append = FALSE)
    #
    animal_in_square <- js_MergingTimeWindows(animal_in_square)
    ################
    # secs <- rep(0, length(animal_in_square$time))
    # for(ll in seq(1, length(animal_in_square$time)))
    # {
    #   t <- as.character(animal_in_square$time[ll])
    #   pos <- gregexpr(':', t)
    #   if (pos[[1]][1] != -1 && length(pos[[1]]) == 2 )
    #   {
    #     secs[ll] <- as.numeric(substr(t, 1, pos[[1]][1]-1))*60*60 + as.numeric(substr(t, pos[[1]][1]+1, pos[[1]][2]-1))*60 + as.numeric(substr(t, pos[[1]][2]+1, nchar(t)))
    #   }
    # }
    # time_window_360 <- ceiling(secs/(6*3600))
    # time_window_240 <- ceiling(secs/(4*3600))
    # time_window_180 <- ceiling(secs/(3*3600))
    # time_window_120 <- ceiling(secs/(2*3600))
    # time_window_60 <- ceiling(secs/3600)
    # time_window_30 <- ceiling(secs/1800)
    # time_window_20 <- ceiling(secs/1200)
    # time_window_15 <- ceiling(secs/900)
    # time_window_10 <- ceiling(secs/600)
    # time_window_05 <- ceiling(secs/300)
    # #
    # flog.trace("Merging local time converted to time windows of different lengths (5 min, 10 min, 15 min, 20 min 30 min, 1 h, 2 h 3 h, 4 h, 6 h) to data set.", name = 'HerdGPS')
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_360)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_240)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_180)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_120)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_60)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_30)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_20)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_15)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_10)
    # animal_in_square <- cbind.data.frame(animal_in_square, time_window_05)
    # names(animal_in_square) <- c("animal", "date", "time", "square", "speed", "360min_window", "240min_window", "180min_window", "120min_window", "60min_window", "30min_window", "20min_window", "15min_window", "10min_window", "5min_window")
    ##################
    dd <- strftime(strptime(paste(animal_in_square[2, 2]), format = format_date), format = "%Y%m%d")
    contacts_filenme_timewindows <- file.path(ppath, "Contact_data", paste("SquareInformation_Timewindows_", as.character(side_length), "_", dd, ".csv", sep = ""))
    con_contacts_timewindows <- file(contacts_filenme_timewindows, "w")
    write.table(animal_in_square, file = contacts_filenme_timewindows, sep = ",", dec = ".", col.names = TRUE, row.names = FALSE, append = FALSE)
    close(con_contacts_timewindows)
  }
  #
  contact_files_timewindows <- dir(file.path(ppath, "Contact_data"), 'SquareInformation_Timewindows_')
  flog.trace("Listing all files with SquareInformation_timewindows.", name = 'HerdGPS')
  flog.trace(paste("Found ", as.character(length(contact_files_timewindows)), " files with SquareInformation_timewindows.", sep = ""), name = 'HerdGPS')
  for (l in seq(1, length(contact_files_timewindows)))
  {
    flog.trace(paste("Working with file '", contact_files_timewindows[l], "'.", sep = ""), name = 'HerdGPS')
    animal_in_square <- read.csv(file.path(ppath, "Contact_data", contact_files_timewindows[l]), sep = ",", header = TRUE, dec = ".")
    dd <- strftime(strptime(paste(animal_in_square[2, 2]), format = format_date), format = "%Y%m%d")
    names(animal_in_square) <- c("animal", "date", "time", "square", "speed", "360min_window", "240min_window", "180min_window", "120min_window", "60min_window", "30min_window", "20min_window", "15min_window", "10min_window", "5min_window")
    animal_in_square <- animal_in_square[animal_in_square$speed <= max_contact_speed,]
    #
    squares <- unique(as.character(paste(animal_in_square$square)))
    for (ii in time_window_lengths)
    {
      flog.trace(paste("Determining animal contacts for time window length ", ii, ".", sep = ""), name = 'HerdGPS')
      contact_net <- js_FindAnimalContacts(animal_in_square, squares, ii)
      ##################
      # contact_net <- c()
      # #
      # H <- animal_in_square[order(animal_in_square$square, animal_in_square[[ii]]), ]
      # windows <- unique(animal_in_square[[ii]])
      # for (j in squares)
      # {
      #   for (jj in windows)
      #   {
      #     H <- H[which(as.character(paste(H$square)) == j & H[[ii]] == jj), ]
      #     animals <- unique(H$animal)
      #     if (!is.na(animals) && length(animals) > 1)
      #     {
      #       for (k in seq(1, length(animals)))
      #       {
      #         if (k < length(animals))
      #         {
      #           for (kk in seq(k + 1, length(animals)))
      #           {
      #             contact_net <- rbind(contact_net, c(animals[k], animals[kk], j, jj))
      #           }
      #         }
      #       }
      #     }
      #   }
      # }
      #
      ##################
      contact_net <- as.data.frame(contact_net)
      tryCatch({
        flog.trace(paste("Writinig contact list for day '", dd, " and time window '", ii, "' to file 'ContactNetwork_Timewindow", substr(ii, 1, nchar(ii) - 7 ), "_Sidelength", as.character(side_length), "_", dd, ".csv'.", sep = ""), name = 'HerdGPS')
        names(contact_net) <- c("Animal1", "Animal2", "Square", paste("TimeWindow(", substr(ii, 1, nchar(ii) - 7 ), ")", sep = ""))
        contact_network_filename <- file.path(ppath, "Contact_data", paste("ContactNetwork_Timewindow", substr(ii, 1, nchar(ii) - 7 ), "_Sidelength", as.character(side_length), "_", dd, ".csv", sep = ""))
        con_contact_network <- file(contact_network_filename, "w")
        write.table(contact_net, file = contact_network_filename, sep = ",", dec = ".", col.names = TRUE, row.names = FALSE, append = FALSE)
        close(con_contact_network)
      },
      warning = function(war) {flog.warn(war, name = 'HerdGPS')},
      error = function(err) {flog.error(err, name = 'HerdGPS')},
      finally = {})
    }
  }
  #
  flog.trace("\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nDONE.\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nHerdGPS-Preprocessor terminated gracefully. Good bye.", name = 'HerdGPS')
  cat("\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nDONE.\n-.-.-.-.-.-.-.-.-.-.-.-.-.-.-\nHerdGPS-Preprocessor terminated gracefully. Good bye.")
}
