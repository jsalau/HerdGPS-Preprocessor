#################################################################################
#################################################################################
### DO NOT ALTER!! ->
#systemtime
systemtime <- str_replace_all(as.character(Sys.time()), fixed(" "), "_")
systemtime <- str_replace_all(systemtime, fixed(":"), "")
day <- substr(str_replace_all(systemtime, fixed(":"), ""), 1, 10)
#
if (!dir.exists(file.path(getwd(), "log")))
{
  dir.create(file.path(getwd(), "log"))
}
flog.threshold(TRACE, name = 'HerdGPS')
options(error = function() { flog.error(geterrmessage(), name = 'HerdGPS') ; traceback() ; stop() })
flog.appender(appender.file(file.path(getwd(), "log", paste("log_HerdGPS-Preprocessor_", day, ".txt", sep = ""))), name = 'HerdGPS')
flog.trace("Starting log-file for HerdGPS-Preprocessor.\n--------------------------------\nReading variable settings from 'Config.R'.", name = 'HerdGPS')
#<-
#################################################################################
#################################################################################
#
#
#
#
#################################################################################
#############################  SET VARIABLES  ###################################
#################################################################################
#path to the folders with animal GPS data
ppath <- file.path("C:", "Users", "Jennifer Salau", "Documents", "Frederik", "RohdatenDinghorst")
#######################################
#SPECIFICS REGARDING THE GPS SENSOR
#######################################
# Please copy the header of a representative csv file exported from your GPS sensor
# in the input variable 'header'. The 'header' will be used to verify that the read
# file contains GPS data of the desired format.
header <- c("ï..INDEX", "RCR","UTC.DATE","UTC.TIME","LOCAL.DATE","LOCAL.TIME","MS","VALID","LATITUDE","N.S","LONGITUDE","E.W","HEIGHT","SPEED","Distance")
# Please specify the formats of local date and local time as recorded by your GPS sensor.
format_date <- "%Y/%m/%d"
format_time <- "%H:%M:%S"
###################################
#SPECIFICS REGARDING THE AREA OF OBSERVATION
###################################
# Specify one point respectively for north, south, east, and west to define the
# minimal (geodesic) rectangle (parallel to the lines of latitude/longitude)
# including the area of observation, i.e. determine on point each to give the
# most north, most south, most east, and most west geodesic coordinates of the
# area of observation.
end_of_the_world_N <- 54.248853#54°14'55.9"N 10°11'47.1"E
end_of_the_world_S <- 54.245#c(54.245834, 10.196059)#54°14'45.0"N 10°11'45.8"E
end_of_the_world_E <- 10.206290#54°14'49.6"N 10°12'22.6"E
end_of_the_world_W <- 10.194165#54°14'50.2"N 10°11'39.0"E
############################
# Provide GPS data of the boundary of the area of observation
name_of_boundary_file <- "Rundgang2.csv"
# Provide list of points within the area of observation
InnerPoints <- list(c(54.246409, 10.197785), c(54.2462, 10.197), c(54.2475, 10.1955))
###################################
#SPECIFICS REGARDING THE GRID
###################################
# Set origin of the grid. Place the origin soutern of the most south and western
# of the most west points of your area of obesrvation to fully have the area of
# observation within the first quadrant.
origin <- c(54.2455, 10.194)
# Set side length of squares and total grid width. Make sure the total grid width
# is chosen larger than the diameter of the area of observation
side_length <- 5
total_width <- 2000
# For the transformation from latitude/longitude degree to meters, two reference
# points with a known distance between them are needed.
ref_point_1 <- c(54.247030, 10.196699)
ref_point_2 <- c(54.246838, 10.197550)
reference_distance <- 60
###################################
#SPECIFICS REGARDING (PRE)FILTERING
###################################
# Speed and distance upper bounds to filter the GPS data of the boundary
# (defaults defined for a walking person)
max_dist_boundary <- 20 # m in 10 seconds
max_speed_boundary <- 7 # km/h
# Speed and distance upper bounds for animals (to prefilter the animals GPS data)
max_dist <- 140 # m in 10 seconds
max_speed <- 50 # km/h
# Set threshold distance for sensor changing between the replacement GPS sensor
# carried by the sensor changing person and the to be replaced GPS sensor on the
# animal.
sensor_changing_distance <- 1 #m
##################################################################################
###################################
#SPECIFICS REGARDING CONTACT DEFINITION
###################################
# The contacts between the animals will be determined based on time windows. The
# implemented time windows are "360min window", "240min window", "180min window",
# "120min window", "60min window", "30min window", "20min window", "15min window",
# "10min window", "5min window". The program will output lists for all time
# windows that are lilsted in the variable 'time_window_lengths'. You could save
# computational time, if you specify the time windows of interest.
time_window_lengths <- c("360min_window", "240min_window", "180min_window", "120min_window", "60min_window", "30min_window", "20min_window", "15min_window", "10min_window", "5min_window")
# Please determine the maximal speed an animal can have such that its presence
# in the same square as another animal should be counted a contact between those
# animals, to exclude situations where one animal is just running past the other
# from being voted as contacts. If you do not want to exclude these situation,
# just set 'max_contact_speed' to a redundantly high value.
max_contact_speed <- 5
##################################################################################
###################################
#SPECIFICS REGARDING CALCULATION AND OUTPUT
###################################
# If you want the program to output statistical visualization of your data, set
# the variable 'data_visualization' to TRUE.
data_visualization <- TRUE
# If you - from a former run of the program - already have a list of squares
# (i.e. a file "ListOfCoveringSquares_Sidelength....RData") of the given side length
# covering your area of observationyou can set the variable 'Calculate_list_of_squares'
# to FALSE. Look up the already calculated files "ListOfCoveringSquares_Sidelength....RData"
# in the directory you specified in variable 'ppath'.
### ATTENTION!!:
# If you set 'Calculate_list_of_squares' to TRUE the program is calculating a grid and
# specifying the squares covering the area of observation. Depending on the side length,
# this could take a lot computational time. So choose 'FALSE' here, if you already have
# the list of squares.
calculate_list_of_squares <- TRUE # FALSE
#load(file = file.path(ppath, paste("GridsquaresCoveringfDinghorst_Sidelength", as.character(side_length) ,".RData", sep = "") )) #list_of_squares
##################################################################################
##################################################################################
