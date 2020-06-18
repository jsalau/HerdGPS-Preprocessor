# HerdGPS-Preprocessor
Sensors delivering information on the position of farm animals have been widely used in precision livestock farming. 
Global Positioning System (GPS) sensors are already known from applications in military, private and commercial environments, 
and their application in animal science is increasing. Here, a software tool ``HerdGPS-Preprocessor'' for the preparation of 
GPS data collected from a herd of animals is proposed. 

In order to face the limited battery life of the sensors but also avoid gaps in data recording, this tool is able to deal with 
two sensors for each animal, that were attached in exchange. Collecting position data from multiple animals nonstop for several 
weeks produces a high amount of raw data which contained position outliers as well as possibly overlapping data from two sensors. 
This needed to be sorted, preprocessed and provided in a suitable format in one file per animal and day.

The cleaned data enables direct statistical analysis or further specialized filtering. Also lists of contacts aggregated 
within various time window lengths are output, making static and dynamic network analysis possible.

############################################################################
# Some hints on the usage:
############################################################################

Preparation of raw data

• GPS data needed to be organized in a directory that contained one subdirectory per animal.
• In these data folders GPS data should be stored in csv files.
• Filenames must consist of
  – the date,
  – the animal identification and
  – a one digit marker (’a’, ’b’), denoting one of two exchange sensors.
Date and animal identification need to be separated by an underscore, whilst the marker is attached
directly to the animal identification, e.g. ’180601_10a.csv’.


Configuration of “HerdGPS-Preprocessor”

Open the configuration file ’Config.R’ and set the following variables to suit your application.
• Set the path to the directory which contains the data folders (see Preparation of raw data).
• Configure variables concerning the used GPS sensor:
  – The header of the data files extracted from the GPS sensor.
  – Formats in which date and time are given
• Configure variables concerning the area of observation.
  – Specify GPS coordinates of corner points of the minimal rectangle including the area of observation.
  – Enter the name of the file containing GPS data of the boundary of the area of observation.
  – GPS coordinates of inner points need to be given as a list.
• Configure additional variables concerning the prefiltering.
  – Set upper bounds for the covered distance between two consecutive GPS measurements for the
    boundary (max_dist_boundary) and the GPS data recorded from the animals (max_dist).
  – Define maximal speed for GPS data of the boundary (max_speed_boundary) and GPS data recorded
    from the animals (max_speed).
• Configure variables concerning the grid used for filtering.
  – Specify GPS coordinates of a grid origin located south and west of the area of observation.
  – Define a side length in meters.
  – Set a total width in meters large enough that the grid covers the area of observation completely.
  – Specify GPS coordinates of two reference points and the distance between them, to enable a trans-
      formation between latitude/longitude degrees and meters.
• Set threshold distance for sensor changing between the ’replacement GPS sensor’ carried by the sensor
changing person and the ’to be replaced GPS sensor’ on the animal -> sensor_changing_distance = 1 m
• Specify the time window lengths for the aggregation of contacts. Choose from the following:
  time_window_lengths <- c("360min_window", "240min_window", "180min_window", "120min_window",
                            "60min_window", "30min_window", "20min_window", "15min_window", 
                            "10min_window", "5min_window")

Save and close ’Config.R’.
