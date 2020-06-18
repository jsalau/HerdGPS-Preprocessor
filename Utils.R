##############################
#FUNCTIONS
js_RemoveAdditionalHeader <- function(data)
{ #w_addheader <- which(data[seq(2, nrow(data)), 1] == "INDEX") + 1
  cat("\n>>>>> 'js_RemoveAdditionalHeader'")
  flog.trace(">>>>>Starting 'js_RemoveAdditionalHeader'", name = 'HerdGPS')
  w_addheader <- which(tolower(as.character(data[seq(2, nrow(data)), 2])) == names(data)[2]) + 1
  print(w_addheader)
  if (length(w_addheader) != 0)
  {
    data <- data[-w_addheader,]
    flog.trace("Additional header was/were removed.", name = 'HerdGPS')
    cat("\nAdditional header was/were removed.")
    data$latitude <- as.numeric(paste(data$latitude))
    data$longitude <- as.numeric(paste(data$longitude))
    data$speed <- as.numeric(paste(data$speed))
    data$distance <- as.numeric(paste(data$distance))
  }

  flog.trace("'js_RemoveAdditionalHeader' done.", name = 'HerdGPS')
  return(data)
}
#
js_PraeFiltering <- function(DF, end_of_the_world_N, end_of_the_world_S, end_of_the_world_E, end_of_the_world_W, max_speed, max_dist)
{
  cat("\n>>>>> 'js_PraeFiltering'")
  flog.trace(">>>>>Starting 'js_PraeFiltering'", name = 'HerdGPS')
  #
  w_N <- which(DF$latitude >= end_of_the_world_N)
  if (length(w_N) != 0) {DF <- DF[-w_N,]}
  w_S <- which(DF$latitude <= end_of_the_world_S)
  if (length(w_S) != 0) {DF <- DF[-w_S,]}
  w_E <- which(DF$longitude >= end_of_the_world_E)
  if (length(w_E) != 0) {DF <- DF[-w_E,]}
  w_W <- which(DF$longitude <= end_of_the_world_W)
  if (length(w_W) != 0) {DF <- DF[-w_W,]}
  w_speed <- which(DF$speed >= max_speed)
  if (length(w_speed) != 0) {DF <- DF[-w_speed,]}
  w_dist <- which(DF$distance >= max_dist)
  if (length(w_dist) != 0) {DF <- DF[-w_dist,]}
  #
  flog.trace("'js_PraeFiltering' done.", name = 'HerdGPS')
  return(DF)
}
#
js_PositionTransform_degree2meter <- function(point1, point2, distance)
{
  cat("\n>>>>> 'js_PositionTransform_degree2meter'")
  flog.trace(">>>>>Starting 'js_PositionTransform_degree2meter'", name = 'HerdGPS')

  degree2meter <-  (abs(point1 - point2))/distance
  #
  flog.trace("'js_PositionTransform_degree2meter' done.", name = 'HerdGPS')
  return(degree2meter)
}
#
js_RepairBoundaryData <- function(boundary_data, side_length, d2m)
{
  cat("\n>>>>> 'js_RepairBoundaryData'")
  flog.trace(">>>>>Starting 'js_RepairBoundaryData'", name = 'HerdGPS')
  #
  d <- sqrt(((boundary_data$latitude - c(boundary_data$latitude[seq(2, nrow(boundary_data))], boundary_data$latitude[1]))/d2m[1])^2 + ((boundary_data$longitude - c(boundary_data$longitude[seq(2, nrow(boundary_data))], boundary_data$longitude[1]))/d2m[2])^2 )
  w <- which(d > 0.5*side_length)
  while( length(w) > 1 )
  {  #
    line <- boundary_data[w[1],]
    line$longitude <- 0.5*(line$longitude + boundary_data$longitude[w[1]+1])
    line$latitude <- 0.5*(line$latitude + boundary_data$latitude[w[1]+1])
    #
    boundary_data <- rbind( rbind( boundary_data[seq(1, w[1]),], line), boundary_data[seq(w[1] + 1, nrow(boundary_data)),])
    #
    d <- sqrt(((boundary_data$latitude - c(boundary_data$latitude[seq(2, nrow(boundary_data))], boundary_data$latitude[1]))/d2m[1])^2 + ((boundary_data$longitude - c(boundary_data$longitude[seq(2, nrow(boundary_data))], boundary_data$longitude[1]))/d2m[2])^2 )
    w <- which(d > 0.5*side_length)
  }
  #
  flog.trace("'js_RepairBoundaryData' done.", name = 'HerdGPS')
  return(boundary_data)
}
#
##Function produces square grid with given "side_length" (in m) in GPS coordinates in the first quadrant relative to the "origin" (given as c(latitude, longitude)).
#The squares are numbered "counting to the right-counting up". The function returns a list with a list "corners" for each square holding the fields "LowLeft",
#"LowRight", "UpLeft", "UpRight".
js_MakeGrid_1stQuadrant <- function(origin, side_length, total_width, d2m)
{
  cat("\n>>>>> 'js_MakeGrid_1stQuadrant'")
  flog.trace(">>>>>Starting 'js_MakeGrid_1stQuadrant'", name = 'HerdGPS')
  #
  no_squares_per_direction <- ceiling(total_width/side_length)
  lats_LowLeft <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  longs_LowLeft <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  lats_LowRight <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  longs_LowRight <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  lats_UpLeft <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  longs_UpLeft <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  lats_UpRight <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  longs_UpRight <- matrix(rep(NA, no_squares_per_direction^2), nrow = no_squares_per_direction, ncol = no_squares_per_direction, byrow = TRUE)
  nms_grid <- rep(NA, no_squares_per_direction)
  for (l in seq(1, no_squares_per_direction))
  {
    for (ll in seq(1, no_squares_per_direction))
    {
      nms_grid[(l-1)*no_squares_per_direction + ll] <- paste(as.character(l), "-", as.character(ll), sep = "")
      #$LowLeft
      lats_LowLeft[l,ll] <- origin[1] + (l-1)*side_length*d2m[1]
      longs_LowLeft[l,ll] <- origin[2] + (ll-1)*side_length*d2m[2]
      #$LowRight
      lats_LowRight[l,ll] <- origin[1] + (l-1)*side_length*d2m[1]
      longs_LowRight[l,ll] <- origin[2] + ll*side_length*d2m[2]
      #$UpLeft
      lats_UpLeft[l,ll] <- origin[1] + l*side_length*d2m[1]
      longs_UpLeft[l,ll] <- origin[2] + (ll-1)*side_length*d2m[2]
      #$UpRight
      lats_UpRight[l,ll] <- origin[1] + l*side_length*d2m[1]
      longs_UpRight[l,ll] <- origin[2] + ll*side_length*d2m[2]
    }
  }
  list_data <- list("LowLeft" = c(1,2), "LowRight" = c(3,4), "UpLeft" = c(5,6), "UpRight" = c(7,8))
  grid <- rep(list(list_data), no_squares_per_direction^2)
  names(grid) <- nms_grid
  for ( i in seq(1,no_squares_per_direction^2) )
  {
    l <- ceiling(i/no_squares_per_direction)
    if (i%%no_squares_per_direction != 0)
    {
      ll <- i%%no_squares_per_direction
    } else {ll <- no_squares_per_direction}
    #
    grid[[i]]$LowLeft <- c(lats_LowLeft[l,ll],longs_LowLeft[l,ll])
    grid[[i]]$LowRight <- c(lats_LowRight[l,ll],longs_LowRight[l,ll])
    grid[[i]]$UpLeft <- c(lats_UpLeft[l,ll],longs_UpLeft[l,ll])
    grid[[i]]$UpRight <- c(lats_UpRight[l,ll],longs_UpRight[l,ll])
  }
  rm(no_squares_per_direction, lats_LowLeft, longs_LowLeft, lats_LowRight, longs_LowRight, lats_UpLeft, longs_UpLeft, lats_UpRight, longs_UpRight, nms_grid)
  #
  flog.trace("'js_MakeGrid_1stQuadrant' done.", name = 'HerdGPS')
  return(grid)
}
#
#Function takes the GPS coordinates on the "boundary" of an area and a "grid". It returns a list with entries "boundary_squares_names" and "boundary_squares"
#The field "boundary_squares_names" contains an array of square names which hold a part of the boundary. The field "boundary_squares" contains a dataframe with variables
#"names" and "bools". THe column "names" contains all square names, and the column "bools" contains a bool indicating whether this square contains a piece of the boundary.
js_BoundarySquares <- function(boundary, grid, produce_plot = NULL, ppath = NULL)
{
  cat("\n>>>>> 'js_BoundarySquares'")
  flog.trace(">>>>>Starting 'js_BoundarySquares'", name = 'HerdGPS')
  #
  boundary_squares <- as.data.frame(cbind(names(grid), rep(0, length(names(grid)))))
  names(boundary_squares) <- c("names", "bools")
  boundary_squares$bools <- as.numeric(paste(boundary_squares$bools))
  for (l in names(grid) )
  {
    w <- which(boundary$longitude >= grid[[l]]$LowLeft[2] & boundary$longitude <= grid[[l]]$LowRight[2] & boundary$latitude >= grid[[l]]$LowLeft[1] & boundary$latitude <= grid[[l]]$UpLeft[1])
    if (length(w) != 0 )
    {
      boundary_squares$bools[which(boundary_squares$names == l)] <- 1
    }
  }
  boundary_squares_names <- boundary_squares$names[which(boundary_squares$bools == 1)]
  #
  if (produce_plot)
  {
    df_squares_boundary <- data.frame(matrix(rep(NA, 3*5*length(boundary_squares_names)), ncol = 3 ))
    names(df_squares_boundary) <- c("BOUNDARY_SQUARES", "latitude", "longitude")
    for (ll in seq(1, length(boundary_squares_names) ))
    {
      sll <- boundary_squares_names[ll]
      lat <- c(grid[[sll]]$LowLeft[1], grid[[sll]]$LowRight[1], grid[[sll]]$UpRight[1], grid[[sll]]$UpLeft[1], grid[[sll]]$LowLeft[1])
      long <- c(grid[[sll]]$LowLeft[2], grid[[sll]]$LowRight[2], grid[[sll]]$UpRight[2], grid[[sll]]$UpLeft[2], grid[[sll]]$LowLeft[2])
      square <- rep(as.character(sll), 5)
      df <- cbind(square, lat, long)
      df_squares_boundary[seq((ll - 1)*5 + 1, ll*5) ,] <- df
    }
    if ( is.null(ppath) )
    {
      pdf(file = file.path(pwd(), paste("Boundary_sidelength", as.character(side_length), "_gridsquares.pdf", sep = "")), width = 1200, height = 600)
    } else {
      pdf(file = file.path(ppath, paste("Boundary_sidelength", as.character(side_length), "_gridsquares.pdf", sep = "")), width = 1200, height = 600)
    }
    f <- ggplot(boundary, aes(x = longitude, y = latitude)) + geom_path(lineend="butt", linejoin="round", linemitre=1)  +
      geom_path(data = df_squares_boundary, lineend="butt", linejoin="round", linemitre=1, aes(x = longitude, y = latitude, group = BOUNDARY_SQUARES, color = BOUNDARY_SQUARES)) +
      theme(legend.position="none")  + labs(x = "Longitude", y = "Latitude") +
      ggtitle(paste("Boundary of the area of observation\ngrid squares, side length ", as.character(side_length), " m", sep = "")) +
      theme( plot.title = element_text(face = "bold", size = "23"), axis.text.x = element_text(face="bold", size = 16), axis.text.y = element_text(face="bold", size = 16), axis.title.x  = element_text(face = "bold", size = 20),  axis.title.y  = element_text(face = "bold", size = 20))
    print(f)
    dev.off()
  }
  #
  flog.trace("'js_BoundarySquares' done.", name = 'HerdGPS')
  return(list("boundary_squares_names" = boundary_squares_names, "boundary_squares" = boundary_squares))
}
#
js_WhichSquare <- function(grid, point)
{
  flog.trace(">>>>>Starting 'js_WhichSquare'", name = 'HerdGPS')
  #
  sidelength_in_lat <- grid[["2-1"]]$LowLeft[1] - grid[["1-1"]]$LowLeft[1]
  sidelength_in_long <- grid[["1-2"]]$LowLeft[2] - grid[["1-1"]]$LowLeft[2]
  #
  if ((point[1]-grid[["1-1"]]$LowLeft[1])/sidelength_in_lat < 0 || (point[2]-grid[["1-1"]]$LowLeft[2])/sidelength_in_long < 0)
  {
    square_name <- c()
   } else {
    square_name <-paste(as.character(ceiling((point[1]-grid[["1-1"]]$LowLeft[1])/sidelength_in_lat)), "-", as.character(ceiling((point[2]-grid[["1-1"]]$LowLeft[2])/sidelength_in_long)), sep = "")
  }

  flog.trace("'js_WhichSquare' done.", name = 'HerdGPS')
  return(square_name)
}
#
#Function takes a given "point" in GPS coordinates and a "grid". It checks, wether the point lies in the area covered by the grid and gives back the name of the grid square.
#It outputs a list with fieldnames "point_covered_by_grid" (holding TRUE/FALSE), and "square_name" (holding the name of the square as specified by "js_MakeGrid_1stQuadrant")
js_IsPointCoveredByGrid <- function(point, grid)
{
  cat("\n>>>>> 'js_IsPointCoveredByGrid'")
  flog.trace(">>>>>Starting 'js_IsPointCoveredByGrid'", name = 'HerdGPS')
  #
  point_covered_by_grid <- FALSE
  square_name <-c()
  #
  if (point[1] >= grid[[names(grid)[1]]]$LowLeft[1] && point[1] <= grid[[names(grid)[length(names(grid))]]]$UpLeft[1] && point[2] >= grid[[names(grid)[1]]]$LowLeft[2] && point[2] <= grid[[names(grid)[length(names(grid))]]]$LowRight[2] )
  {
    point_covered_by_grid <- TRUE
    #
    square_name <- js_WhichSquare(grid, point)
  } else {cat("\nGiven point is not located in the given grid!")}
  #
  flog.trace("'js_IsPointCoveredByGrid' done.", name = 'HerdGPS')
  return(list("point_covered_by_grid" = point_covered_by_grid, "square_name" = square_name))
}
#
js_ListNeighbourSquares <- function(square_name)
{
  cat("\n>>>>> 'js_ListNeighbourSquares'")
  flog.trace(">>>>>Starting 'js_ListNeighbourSquares'", name = 'HerdGPS')
  #
  pos <- gregexpr('-', square_name)
  neighbours <- c(paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) - 1), "-", substr(square_name, pos[[1]][1] + 1, nchar(square_name)), sep = ""),
                  paste(substr(square_name, 1, pos[[1]][1] - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) - 1), sep = ""),
                  paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) + 1), "-", substr(square_name, pos[[1]][1] + 1, nchar(square_name)), sep = ""),
                  paste(substr(square_name, 1, pos[[1]][1] - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) + 1), sep = ""))
  #
  # neighbours <- c(paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) - 1), "-", substr(square_name, pos[[1]][1] + 1, nchar(square_name)), sep = ""),
  # paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) - 1), sep = ""),
  # paste(substr(square_name, 1, pos[[1]][1] - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) - 1), sep = ""),
  # paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) + 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) - 1), sep = ""),
  # paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) + 1), "-", substr(square_name, pos[[1]][1] + 1, nchar(square_name)), sep = ""),
  # paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) + 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) + 1), sep = ""),
  # paste(substr(square_name, 1, pos[[1]][1] - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) + 1), sep = ""),
  # paste(as.character(as.numeric(substr(square_name, 1, pos[[1]][1] - 1)) - 1), "-", as.character(as.numeric(substr(square_name, pos[[1]][1] + 1, nchar(square_name))) + 1), sep = ""))
  #
  flog.trace("'js_ListNeighbourSquares' done.", name = 'HerdGPS')
  return(neighbours)
}
#
#Function takes a given "inner_point" of the area in GPS coordinates, a "grid" and a list "boundary_squares" as output by "js_BoundarySquares". It calls "js_IsPointCoveredByGrid".
#If a square of the grid holds "inner_point", a dataframe of "area_squares" with the same structure as the output boundary_squares of "js_BoundarySquares" is generated,
#which is output by the function. If "inner_point" is not covered by "grid", area_squares" is returne empty.
js_AreaSquares <- function(InnerPoints, grid, boundary_squares)#, produce_plot = TRUE, boundary = NULL, ppath = NULL)
{
  cat("\n>>>>> 'js_AreaSquares'")
  flog.trace(">>>>>Starting 'js_AreaSquares'", name = 'HerdGPS')
  #
  area_squares <- boundary_squares
  area_squares$bools <- as.numeric(paste(area_squares$bools))
  #
  for (l in seq(1, length(InnerPoints)))
  {
    inner_point <- InnerPoints[[l]]
    cat("Working with inner point: ")
    print(inner_point)
    output <- js_IsPointCoveredByGrid(inner_point, grid)
    if (output$point_covered_by_grid)
    {
      # area_squares <- boundary_squares
      # area_squares$bools <- as.numeric(paste(area_squares$bools))
      if (area_squares$bools[which(area_squares$names == output$square_name)] == 0)
      {
        area_squares$bools[which(area_squares$names == output$square_name)] <- 1
        #
        L <- js_ListNeighbourSquares(output$square_name)
        a <- area_squares[which(is.element(area_squares$names, L) == TRUE), ]
        list_of_squares2check <- as.character(paste(a$names[which(a$bools == 0)]))
        while (length(list_of_squares2check) != 0)
        {
          cat("\nLength of list squares to check:")
          print(length(list_of_squares2check))
          #
          w <- which(area_squares$names == list_of_squares2check[1])
          if (length(w) != 0)
          {
            if ( area_squares$bools[which(area_squares$names == list_of_squares2check[1])] == 0)
            {
              area_squares$bools[which(area_squares$names == list_of_squares2check[1])] <- 1
              #
              L <- js_ListNeighbourSquares(list_of_squares2check[1])
              a <- area_squares[which(is.element(area_squares$names, L) == TRUE), ]
              list_of_squares2check <- unique(c(list_of_squares2check[-1], as.character(paste(a$names[which(a$bools == 0)]))))
            } else { list_of_squares2check <- list_of_squares2check[-1] }
          } else { list_of_squares2check <- list_of_squares2check[-1] }
        }
        cat("\nwhile-loop terminated.")
      } else {
        cat("\nGPS coordinates have been used as inner point before!")
      }
    }
    else{
      cat("\nThe given GPS coordinates is not covered by then given grid!!")
    }
  }
  area_squares_names <- area_squares$names[which(area_squares$bools == 1)]
  #
  flog.trace("'js_AreaSquares' done.", name = 'HerdGPS')
  return(list("area_squares_names" = area_squares_names, "area_squares" = area_squares))
}
#
js_FilterGPSbyGrid <- function(list_of_squares, grid, GPS_df)
{
  cat("\n>>>>> 'js_FilterGPSbyGrid'")
  flog.trace(">>>>>Starting 'js_FilterGPSbyGrid'", name = 'HerdGPS')
  #
  if (nrow(GPS_df) > 0)
  {
    animal_in_square <- data.frame(GPS_df$local.date, GPS_df$local.time, rep(NA, nrow(GPS_df)), GPS_df$speed)
    inside_position <- seq(1, nrow(GPS_df))
    #
    S <- lapply(as.list(as.data.frame(t(cbind(as.vector(GPS_df$latitude), as.vector(GPS_df$longitude))))), function(obj) {js_WhichSquare(grid,obj)} )
    v <- lapply(S, function(obj) { if (!is.null(obj)) { if (is.element(obj, list_of_squares)) {l <- 1} else {l <- 0} } } )
    #
    inside_position <- inside_position[which(unlist(v) == 1)]
    animal_in_square <- animal_in_square[which(unlist(v) == 1), ]
    animal_in_square[, 3] <- unlist(S)[which(unlist(v) == 1)]
    # #
    # for (l in seq(1, nrow(GPS_df)))
    # {
    #   square_name <- js_WhichSquare(grid, c(GPS_df$latitude[l], GPS_df$longitude[l]))
    #   if (!is.null(square_name))
    #   {
    #     if (is.element(square_name, list_of_squares))
    #     {
    #       inside_position <- c(inside_position, l)
    #       #
    #       animal_in_square <- rbind(animal_in_square, c(as.character(GPS_df$local.date[l]), as.character(GPS_df$local.time[l]), square_name, GPS_df$speed[l]))
    #     }
    #   }
    # }
    # #
  } else {
    animal_in_square <- c()
    inside_position <- c()
  }
  filtered_GPS_df <- GPS_df[inside_position, ]
  #
  flog.trace("'js_FilterGPSbyGrid' done.", name = 'HerdGPS')
  return(list("filtered_GPS_df" = filtered_GPS_df, "animal_in_square" = animal_in_square ) )
}

###################
#
# Data Frame markieren mit 'marker' in neuer Spalte.
jhh_DF_mark <- function(DF,  marker) # Character
{
  cat("\n>>>>> 'jhh_DF_mark'")
  flog.trace(">>>>>Starting 'jhh_DF_mark'", name = 'HerdGPS')
  # Neue Faktor-Spalte hinzufügen
  DF[ncol(DF)+1] <- as.factor(rep(marker, nrow(DF)))
  # Name der neuen Spalte: 'jhh_marker'
  names(DF)[ncol(DF)] <- 'jhh_marker'

  flog.trace("'jhh_DF_mark' done.", name = 'HerdGPS')
  return(DF)
}
#
jhhjs_ConvertDateTimeFormat <- function(DF)
{
  cat("\n>>>>> 'jhhjs_ConvertDateFormat'")
  flog.trace(">>>>>Starting 'jhhjs_ConvertDateFormat'", name = 'HerdGPS')
  # Spalten aus DF in Character-Vektor vereinigen
  char_vector <- paste( DF$local.date, DF$local.time )
  date_vector <- strptime(char_vector, format = paste(format_date, format_time, sep = " "))#"%Y/%m/%d %H:%M:%S")
  #
  print(date_vector[1])
  #
  flog.trace("'jhhjs_ConvertDateFormat' done.", name = 'HerdGPS')
  return(date_vector)
}
#
js_SpecifyOverlappingGPSdata <- function(GPS_df)
{
  cat("\n>>>>> 'js_SpecifyOverlappingGPSdata'")
  flog.trace(">>>>>Starting 'js_SpecifyOverlappingGPSdata'", name = 'HerdGPS')
  starting_marker <- paste(GPS_df$marker)[1]
  # cat("\n")
  # print(starting_marker)
  # print(unique(paste(GPS_df$marker)))
  #
  if (length(unique(paste(GPS_df$marker))) == 1 )
  {
    # GPS_df_overlap <- c()
    overlapping_indices <- c()
    time1 <- c()
    time2 <- c()
  } else {
    w_a <- which(GPS_df$marker == "a")
    w_b <- which(GPS_df$marker == "b")
    # cat("\n")
    # print(w_a)
    # print(w_b)
    if (starting_marker == "a")
    {
      time1 <- GPS_df$ConvertedDate[w_a[length(w_a)]]
      time2 <- GPS_df$ConvertedDate[w_b[1]]
    } else {
      time1 <- GPS_df$ConvertedDate[w_b[length(w_b)]]
      time2 <- GPS_df$ConvertedDate[w_a[1]]
    }
    overlapping_indices <- which(GPS_df$ConvertedDate <= time1 & GPS_df$ConvertedDate >= time2)
  }
  #
  cat("\n")
  print(time1)
  print(time2)
  flog.trace("'js_SpecifyOverlappingGPSdata' done.", name = 'HerdGPS')
  return(list("overlapping_indices" = overlapping_indices, "starting_marker" = starting_marker, "time1" = time1, "time2" = time2))
}
#
js_SpacialDifferences <- function(DF, d2m)
{
  cat("\n>>>>> 'js_SpacialDifferences'")
  flog.trace(">>>>>Starting 'js_SpacialDifferences'", name = 'HerdGPS')
  #
  lat_diff <- (DF$latitude[seq(1, nrow(DF)-1)] - DF$latitude[seq(2, nrow(DF))])/d2m[1]
  long_diff <- (DF$longitude[seq(1, nrow(DF)-1)] - DF$longitude[seq(2, nrow(DF))])/d2m[2]
  distances <- sqrt(lat_diff^2 + long_diff^2)
  #
  flog.trace("'js_SpacialDifferences' done.", name = 'HerdGPS')
  return(distances)
}
#
js_RemovingPersonGPSvalues  <- function(GPS_df, overlapping_indices, starting_marker, sensor_changing_distance, d2m)
{
  cat("\n>>>>> 'js_RemovingPersonGPSvalues'")
  flog.trace(">>>>>Starting 'js_RemovingPersonGPSvalues'", name = 'HerdGPS')
  #
  distances <- js_SpacialDifferences(GPS_df[overlapping_indices, ], d2m)
  w_distances <- which(distances < sensor_changing_distance)
  if (length(w_distances) == 0)
  {
    w_distances <- which(distances == min(distances))
    flog.trace(paste("NO DISTANCES SMALLER THAN THE THRESHOLD OF ", as.character(sensor_changing_distance), " m FOUND! Using the minimal distance instead." , sep = ""), name = 'HerdGPS')
  }
  flog.trace("Determined point of sensor changing. Skipping Frederik's GPS data.", name = 'HerdGPS')
  GPS_1 <- GPS_df[seq(1, overlapping_indices[1] - 1), ]
  GPS_2a <- GPS_df[overlapping_indices[seq(1, w_distances[length(w_distances)] - 1)], ]
  GPS_2a <- GPS_2a[which(GPS_2a$marker == starting_marker), ]
  GPS_2b <- GPS_df[overlapping_indices[seq(w_distances[length(w_distances)], length(overlapping_indices))], ]
  GPS_2b <- GPS_2b[which(GPS_2b$marker != starting_marker), ]
  GPS_3 <- GPS_df[seq(overlapping_indices[length(overlapping_indices)] + 1, nrow(GPS_df)), ]
  filtered_GPS_df <- rbind.data.frame(GPS_1, rbind.data.frame(rbind.data.frame(GPS_2a, GPS_2b), GPS_3))
  #
  flog.trace("'js_RemovingPersonGPSvalues' done.", name = 'HerdGPS')
  return(filtered_GPS_df)
}
#
js_FilteringOverlappingGPSvalues <- function(GPS_df, sensor_changing_distance, d2m, animal_no, file_name, ppath, systemtime)
{
  cat("\n>>>>> 'js_FilteringOverlappingGPSvalues'")
  flog.trace(">>>>>Starting 'js_FilteringOverlappingGPSvalues'", name = 'HerdGPS')
  output <- js_SpecifyOverlappingGPSdata(GPS_df)
  overlapping_indices <- output$overlapping_indices
  starting_marker <- output$starting_marker
  if ( !is.null(overlapping_indices) )
  {
    flog.trace(paste("Data from TWO SENSORS found in '", file_name, "'.", sep = ""), name = 'HerdGPS')
    cat(paste("\nData from TWO SENSORS found in '", file_name, "'.", sep = ""))
    time1 <- output$time1
    time2 <- output$time2
    if (time1 > time2)
    {
      GPS_df_filtered <- js_RemovingPersonGPSvalues(GPS_df, overlapping_indices, starting_marker, sensor_changing_distance, d2m)
    } else {
      time_gap <- round(as.numeric(difftime(time2,time1, units = "min")))
      if (time_gap >= 1)
      {
        write(paste(animal_no, ", ", starting_marker, ", ", time1, ", ", time2, ", ", time_gap, sep = ""), file.path(ppath, paste("TimeGaps_duringSensorChanging_", systemtime, ".csv", sep = "")), append = TRUE)
        #
        flog.info(paste("TIME GAP (", as.character(time_gap), " minutes) between last measurement made by the sensor to be replaced and the first measurement made by the replacement in '", file_name, "'.", sep = ""), name = 'HerdGPS')
        cat(paste("\nTIME GAP (", as.character(time_gap), " minutes) between last measurement made by the sensor to be replaced and the first measurement made by the replacement in '", file_name, "'.", sep = ""))
      }
      GPS_df_filtered <- GPS_df
    }
  } else {
    flog.trace(paste("Only data from ONE SENSOR in '", file_name, "'.", sep = ""), name = 'HerdGPS')
    GPS_df_filtered <- GPS_df
    cat(paste("\nOnly data from ONE SENSOR in ", file_name, "'.\n", sep = ""))
  }

  flog.trace("'js_FilteringOverlappingGPSvalues' done.", name = 'HerdGPS')
  return(GPS_df_filtered)
}
#################################################################################
js_DescrStats_BasicTable <- function(GPS_df)
{
  cat("\n>>>>> 'js_DescrStats_BasicTable'")
  flog.trace(">>>>>Starting 'js_DescrStats_BasicTable'", name = 'HerdGPS')
  mx <- c(max(GPS_df$latitude), max(GPS_df$longitude), round(max(GPS_df$speed), digits = 4), round(max(GPS_df$distance), digits = 4))
  mn <- c(min(GPS_df$latitude), min(GPS_df$longitude), round(min(GPS_df$speed), digits = 4), round(min(GPS_df$distance), digits = 4))
  mw <- c(mean(GPS_df$latitude), mean(GPS_df$longitude), round(mean(GPS_df$speed), digits = 4), round(mean(GPS_df$distance), digits = 4))
  md <- c(median(GPS_df$latitude), median(GPS_df$longitude), round(median(GPS_df$speed), digits = 4), round(median(GPS_df$distance), digits = 4))
  sd <- c(sqrt(var(GPS_df$latitude)), sqrt(var(GPS_df$longitude)), round(sqrt(var(GPS_df$speed)), digits = 4), round(sqrt(var(GPS_df$distance)), digits = 4))
  sum <- c(sum(GPS_df$latitude), sum(GPS_df$longitude), round(sum(GPS_df$speed), digits = 4), round(sum(GPS_df$distance), digits = 4))
  stats <- data.frame(mx, mn, mw, md, sd, sum)
  names(stats) <- c("max", "min", "mean", "median", "std.dev.", "sum")
  row.names(stats) <- c("latitude", "longitude", "speed", "distance")

  flog.trace("'js_DescrStats_BasicTable' done.", name = 'HerdGPS')
  return(stats)
}
#
js_GenerateStatsLine4Animal <- function(GPS_df, day, stats)
{
  cat("\n>>>>> 'js_GenerateStatsLine4Animal'")
  flog.trace(">>>>>Starting 'js_GenerateStatsLine4Animal'", name = 'HerdGPS')
  w_5 <- which(GPS_df$speed < 5)
  w_515 <- which(GPS_df$speed >= 5 && GPS_df$speed < 15)
  w_15 <- which(GPS_df$speed > 15)
  N <- length(GPS_df$speed)
  stats_line <- c(day, N, stats[1,1], stats[1,2], stats[2,1], stats[2,2], stats[3,1], stats[3,2], stats[3,3], stats[3,4], stats[3,5], round(length(w_5)/N, digits = 4), round(length(w_515)/N, digits = 4), round(length(w_15)/N, digits = 4), stats[4,1], stats[4,2], stats[4,3], stats[4,4], stats[4,5], stats[4,6])
  flog.trace("'js_GenerateStatsLine4Animal' done.", name = 'HerdGPS')

  return(stats_line)
}
#################################################################################
js_RemovingPersonsSquareInformation <- function(animal_in_square)
{
  cat("\n>>>>> 'js_RemovingPersonsSquareInformation'")
  flog.trace(">>>>>Starting 'js_RemovingPersonsSquareInformation'", name = 'HerdGPS')
  animals <- paste(unique(animal_in_square[, 1]))
  H <- c()
  for (h in animals)
  {
    tryCatch({
      dd <- strftime(strptime(paste(animal_in_square[2, 2]),format = format_date), format = "%Y%m%d")
      D <- read.csv(file = file.path(ppath, paste("Day_datafiles_", systemtime, sep = ""), paste("GPSdata_", dd, "_Sidelength", as.character(side_length), "_Animal", h, ".csv", sep = "")))#GPSdata_20180601_10
      H_animal <- animal_in_square[which(animal_in_square[,1] == h), ]
      H_animal <- H_animal[which(is.element(paste(H_animal[, 3]), paste(D$local.time)) == TRUE), ]
      H <- rbind.data.frame(H, H_animal)
    },
    warning = function(war) {
      flog.warn(war, name = 'HerdGPS')
      success2 <- TRUE },
    error = function(err) {
      flog.error(err, name = 'HerdGPS')
      success2 <- FALSE },
    finally = {})
  }
  animal_in_square <- H
  names(animal_in_square) <- c("animal", "date", "time", "square", "speed")

  flog.trace("'js_RemovingPersonsSquareInformation' done.", name = 'HerdGPS')
  return(animal_in_square)
}
#
js_SecondsUntilMidnight <- function(time)
{
  cat("\n>>>>> 'js_SecondsUntilMidnight'")
  flog.trace(">>>>>Starting 'js_SecondsUntilMidnight'", name = 'HerdGPS')
  secs <- rep(0, length(time))
  for(ll in seq(1, length(time)))
  {
    t <- as.character(time[ll])
    pos <- gregexpr(':', t)
    if (pos[[1]][1] != -1 && length(pos[[1]]) == 2 )
    {
      secs[ll] <- as.numeric(substr(t, 1, pos[[1]][1]-1))*60*60 + as.numeric(substr(t, pos[[1]][1]+1, pos[[1]][2]-1))*60 + as.numeric(substr(t, pos[[1]][2]+1, nchar(t)))
    }
  }

  flog.trace("'js_SecondsUntilMidnight' done.", name = 'HerdGPS')
  return(secs)
}
#
js_MergingTimeWindows <- function(data)
{
  cat("\n>>>>> 'js_MergingTimeWindows'")
  flog.trace(">>>>>Starting 'js_MergingTimeWindows'", name = 'HerdGPS')
  secs <- js_SecondsUntilMidnight(data$time)
  #
  time_window_360 <- ceiling(secs/(6*3600))
  time_window_240 <- ceiling(secs/(4*3600))
  time_window_180 <- ceiling(secs/(3*3600))
  time_window_120 <- ceiling(secs/(2*3600))
  time_window_60 <- ceiling(secs/3600)
  time_window_30 <- ceiling(secs/1800)
  time_window_20 <- ceiling(secs/1200)
  time_window_15 <- ceiling(secs/900)
  time_window_10 <- ceiling(secs/600)
  time_window_05 <- ceiling(secs/300)
  #
  nms <- names(data)
  #
  flog.trace("Merging local time converted to time windows of different lengths (5 min, 10 min, 15 min, 20 min 30 min, 1 h, 2 h 3 h, 4 h, 6 h) to data set.", name = 'HerdGPS')
  data <- cbind.data.frame(data, time_window_360)
  data <- cbind.data.frame(data, time_window_240)
  data <- cbind.data.frame(data, time_window_180)
  data <- cbind.data.frame(data, time_window_120)
  data <- cbind.data.frame(data, time_window_60)
  data <- cbind.data.frame(data, time_window_30)
  data <- cbind.data.frame(data, time_window_20)
  data <- cbind.data.frame(data, time_window_15)
  data <- cbind.data.frame(data, time_window_10)
  data <- cbind.data.frame(data, time_window_05)
  names(data) <- c(nms, "360min_window", "240min_window", "180min_window", "120min_window", "60min_window", "30min_window", "20min_window", "15min_window", "10min_window", "5min_window")

  flog.trace("'js_MergingTimeWindows' done.", name = 'HerdGPS')
  return(data)
}
#
js_FindAnimalContacts <- function(animal_in_square, squares, time_window)
{
  cat("\n>>>>> 'js_FindAnimalContacts'")
  flog.trace(">>>>>Starting 'js_FindAnimalContacts'", name = 'HerdGPS')
  contact_net <- matrix(rep(NA, 4*nrow(animal_in_square)), ncol = 4)#c()
  #
  H <- animal_in_square[order(animal_in_square$square, animal_in_square[[time_window]]), ]
  windows <- unique(animal_in_square[[time_window]])
  for (j in squares)
  {
    for (jj in windows)
    {
      HH <- H[which(as.character(paste(H$square)) == j & H[[time_window]] == jj), ]
      animals <- unique(HH$animal)
      if (!is.na(animals) && length(animals) > 1)
      {
        for (k in seq(1, length(animals)))
        {
          if (k < length(animals))
          {
            for (kk in seq(k + 1, length(animals)))
            {
              contact_net[which(is.na(contact_net[, 1]) == TRUE)[1], ] <- c(animals[k], animals[kk], j, jj)
              #contact_net <- rbind(contact_net, c(animals[k], animals[kk], j, jj))
            }
          }
        }
      }
    }
  }
  contact_net <- contact_net[seq(1:which(is.na(contact_net[, 1]) == TRUE)[1] - 1), ]

  flog.trace("'js_FindAnimalContacts' done.", name = 'HerdGPS')
  return(contact_net)
}
