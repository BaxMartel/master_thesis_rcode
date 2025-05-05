library(sf)
library(dplyr)
library(rvest)
library(sp)

## LOAD DATASETS

df1 <- st_read("YOUR/PATH/GPS_Collar84475_20231128152736.kml"); df1$id <- 84475
df2 <- st_read("YOUR/PATH/GPS_Collar84474_20231128155210.kml"); df2$id <- 84474


# NOTE: Only a few sample datasets are loaded here for demonstration.
# IF you have more GPS/KML files, replicate this block.

df <- rbind(df1, df2)


## EXTRACT INFOS FROM KML DESCRIPTION COLUMN (HTML)

extract_info <- function(html_text, tag) {
  html <- read_html(html_text)
  value <- html_nodes(html, xpath = paste0("//td[contains(text(), '", tag, "')]/following-sibling::td[1]")) %>%
    html_text(trim = TRUE)
  
  if (length(value) > 0) {
    return(value)
  } else {
    return(NA)
  }
}

# apply extract_info

df <- df %>%
  mutate(
    date = sapply(Description, extract_info, tag = "Date"),
    time = sapply(Description, extract_info, tag = "Time"),
    z = sapply(Description, extract_info, tag = "Altitude"),
    DOP = sapply(Description, extract_info, tag = "DOP"),
    sats_used = sapply(Description, extract_info, tag = "Sats used"),
  )


## GET COODINATES

coordinates <- st_coordinates(df$geometry)
df <- cbind(df, long = coordinates[, "X"], lat = coordinates[, "Y"])

## FORMAT CONVERSION

df <- df %>%
  mutate(
    z = as.numeric(z),
    dop = as.numeric(gsub(",", ".", DOP)),
    sats = as.integer(sats_used),
    date = as.Date(date, format = "%d.%m.%Y"),
    timestamp = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M", tz = "UTC")
  )

## REMOVE FIRST 24H AND LAST DAY

df <- df %>%
  group_by(id) %>%
  filter(timestamp > (min(timestamp) + 24*60*60) & date < max(date)) %>%
  ungroup()

## FILTER GPS QUALITY

df <- df %>%
  filter(dop < 10 & sats >= 7)


## CALCULATE DURATION (MIN) BETWEEN EACH ENTRY

dfges <- df %>%
  arrange(id, timestamp) %>%
  group_by(id) %>%
  mutate(duration = c(NA, difftime(timestamp[-1], timestamp[-n()], units = "mins")))

## WGS TO UTM CONVERSION

dfges <- data.frame(dfges)

wgs84_to_utm <- function(df) {
  
  points <- SpatialPoints(cbind(df$long, df$lat), proj4string = CRS("+proj=longlat"))
  
  points_utm <- spTransform(points, CRS("+proj=utm +zone=33 ellps=WGS84"))
  
  utm_points_df <- SpatialPointsDataFrame(points_utm, data = df)
  
  # Extract UTM coordinates
  utm_coords <- coordinates(utm_points_df)
  
  # Add UTM coordinates as columns to the input data frame
  df$x <- utm_coords[, 1]
  df$y <- utm_coords[, 2]
  
  return(df)
}

dfges <- wgs84_to_utm(dfges)


## CLEANUP

dfges <- subset(dfges, select = c( - Description, - geometry, - date, - time, -z, -DOP, -sats_used))


## SAFE PROCESSED DATA

write.csv2(dfges,"YOUR/PATH/red_deer_enclosures_7.csv")

## CALCULATE DISTANCES

distances_list <- list()

df_split <- split(dfges, dfges$id)

# loop through each group
for (group_id in names(df_split)) {
  df_group <- df_split[[group_id]]
  
  # create SpatialPoints for group
  group_coordinates <- cbind(df_group$x, df_group$y)
  group_points <- SpatialPoints(group_coordinates)
  
  # vector for distances within group with ids
  group_distances <- data.frame(id = character(), distance = double())
  
  # loop through rows of the group
  for (i in 2:nrow(df_group)) {
    if (df_group$duration[i] >= 12 && df_group$duration[i] <= 18) {
      # distance
      dis <- spDists(group_points[i,], group_points[(i-1),], longlat = FALSE)
      
      # append distances and ids to dataframe
      group_distances <- rbind(group_distances, data.frame(id = group_id, distance = dis))
    }
  }
  
  # store distances for group
  distances_list[[group_id]] <- group_distances
}

# combine distances
dist <- do.call(rbind, distances_list)

# row names to NULL
rownames(dist) <- NULL

# engineering e

dist$distance <- formatC(as.numeric(dist$distance),format="e")

## EXTRACTION OF FIRST DIGITS OF EACH DISTANCE

dist$digit <- substr(dist$distance, 1, 1)

dist <- dist %>%
  filter(digit != 0)

# create dataframe with counts of each digit
digits <- as.data.frame(table(dist$digit))

# calculate percentage of each digit
digits$percentage <- digits$Freq / length(dist$digit) * 100

# convert distance to numeric
dist$distance <- as.numeric(dist$distance)

write.csv2(dist,"YOUR/PATH/distance_red_deer_enclosures_7.csv")

