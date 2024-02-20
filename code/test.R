# Install required packages if you haven't already
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("highcharter")) install.packages("highcharter")

library(jsonlite)
library(httr)
library(tidyverse)
library(highcharter)

# Convert JSON string to R object
json_text <- fromJSON("https://gbfs.citibikenyc.com/gbfs/gbfs.json")
json_text


# Extract the URLs
# Assuming `json_data` is correctly parsed as shown previously
feeds <- json_text$data$en$feeds
names(feeds)
View(feeds)

not_interested_datasets <- c("gbfs","gbfs_versions","system_hours","system_calendar","system_information","system_pricing_plans","system_alerts","free_bike_status")
# Filter feeds_df to only include rows with the interested datasets
filtered_feeds <- feeds %>% filter(name %notin% not_interested_datasets)

# Create a named list of URLs for easier access
urls_list <- setNames(filtered_feeds$url, filtered_feeds$name)

# Function to fetch and parse JSON data
fetch_data <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- content(response, "text")
    json_data <- fromJSON(data)
    return(as.data.frame(json_data))
  } else {
    stop("Failed to fetch data from", url)
  }
}

# Initialize an empty list to store the dataframes
dataframes_list <- list()

# Iterate over the urls_list to fetch and parse data for each dataset
for (name in names(urls_list)) {
  url <- urls_list[[name]]
  dataframes_list[[name]] <- fetch_data(url)
}

ebikes_data_df <- dataframes_list[["ebikes_at_stations"]]
system_regions_df <- dataframes_list[["system_regions"]]
station_info_data_df <- dataframes_list[["station_information"]]
station_status_data_df <- dataframes_list[["station_status"]]

# > ebikes at stations dataset ----
flatten_ebikes <- function(row) {
  # Extracting the nested list of ebikes
  ebikes <- row[["data.stations.ebikes"]]
 # If ebikes is NULL or empty, return a row with NA values for ebike attributes
  if (is.null(ebikes) || length(ebikes) == 0) {
    return(tibble(
      conservative_range_miles = NA_real_,
      estimated_range_miles = NA_real_,
      rideable_id = NA_character_,
      displayed_number = NA_character_,
      docking_capability = NA_integer_,
      make_and_model = NA_character_,
      is_lbs_internal_rideable = NA_logical_,
      battery_charge_percentage = NA_integer_
    ))
  }
  # Flatten the nested ebikes data
  ebikes_df <- map_df(ebikes, function(ebike) {
    tibble(
      conservative_range_miles = ebike$range_estimate$conservative_range_miles,
      estimated_range_miles = ebike$range_estimate$estimated_range_miles,
      rideable_id = ebike$rideable_id,
      displayed_number = ebike$displayed_number,
      docking_capability = ebike$docking_capability,
      make_and_model = ebike$make_and_model,
      is_lbs_internal_rideable = ebike$is_lbs_internal_rideable,
      battery_charge_percentage = ebike$battery_charge_percentage
    )
  })
  # Add other non-nested attributes to the flattened ebikes dataframe
  ebikes_df <- ebikes_df %>%
    mutate(
      station_id = row[["data.stations.station_id"]],
      last_updated = row[["last_updated"]],
      ttl = row[["ttl"]],
      version = row[["version"]]
    )
  
  return(ebikes_df)
}

flattened_ebikes_df <- map_df(1:nrow(ebikes_data_df), function(i) {
  flatten_ebikes(ebikes_data_df[i, ])
})

View(flattened_ebikes_df)

ebikes_at_station_df  <- flattened_ebikes_df %>% 
    mutate(
        station_id = str_replace_all(station_id, ".*_", "")
    )

View(ebikes_at_station_df)

# stations datasets
colnames(station_info_data_df) <- gsub("^data\\.stations\\.", "", colnames(station_info_data_df))
colnames(station_status_data_df) <- gsub("^data\\.stations\\.", "", colnames(station_status_data_df))
colnames(system_regions_df)  <- gsub("^data\\.regions\\.", "", colnames(system_regions_df))


# wrangling ----
ebikes_at_station_agg  <- ebikes_at_station_df %>% 
    reframe(
        n_ebike = n(),
        mean_battery_pct = mean(battery_charge_percentage, na.rm = T),
        .by = station_id
    ) %>% 
    left_join(
        station_info_data_df  %>% select(station_id, station_name = name, capacity,region_id, lat, lon),
        join_by(station_id)
    ) %>% 
    left_join(
        system_regions_df  %>% select(region_id, region_name = name),
        join_by(region_id)
    ) %>% 
    mutate(
        region_name = case_when(
            is.na(region_name) ~ "Others",
            TRUE ~ region_name
        ),
        pct_ebikes = n_ebike*100/capacity,
        region_station = paste0(region_name,": ",station_name)
    )

View(ebikes_at_station_agg)

ebikes_at_station_count  <- ebikes_at_station_df %>% 
    select(station_id) %>%
    left_join(
        station_info_data_df %>% select(station_id, station_name = name,region_id),
        join_by(station_id)
    ) %>% 
    left_join(
        system_regions_df  %>% select(region_id, region_name = name),
        join_by(region_id)
    ) %>% 
    mutate(
        region_name = case_when(
            is.na(region_name) ~ "Others",
            TRUE ~ region_name
        ),
        region_station = paste0(region_name,": ",station_name),
        val = 1
    )

View(ebikes_at_station_count)

# visualizing ----

# ebike distribution
cols_1 <- c("#005f73","#0a9396","#94d2bd","#e9d8a6")
lvl_opts <-  list(
    list(
      level = 1,
      borderWidth = 0,
      borderColor = "transparent",
      dataLabels = list(
        enabled = TRUE,
        align = "left",
        verticalAlign = "top",
        style = list(
          fontSize = "12px", 
          textOutline = FALSE,
          color = "white",
          fontWeight = "normal"
          )
      )
    ),
    list(
      level = 2,
      borderWidth = 0,
      borderColor = "transparent",
      colorVariation = list(key = "brightness", to = 0.250),
      dataLabels = list(enabled = FALSE),
      style = list(
        fontSize = "8px",
        textOutline = FALSE, 
        color = "white", 
        fontWeight = "normal"
        )
    )
  )

hchart(
  data_to_hierarchical(ebikes_at_station_count, c(region_name, region_station), val, colors = cols_1),
  type = "treemap",
  # levelIsConstant = FALSE,
  allowDrillToNode = TRUE,
  levels = lvl_opts,
  tooltip = list(valueDecimals = FALSE)
  ) |> 
  hc_chart(
    style = list(fontFamily = "Roboto")
  ) |> 
  hc_title(
    text = "Number of Ebikes at Regions and Stations",
    style = list(fontFamily = "Roboto")
    ) |> 
  hc_size(height = 500)|>
  hc_add_theme(hc_theme_bloom())


# ebike usability
cols_2  <- c("#bb3e03", "#ca6702", "#ee9b00", "#e9d8a6")

ebikes_at_station_agg$colours  <- colorize(ebikes_at_station_agg$pct_ebikes, cols_2)

tltip_x  <- c("Station", "District", "Capacity", "Occupancy","Average Battery Percentage")
tltip_y <- c(sprintf("{point.%s}",c("station_name","region_name")),sprintf("{point.%s:.2f}",c("capacity","pct_ebikes","mean_battery_pct")))
tltip  <- tooltip_table(tltip_x,tltip_y)

hchart(
  ebikes_at_station_agg,
  "scatter",
  hcaes(
    pct_ebikes, 
    mean_battery_pct, 
    size = capacity, 
    color = pct_ebikes    ),
  minSize = 2,
  maxSize = 20
  )|>
  hc_xAxis(
    title = list(text = "Station Occupancy"),
    gridLineWidth = 0
    ) |>
  hc_yAxis(
    title = list(text = "Average Battery Percentage"),
    gridLineWidth = 0,
    softMax = 90,
    tickInterval = 10
    ) |>
  hc_title(
    text = "Ebikes Availability and Usage"
    ) |>
  hc_subtitle(
    text = "in New York"
    ) |>
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
    ) %>% 
 hc_add_theme(hc_theme_538())

 # battery efficiency

tltip_x_2  <- c("Bike ID", "Model","Battery Percentage", "Estimated Distance")
tltip_y_2 <- c(sprintf("{point.%s}",c("rideable_id","make_and_model")),sprintf("{point.%s:.2f}",c("battery_charge_percentage","estimated_range_miles")))
tltip_2  <- tooltip_table(tltip_x_2,tltip_y_2)

hchart(
  ebikes_at_station_df,
  "scatter",
  hcaes(
    battery_charge_percentage, 
    estimated_range_miles, 
    color = make_and_model)
  )|>
  hc_xAxis(
    title = list(text = "Battery Charge Percentation"),
    gridLineWidth = 0
    ) |>
  hc_yAxis(
    title = list(text = "Travel Distance Capability (miles)"),
    gridLineWidth = 0,
    tickInterval = 20
    ) |>
  hc_title(
    text = "Ebikes Battery Efficiency"
    ) |>
  hc_subtitle(
    text = "in New York"
    ) |>
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip_2
    ) %>% 
 hc_add_theme(hc_theme_538())
