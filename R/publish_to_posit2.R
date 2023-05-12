library(connectapi)
library(dplyr)

client <- connect(
  server = 'http://posit2:3939',
  api_key = 'NRZYcQFo81oU2kS6jUSjMViWd0VjMz1T'
)

# get all content
all_content <- get_content(client, limit = Inf)
gis_slides <- all_content %>% select(title, name, guid) %>%
  filter(name == "practical_gis_in_rstudio")

guid <- gis_slides$guid[1]

# Get the deployment bundle for this
gis_content <- content_item(client, guid)

# Reset the access_type to "logged_in"
gis_content$update(access_type="logged_in")
