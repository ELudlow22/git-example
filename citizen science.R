install.packages("rgbif")
library(rgbif)



#key <- name_suggest(q = "Caprimulgidae", rank="family")$data["key"] # Nightjar
key <- name_suggest(q = "Loxia", rank="genus")$data["key"][1,1] # Crossbill
cntry_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]
sx <- occ_search(taxonKey = key, country = cntry_code)

# To only return only if geographic coords available
sx <- occ_search(taxonKey = key, country = cntry_code, hasCoordinate = TRUE,
                 eventDate = '1990,2020', limit=2500)
unique(sx$data$scientificName)

sx$data <- sx$data[sx$data$scientificName != "Loxia curvirostra curvirostra",]
sx$data <- sx$data[sx$data$scientificName != "Loxia leucoptera bifasciata (C.L.Brehm, 1827)",]
sx$data <- sx$data[sx$data$scientificName != "Loxia Linnaeus, 1758",]
unique(sx$data$scientificName)

problems <- gbif_issues()
View(problems)

sx <- sx %>% occ_issues(-bri, -cdiv, -cdout, -txmathi) # Pipe syntax
# sx <- occ_issues(sx, -bri, -cdiv, -cdout, -txmathi) # Standard syntax

library(ggplot2)
library(dplyr)
ggplot(sx$data, aes(x=year)) +
  geom_histogram()

records_per_yr <- sx$data %>% 
  group_by(year) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = year, y=count_per_year)) +
  geom_line()



#### need to have a look at ways it can be adapted









library(leaflet)

library(leafem)

# Create map 
# We will use different colour codes for each species
unique_spp <- unique(sx$data$scientificName) # Unique list of species
marker_col <- rainbow(length(unique_spp))    # Define set of rainbow colours
base_col <- rep("red", nrow(sx$data))        # Create a vector with default red


for(i in 1:nrow(sx$data)){
  for(j in 1:length(unique_spp)){
    if(sx$data$scientificName[i] == unique_spp[j]){
      base_col[i] <- marker_col[j]
    }
  }
}

m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(sx$data$decimalLongitude, sx$data$decimalLatitude,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col=base_col) %>% 
  addLegend(colors = marker_col, opacity=1, labels=unique_spp)
m


#### Try modifying your map to display an information popup when you click on a marker.







install.packages("rinat")
library(rinat)

sx <- get_inat_obs(query = "crossbill")

sx <- get_inat_obs(query = "crossbill", place_id=6857, maxresults=2500)
nrow(sx)

gb_ll <- readRDS("gb_simple.RDS")
plot(gb_ll)

sx2 <- get_inat_obs(query = "crossbill", bounds=gb_ll, maxresults=2500)
nrow(sx2)


sx <- sx[sx$quality_grade == "research",]
nrow(sx)


unique(sx$scientific_name)

sx <- sx[sx$scientific_name != "Loxia", ]
sx <- sx[sx$scientific_name != "Carduelis carduelis", ]
sx <- sx[sx$scientific_name != "Chloris chloris", ]
unique(sx$scientific_name)


install.packages("lubridate")
library(lubridate)

summary(sx$datetime) # Note character representation

# Convert datetime from character into a proper date format
sx$datetime <- sx$datetime %>% 
  ymd_hms()
summary(sx$datetime) # Now correctly coded as a date and time


sx <- sx %>% 
  mutate(year = year(datetime))

ggplot(sx, aes(x=year)) +
  geom_histogram()

records_per_yr <- sx %>% 
  group_by(year) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = year, y=count_per_year)) +
  geom_line()


for(i in 1:nrow(sx)){  
  for(j in 1:length(unique_spp)){
    if(sx$scientific_name[i] == unique_spp[j]){
      base_col[i] <- marker_col[j]
    }
  }
}

m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(sx$longitude, sx$latitude,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col=base_col) %>% 
  addLegend(colors = marker_col, opacity=1, labels=unique_spp)
m


sx$image_url[1]

sx$sound_url[sx$sound_url != ""] # Look for where it does not equal (!=) a blank


nbn <- read.csv("records-2021-04-27/records-2021-04-27.csv")
nbn <- nbn[nbn$identificationVerificationStatus.processed == "Accepted",]


ggplot(nbn, aes(x=year.processed)) +
  geom_histogram()


records_per_yr <- nbn %>% 
  group_by(year.processed) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = year.processed, y=count_per_year)) +
  geom_line()


m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(nbn$decimalLongitude.processed, nbn$decimalLatitude.processed,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red") %>% 
  addLegend(colors = "red", opacity=1, labels="Scottish Crossbill")
m



install.packages("devtools")
library(devtools)
install_github("fozy81/NBN4R", force = TRUE)

library(NBN4R)
nbn_reasons()

nbn_config(download_reason_id = 4)

nbn_config(warn_on_empty=TRUE)


tx <- taxinfo_download("rk_family:Fringillidae",
                       fields=c("guid","rk_genus","scientificName","rank"))
# View(tx[])  # List structure in R so need empty square brackets to access the data

install.packages("phytools")
library(phytools)

tx <- tx[tx$rank %in% c("species","subspecies"),] ## restrict to species and subspecies

## as.phylo requires the taxonomic columns to be factors
tx$genus <- as.factor(tx$genus)
tx$scientificName <- as.factor(tx$scientificName)

## create phylo object of scientific Latin name nested within Genus
ax <- as.phylo(~genus/scientificName, data=tx)
plotTree(ax, type="fan", fsize=0.6)


#### If you want, it is possible to change the structure of the displayed taxonomic tree, and even add photos of species to the dendrogram. See the NBN4R website.


nbn_config(caching="off")
nbn_config(verbose=TRUE)

loxia_recs <- occurrences(taxon="Loxia scotica", download_reason_id=10,
                          email="e.ludlow2@newcastle.ac.uk", verbose=TRUE)



loxia_recs <- ALA4R::occurrences(taxon="Loxia", download_reason_id=10,
                                 email="e.ludlow2@newcastle.ac.uk", verbose=TRUE)


loxia_recs <- readRDS("nbn_Loxia.RDS")

# Remove unconfirmed records
unique(loxia_recs$data$identificationVerificationStatus)

nrow(loxia_recs$data)

loxia_recs$data <- loxia_recs$data[loxia_recs$data$identificationVerificationStatus != "Unconfirmed",]
loxia_recs$data <- loxia_recs$data[loxia_recs$data$identificationVerificationStatus != "Unconfirmed - not reviewed",]
nrow(loxia_recs$data)


# Check list of species
unique(loxia_recs$data$scientificName)

# Remove records only identified to genus Loxia
loxia_recs$data <- loxia_recs$data[loxia_recs$data$scientificName != "Loxia",]

# Loxia curvirostra crivirostra is the subspecies for Loxia curvirostra
# (common crossbill). Generally recorded the same so may be better to merge
# Use a conditional search with == inside square brackets to replace
# subspecies with species
loxia_recs$data$scientificName[loxia_recs$data$scientificName == "Loxia curvirostra curvirostra"] <- "Loxia curvirostra"
unique(loxia_recs$data$scientificName)


# If you only keep Loxia scotica for simplicity for now
loxia_scotica <- loxia_recs
loxia_scotica$data <- loxia_scotica$data[loxia_scotica$data$scientificName == "Loxia scotica",]
nrow(loxia_scotica$data)

ggplot(loxia_scotica$data, aes(x=startDateYear)) +
  geom_histogram()

records_per_yr <- loxia_scotica$data %>% 
  group_by(startDateYear) %>% 
  summarise(count_per_year = n())

ggplot(records_per_yr, aes(x = startDateYear, y=count_per_year)) +
  geom_line()


m <- addProviderTiles(leaflet(),"Esri.WorldImagery") %>% 
  ## add markers
  addCircleMarkers(loxia_scotica$data$longitudeWGS84, loxia_scotica$data$latitudeWGS84,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red") %>% 
  addLegend(colors = "red", opacity=1, labels="Scottish Crossbill")
m

#### Try modifying the above code, and display all 4 species of crossbill, using different colour codes, using the loxia_recs data. See the example from the iNaturalist map earlier. Also try modifying your line plot to display different lines for each species over time



####Try downloading some taxa records for species you are particularly interested in. Plot the number of records over time, and produce some interactive leaflet maps to visualise the citizen science data. Compare the same taxon between two or more different citizen science websites.







