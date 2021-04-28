library(BIRDS)
library(NBN4R)
library(sp)
library(sf)
library(lubridate)


nbn_config(caching="off")
drosera_nbn <- ALA4R::occurrences(taxon="Drosera",
                                  download_reason_id=4,
                                  verbose=TRUE,
                                  email="e.ludlow2@newcastle.ac.uk")

# If the download fails, use GBIF, or this drosera_nbn.RDS file from Canvas
# drosera_nbn <- readRDS("drosera_nbn.RDS")

drosera_nbn <- readRDS("drosera_nbn.RDS")

unique(drosera_nbn$data$scientificName)

PBD <- drosera_nbn$data
PBD <- PBD[!is.na(PBD$longitudeWGS84) | !is.na(PBD$latitudeWGS84) ,]
PBD <- PBD[PBD$startDate != "" ,]
PBD <- PBD[PBD$locality  != "" ,]
PBD <- PBD[PBD$recorder  != "" ,]
PBD <- PBD[PBD$rank      == "species" ,] # This can be done via organizeBirds
PBD <- PBD[PBD$startDateYear >= 1970 ,]


OB <- organizeBirds(PBD,
                    sppCol = "scientificName",
                    simplifySppName = TRUE,  # Removes authors and years etc.
                    idCols = c("locality", "recorder"),
                    xyCols = c("longitudeWGS84", "latitudeWGS84"),
                    timeCols = "startDate")

gb_ll_sf <- readRDS("gb_simple.RDS")
scot_ll_sf <- gb_ll_sf[2,]  # Scotland is coded number 2 in the GB map used
scot_ll_sp <- as_Spatial(scot_ll_sf) # Expects polygon in sp rather than sf format

scot_grid <- makeGrid(scot_ll_sp, 25)

## Warning in proj4string(obj): CRS object has comment, which is lost in output
plot(scot_grid)

SB <- summariseBirds(OB, grid=scot_grid)

all_species <- listSpecies(SB)
all_species

focal_species <- all_species[1] # Let's pick Drosera anglica
focalSpReport(SB, focalSp=focal_species)


drosera_summary <- speciesSummary(SB)
View(SB)


# Look at some summarised variables:
# Number of observations
EBnObs <- exportBirds(SB, dimension = "temporal", timeRes = "yearly", 
                      variable = "nObs", method = "sum")
# Number of visits
EBnVis <- exportBirds(SB, dimension = "temporal", timeRes = "yearly", 
                      variable = "nVis", method = "sum")
# The ratio of number of observations over number of visits
relObs <- EBnObs/EBnVis
# Average species list length (SLL) per year (a double-average, i.e. the mean 
# over cell values for the median SLL from all visits per year and cell) 
EBavgSll <- colMeans(SB$spatioTemporal[,,"Yearly","avgSll"], na.rm = TRUE)

# Set the result to a time-series object
EBavgSll <- xts::xts(EBavgSll, date(relObs))


# column-bind the observations and visits
obs_visits <- cbind(EBnObs, EBnVis)

# plot(obs_visits)                   # Default time-series plot
autoplot(obs_visits, facet=NULL) +   # ggplot2-compatible
  xlab("Year") +
  ylab("Observations or visits")

autoplot(relObs) +
  xlab("Year") +
  ylab("Relative observations by visit")

autoplot(EBavgSll) +
  xlab("Year") +
  ylab("Average species list length per cell ")


# Identify gridcells which have no records. A bit complicated a the SB
# object is an R "list" structure.
wNonEmpty<-unname( which( unlist(lapply(SB$overlaid, nrow)) != 0) )

# Plot Scotland in light grey
plot(scot_ll_sp, col="grey90", border = "grey90", lwd=1)
# Plot records in darker gray
plot(SB$spatial[wNonEmpty,], col="grey", border = NA, add=TRUE)


# Export single variables from SummarisedBirds object. Here we want the total 
# number of years, nYears, for each cell, with information broken down by
# month.
EB <- exportBirds(SB,
                  dimension = "Spatial",
                  timeRes = "Month",
                  variable = "nYears", # nObs, nVis, nSpp, nDays, AvgSll options
                  method = "sum")      # sum, medain, mean options


# Create a nice looking colour palette from white to navy blue
palBW <- leaflet::colorNumeric(palette = "Blues",
                               domain = c(0, max(EB@data, na.rm = TRUE)) )
# Create a sequence of 5 numbers from 0 to max number of years
yrCnts <- seq(0, max(EB@data, na.rm = TRUE), length.out = 5)


yrCnts

palBW(yrCnts)

# Set plot window to 1 row and 2 columns
par(mfrow=c(1,2))

# July data
# Plot Scotland background
plot(scot_ll_sp, col="grey90", border = "grey90", lwd=1)
# Plot July visits. Give the col option the palBW function with July data
plot(EB, col=palBW(EB@data$Jul), border = NA, add=TRUE)
# Map caption
mtext("Number of years for which \nJuly was sampled", 3, line=-2)

# Plot December data in the same way
plot(scot_ll_sp, col="grey90", border = "grey90", lwd=1)
plot(EB, col=palBW(EB@data$Dec), border = NA, add=TRUE)
mtext("Number of years for which \nDecember was sampled", 3, line=-2)

# Add legend showing number of years
legend("bottomleft",                 # Position of legend (in last plot)
       legend= yrCnts,               # Numbers to use in legend
       col = palBW(yrCnts),          # Colours for legend
       title = "Number of years",    # Legend title
       pch = 15,                     # Shape of colour symbols; 15=square
       bty="n")                      # Do not draw box round legend

# Reset plot window
par(mfrow=c(1,1))



# Create a nice colour-palette. The upper boundary is set to maximum of `nVis`
# palBW is an R function to generate sequence of colours within limits
palBW <- leaflet::colorNumeric(palette = "Blues", 
                               domain = c(0, max(SB$spatial@data$nVis, na.rm = TRUE)))
# Create sequence numbers from to 0 to max visits; 5 number groups, for legend
seqNVis <- round(seq(0, max(SB$spatial@data$nVis, na.rm = TRUE), length.out = 5))


# Grey background map
plot(scot_ll_sp, col="grey90", border = "grey90", lwd=1)
# Add visits
plot(SB$spatial, col=palBW(SB$spatial@data$nVis), border = NA, add=TRUE)

# Add legend
legend("bottomleft",
       legend = seqNVis,                    # Numbers used in legend              
       col = palBW(seqNVis),                # Colours used in legend
       title = "Number of \nobservations",  # Legend caption
       pch = 15,                            # Shape of colour; 15 is square
       bty="n")                             # Omit background box


# Extract ignorance scores. h = 5 is the half-ignorance parameter value
ign <- exposeIgnorance(SB$spatial@data$nVis, h = 5)

# Create a colour palette function, sequence, and colour codes
palBWR <- leaflet::colorNumeric(palette = c("green", "white","red"), 
                                domain = c(0, 1))
seqIgn <- seq(0, 1, length.out = 5)  # numbers 0.00, 0.25, 0.50, 0.75, 1.00
colIgn <- palBWR(seqIgn)             # convert to R colour codes


# Grey background map
plot(scot_ll_sp, col="grey90", border = "grey90", lwd=1)
# Ignorance map
plot(SB$spatial, col=palBWR(ign), border = NA, add=TRUE)

# Add legend
legend("bottomleft",
       legend=c(seqIgn, "NA"),       # Add an extra category for NA values
       col = c(colIgn, "grey90"),    # Colour NA values grey
       title = "Ignorance on nVis; \nh 0.5 = 5",
       pch = 15,
       bty="n")











