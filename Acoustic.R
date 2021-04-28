# Install devtools from CRAN if not already done
install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")

install.packages("dbehaviouR", force=TRUE)

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)


# Now we will create folder in your RStudio Project called 'FocalRecordings'
dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

# Now we will load the sound files that were in the behaviouR package
githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

# Now we will save the recordings to the new folder you created as standard
# audio .wav format files; You do not need to understand the next set of code
# in detail
for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = ""))
}

GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")
GibbonWaveFile

#Check duration makes sense
duration(GibbonWaveFile) * GibbonWaveFile@samp.rate

#can plot the amplitude 
oscillo(GibbonWaveFile)

oscillo(GibbonWaveFile, from = 0.1, to = 0.2)

#zoom in further
oscillo(GibbonWaveFile, from = 0.15, to = 0.2)


SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")

#zoom to where the calls are
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500)

#in colour
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500, Colors = "Colors")

?SpectrogramSingle


# It is sometimes a case of trial-and error to get the limits and spectro.colors
# at a suitable scale to see the information displayed nicely
v <- ggspectro(GibbonWaveFile, flim=c(0,2.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

v


# We can tell R to print the spectrograms 2x2 using the code below
par(mfrow = c(2, 2))


# This is the function to create the spectrograms
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, 
                    max.freq = 2500, Colors = "Colors")
par(mfrow = c(1,1))
# This is the function to create the spectrograms
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, 
                    max.freq = 2500, Colors = "Colors")


FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")

dim(FeatureDataframe)

View(FeatureDataframe)

install.packages("vegan")
library(vegan) # This should already be installed from NES8010
source("nes8010.R")

# Use [, -1] to keep all rows but omit first column
acoustics_pca <- ordi_pca(FeatureDataframe[, -1], scale=TRUE)
summary(acoustics_pca)

ordi_plot(acoustics_pca, display="sites")


acoustics_sco <- ordi_scores(acoustics_pca, display="sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

#### try doing some extra work here that we performed in the previous module



install.packages("warbleR")
library(warbleR)

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)


blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)



blackbird_alarm2 <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:male type:alarm len:5-25', download = FALSE)

View(blackbird_alarm)

map_xc(blackbird_songs, leaflet.map = TRUE)


# Create subfolders in your RStudio Project for song calls and alarm calls
dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# Download the .MP3 files into two separate sub-folders
query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")


#### come back to this later to understand it better

library(stringr) # part of tidyverse

old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)


old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)


dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))


blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")
blackbird_wav

oscillo(blackbird_wav)

oscillo(blackbird_wav, from = 0.59, to = 0.60)

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")


blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)

dim(blackbird_mfcc)

blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca)

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 







