writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

install.packages("jsonlite", type = "source")

install.packages("keras")
library(keras)

install_keras()

mydata <- dataset_mnist()
summary(mydata)

detach("package:keras", unload = TRUE)

install.packages("rgbif")
install.packages("rinat")

install.packages("devtools")
library(devtools)
install_github("fozy81/NBN4R", force = TRUE)

devtools::install_github("https://github.com/DenaJGibbon/behaviouR", force= TRUE)

install.packages("shiny")
library(shiny)

install.packages("BIRDS")

library(ggplot2)
library(dplyr)
library(lubridate)
library(warbleR)
library(sp)
library(sf)
library(raster)
library(mapview)
library(leaflet)
library(leafem)
library(xts)
library(zoo)
library(shinipsum)
library(camtrapR)
library(imager)

