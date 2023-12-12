library(tidyverse)
library(XML)

# Load your data
data <- read.table(text = "station lat lon
House1L -26.37972222 28.94472222
House2L -26.38083333 28.93222222
House3L -26.37916667 28.93472222
House4L -26.38 28.94083333
House1E -26.55972222 29.07666667
House2E -26.54 29.07194444
House3E -26.54805556 29.06722222
House4E -26.53194444 29.07055556
EmbaSouth -26.570343 29.07457
EmbaNorth -26.536389 29.0725
LebohangS -26.3811 28.91833
Secunda -26.550639 29.079028
Lebohang -26.38097458 28.93727302
eMbalenhle -26.56123474 29.08227378", header = TRUE)

# Create a KML file for each station
for (i in 1:nrow(data)) {
  station_name <- data$station[i]
  lat <- as.character(data$lat[i])
  lon <- as.character(data$lon[i])

  # Create a new XML document
  doc <- newXMLDoc()

  # Create the root KML element
  kml <- newXMLNode("kml", attrs = c(xmlns = "http://www.opengis.net/kml/2.2",
                                     `xmlns:gx` = "http://www.google.com/kml/ext/2.2",
                                     xmlns_kml = "http://www.opengis.net/kml/2.2",
                                     xmlns_atom = "http://www.w3.org/2005/Atom"),
                    parent = doc)

  # Create the Document element
  document <- newXMLNode("Document", parent = kml)

  # Add the Placemark element
  placemark <- newXMLNode("Placemark", parent = document)
  name <- newXMLNode("name", station_name, parent = placemark)

  # Add the LookAt and Point elements
  lookAt <- newXMLNode("LookAt", parent = placemark)
  longitude <- newXMLNode("longitude", lon, parent = lookAt)
  latitude <- newXMLNode("latitude", lat, parent = lookAt)
  altitude <- newXMLNode("altitude", "0", parent = lookAt)

  point <- newXMLNode("Point", parent = placemark)
  coordinates <- newXMLNode("coordinates", paste(lon, lat, "0", sep = ","), parent = point)

  # Save the XML document to a file
  saveXML(doc, file = paste0(station_name, ".kml"))
}

