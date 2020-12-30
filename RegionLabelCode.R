library(rgdal)
library(classInt)
library(sf)
library(tmaptools)
library(tmap)

####################
## Demo Functions ##
####################

#How to add region label layer given a custom palette and color break
#determined by tmap functionality
RegionLabelDemo<-function(){
  map <- readOGR(dsn="India Shapefile", layer="IND_adm1", verbose = FALSE)
  map@data$RandomData <- sample(10, size = nrow(map@data), replace = T)
  map <- st_as_sf(map)
  
  palette = get_brewer_pal("Accent",n=10)
  tm_shape(map)+tm_fill(col="RandomData",n=10, style="pretty", palette=palette)+
    tm_layout(bg.color="white")+BinAndBindLabels(map,"RandomData", "NAME_1", 10, "pretty", palette, "white")
}

#How to add region label layer given custom colorings by region external to tmap
#Note: the data is essentially the same as RegionLabelDemo, just bound to map
# prior to being passed to tmap
CustomRegionLabelDemo<-function(){
  map <- readOGR(dsn="India Shapefile", layer="IND_adm1", verbose = FALSE)
  
  map <- st_as_sf(map)
  palette = get_brewer_pal("Accent",n=10)
  map$RandomData <- sample(10, size = nrow(map), replace = T)
  map$CustomBin <- findCols(classIntervals(map$RandomData, n=10, "pretty"))
  map$Color <- palette[map$CustomBin]
  
  tm_shape(map)+tm_fill(col="Color")+
    tm_layout(bg.color="white")+ProprietaryColoringLabelBinding(map,"Color", "NAME_1", "white")
}

########################
## Callable Functions ##
########################

#function called to add well-contrasted region labels based on data
  #data is the shapefile with data being mapped bound 
  #dataCol is name of column with data being mapped
  #regionCol is name of column with region names
  #n and style is number of color options and their breaking style (should line up with palette brewer)
  #palette is brewed palette for the given map colors
  #background is background color of map
BinAndBindLabels<-function(data, dataCol, regionCol, n, style, palette, background){
  colnames(data)[which(colnames(data) == dataCol)] <- "ActiveData"
  colnames(data)[which(colnames(data) == regionCol)] <- "SubRegion"
  
  data$Bin <- findCols(classIntervals(data$ActiveData, n, style))
  labelPalette <- ContrastPalette(palette, background, T)
  data$LabelColor <- labelPalette[data$Bin]
  RegionLabels(data)
}

#function to add well-contrasted region labels based on pre-set region colors
  #data is the shapefile, only colors of regions has to be bound
  #colorCol is name of column with color hex values
  #regionCol is name of column with region names
  #background is background color of map
ProprietaryColoringLabelBinding<-function(data, colorCol, regionCol, background){
  colnames(data)[which(colnames(data) == colorCol)] <- "RegionColor"
  colnames(data)[which(colnames(data) == regionCol)] <- "SubRegion"
  
  propietaryPalette <- unique(data$RegionColor)
  data <- transform(data, Bin=match(RegionColor, unique(RegionColor)))
  labelPalette <- ContrastPalette(propietaryPalette, background, T)
  data$LabelColor <- labelPalette[data$Bin]
  RegionLabels(data)
}

######################
## Helper Functions ##
######################

#builds palette for text layer to best contrast their background region
ContrastPalette<-function(palette, background, autoOverrideWCAG=FALSE){
  contrastPalette <- lapply(palette, function(x) ContrastHex(x[1], background, autoOverrideWCAG))
  i<-1
  for(color in contrastPalette){
    palette[i] <- color
    i<-i+1
  }
  
  palette
}

#determines the best contrasting color for a given background region color
#and background color
ContrastHex<-function(labelBackground, masterBackground, quiet){
  range<-c(0:255)
  greyScale <- data.frame("red"=range, "green"=range, "blue"=range)
  greyScale$Luminosity <- apply(greyScale, 1, function(x) ComputeRelLumin(x[1],x[2],x[3]))
  
  label <- col2rgb(labelBackground)
  labelLumin <- ComputeRelLumin(label["red", 1], label["green", 1], label["blue", 1])
  greyScale$LabelLumin <- labelLumin
  
  master <- col2rgb(masterBackground)
  masterLumin <- ComputeRelLumin(master["red", 1], master["green", 1], master["blue", 1])
  greyScale$MasterLumin <- masterLumin
  
  greyScale$LabelContrastRatio <- (greyScale$LabelLumin+.05)/(greyScale$Luminosity+.05)
  greyScale$LabelContrastRatio[greyScale$LabelContrastRatio < 1] <- 1/greyScale$LabelContrastRatio[greyScale$LabelContrastRatio < 1]
  greyScale$MasterContrastRatio <- (greyScale$MasterLumin+.05)/(greyScale$Luminosity+.05)
  greyScale$MasterContrastRatio[greyScale$MasterContrastRatio < 1] <- 1/greyScale$MasterContrastRatio[greyScale$MasterContrastRatio < 1]
  
  optimalRowNumber <- OptimizeContrast(greyScale[,c("LabelContrastRatio", "MasterContrastRatio")], quiet)
  optimalRow <- greyScale[optimalRowNumber,]
  bestColor <- paste("#",format(as.hexmode(optimalRow$red), width=2), format(as.hexmode(optimalRow$green), width=2), format(as.hexmode(optimalRow$blue), width=2), sep="")
  toupper(bestColor)
}

#computes the relative luminosity of a given rbg value based to determine contrast
ComputeRelLumin<-function(red, green, blue){
  rgbVec <- c(red, green, blue)
  rgbVec <- sapply(rgbVec, ComputeRelColor)
  relLumin <- rgbVec[[1]]*.2126+rgbVec[[2]]*.715+rgbVec[[3]]*.0722
  relLumin
}

#computes the relative color of a given rbg value based to determine luminosity
ComputeRelColor<-function(color){
  color <- color/255
  if(color <= .03928)
    color <- color/12.92
  else
    color <- ((color+.055)/1.055)**2.4
  color
}

#optimizes the acceptable contrasting colors to best contrast their background
#given the input to the quiet parameter, will prompt user if their color palette for some regions
#combined with the background color makes it difficult to find a properly contrasting label color
OptimizeContrast<-function(contrasts, quiet){
  labelTests <- c(8:5)
  labelTests <- c(labelTests, 4.5)
  masterTests <- (8:2)
  for(labelContrast in labelTests){
    for(masterContrast in masterTests){
      testFrame <- contrasts[(contrasts$LabelContrastRatio > labelContrast & contrasts$MasterContrastRatio > masterContrast), ]
      if(nrow(testFrame) > 0)
        break
    }
    if(nrow(testFrame) > 0)
      break
  }
  
  if(nrow(testFrame) <= 0){
    
    if(quiet==FALSE){
      print("Some label colors cannot meets Web Content Accessibility Guidelines for proper contrast with background color and region color. 
            Consider adjusting your background color or palette")
      worseContrast <- readline(prompt="Would you like to test a contrast below Web Content Accessibility Guidelines? (y/n)\n")
      worseContrast <- toupper(worseContrast)
    }
    else
      worseContrast='Y'
    
    if(worseContrast == 'Y'){
      labelTests <- c(4:2)
      masterTests <- (3:1)
      
      for(labelContrast in labelTests){
        for(masterContrast in masterTests){
          testFrame <- contrasts[(contrasts$LabelContrastRatio > labelContrast & contrasts$MasterContrastRatio > masterContrast), ]
          if(nrow(testFrame) > 0)
            break
        }
        if(nrow(testFrame) > 0)
          break
      }
    }
    else{
      print("Returning black instead")
      testFrame <- contrasts[which(rownames(contrasts) == "1"), ]
    }
    
    if(nrow(testFrame) <= 0){
      print("No adequette colors, try a different pallete or adjust your background color. Returning black") 
      testFrame <- contrasts[which(rownames(contrasts) == "1")]
    }
  }
  
  rownames(testFrame[1,])
}

#function adding the tmap text layer
#adjusting can change size and appearance of labels if desired
RegionLabels<-function(regions){

  if(nrow(regions) > 0){
    labels <- st_centroid(st_as_sf(regions))
    labels$Area <- st_area(st_as_sf(regions))
    labels$Area <- as.numeric(labels$Area)
    labels$Area <- labels$Area/100000000 #determines label size
    labels$CleanNames <- sapply(regions$SubRegion, NameCleaner)
    
    tm_shape(labels)+
      tm_text(text="CleanNames", size="Area", col = "LabelColor", root=5, just="center", 
              fontface = 2, legend.size.show=FALSE, alpha=.6,legend.col.show = FALSE,
              scale=.8, print.tiny = FALSE)
  }
}

#formats region names to be capitalized and shorter if possible
NameCleaner<-function(nameString){
  nameString <- tolower(nameString)
  output <- strsplit(nameString, " ")[[1]]
  output <- paste(toupper(substring(output, 1,1)), substring(output, 2), " ", sep="")
  output <- gsub("And ", "& ", output)
  output <- paste(output, collapse="")
  substring(output, 0, nchar(output)-1)
}