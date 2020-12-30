# Region Labels for R tmap Package
I had trouble finding a way to label regions in the maps I was making with tmap, especially in a way that guaranteed ease of reading. This repository supports two functions that do essentially the same thing, just for different implementations of tmap, but both ensure that region labels adhere to Web Content Accessibility Guidelines for proper contrast when they are printed against colorful regions or backgrounds. Set to prompt user if the palette and background don't allow for proper contrast, but this functionality can be turned off by setting the "autoOverrideWCAG" default value in ```ContrastPalette()``` to TRUE.

## Functions 
1. ```BinAndBindLabels()``` should be added as a layer to maps that allow tmap to sort the legend. Sample use in ```RegionLabelDemo()``` function in the R file.
2. ```ProprietaryColoringLabelBinding()``` should be added as a layer to maps that have custom colorings, like [this bivariate choropleth](https://github.com/sdesabbata/BivariateTMap) implementation. Sample use in ```CustomRegionLabelDemo()``` function in the R file.

## Example
![Sample Image](/ContrastPlot.png)

## Acknowledgement
Built initally for Columbia University INCITE's REALM project, released for use as an open-source tool.
