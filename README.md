# CSmosquitoSp
## R package for Spatial analysis of Citizen Science mosquito reports

The package provides a collection of analytics to perform spatially-oriented analysis of Citizen Science mosquito reports for specific Area Of Interest and defined temporal interval. It provides function to handle spatial data and to generate map layout.
The package allows to import spatialized mosquito reports (at present ZanzaMapp DataBase is supported), perform spatio-temporal analysis of the report dataset, create spatial map layoutsand export results.
Spatial polygons for the Italian territory are provided. Users can upload custom spatial polygons 

### Main features:

* import spatialized mosquito reports (at present ZanzaMapp DataBase is supported)
* clean up reports
* perform spatial analysis of the report dataset for a defined temporal interval
* create spatial map layouts
* export results

### Input spatial features:

* Vector spatial features of class 'sf' with the following fields: 'CODE' (unique code); 'AREA' (in square km); 'POPULATION'; 'POP_DENS' (population every 1 square km)

### Output:

* Spatial vector features in Shapefile format (and optionally KML format) containing results computed for each spatial polygon
* Map layout in PNG format

### Installation

**To load** (using `devtools`):
```r
library(devtools)
install_github("ffilipponi/CSmosquitoSp")
```
### Use in QGIS

The package can be used from QGIS. In QGIS version 3 it is necessary to install 'Processing R Provider' plugin. The CSmosquitoSp spatial analyst can be integrated in QGIS Processing by adding the files /inst/rscript/CSmosquitoSp_spatial_analyst.rsx and /inst/rscript/CSmosquitoSp_spatial_analyst.rsx in the R script folder used by QGIS R processing.

### Authors

* Filipponi Federico
* Manica Mattia

### License

Licensed under the GNU General Public License, Version 3.0: https://www.gnu.org/licenses/gpl-3.0.html
