# RRASSLER  

![RRASSLER LOGO](https://github.com/JimColl/RRASSLER/blob/main/man/figures/RRASSLER_knockout.png)

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# Executive Summery
The HEC-RAS model format, both as stand alone models or in archive formats, are incomparable for end users whose use case includes widespread accounting and deployment of that data as inputs into other workflows.  This tool should be deployed to ingest HEC-RAS models into a "RAS_model_catalog", a normalized and spatialized representation of those models with the requisite metadata and formatting needed to mesh seamlessly with [national scale hydrofabric efforts and data models](https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html), and applications such as [T-Route](https://github.com/NOAA-OWP/t-route) and [RAS2FIM](https://github.com/NOAA-OWP/ras2fim).

<TOC>

# Intro
There are few hydraulic models as prolific as HEC-RAS, and since it's first named release in 1995 users have created these models using public and private data and countless hours of engineering scrutinization in order to generate the best possible purpose-built representation of the world.  Like any model, some level of input massaging is necessary in order to get the data into the specified mathematical format a model requires.  Like most domain specific software solutions, that massaging was rather, forceful, to the point of permanently altering the shape of those inputs into something that most geospatial data readers are unable to handle.  This creates a great deal of friction both in terms of model accountability and interoperability, particularly when you take the standpoint as a model consumer.   The R based HEC-RAS Wrestler (RRASSLER) is here to mediate that.

## Ingest logic
In order to wrassle that friction, RRASSLER has a few theoretical value judgments that need to be described in order to help understand why it does what it does and how to use it properly.  RRASSLER is focused on making the critical data objects needed to run a HEC-RAS model readily available and accountable as a model (as opposed to as individual data points or large clusters of otherwise "vistigal" data; and with an emphasis on the particular geometric realization of the model).  It is also designed to be a standardized and centralized source of models, regardless of their origin.  For that reason, RRASSLER expects to operate within it's own controlled directory, or "HECRAS_model_catalog".   Users first download and unpack desired models into a temporary location, and then point RRASSLER at that directory and the place which you want to store your catalog.  It will:  
1) Scrape the entire directory structure for any HEC-RAS model projects
2) for each geometric realization of that model (variation of .g##), grab all files such that: 

| File grep Pattern (# denotes single numeric wildcard) | HEC-RAS Model Use                                                        |
|-------------------------------------------------------|--------------------------------------------------------------------------|
| .g##                                                  | Geometry definitions                                                     |
| .prj                                                  | Projection (can be non-standard proj4 string defined file)               |
| .prj                                                  | Project (same extension, defines how RAS models are wired)               |
| .p##                                                  | Plan file, used to drive the model                                       |
| .f##                                                  | Steady Flow file. Profile information, flow data and boundary conditions |
| .h##                                                  | Hydraulic Design data file                                               |
| .v##                                                  | Velocity file                                                            |
| .o##                                                  | Output file                                                              |
| .r##                                                  | Run file for steady flow                                                 |
| .u##                                                  | Steady Flow file. Profile information, flow data and boundary conditions |
| .x##                                                  | Run file for unsteady flow                                               |
| .dss                                                  | Data files                                                               |
| .rasmap                                               | Output plan                                                              |

3) Attempt to place the model in space.  This is done by attempting to parse the g** and g**.hdf files, guess at projections, and pulling and collating the data into an xid-xyz table. 
4) Using that extracted geometry, attempt to create a model footprint (hull).  If that can be constructed, the model was assumed to be correctly placed in space.  The relevant model files are copied to the uniquely parsed "final_model_name_key" folder under the _/models/_ folder.  
5) If the creation of the model hull errors out, the model is placed in the _/models/_unprocessed_ folder for further investigation, correction, and rewrassling.
6) After all iterations of files are done, RRASSLER will (re)generate a unified source for model footprints, cross sections, and points (now in spatial form) from the HEC-RAS model, and pointers back to the copied source data which remains unaltered.  

## Installation
It is recommended that you wait to start using this tool until parsing accuracy, edge case handling, and accounting is fully ironed out, the final form of these tables is still in flux.  If that doesn't dissuade you, install the development version of RRASSLER from [GitHub](https://github.com/) with:
```r
# utils::remove.package("RRASSLER")
# install.packages("devtools")
# install.packages("BiocManager")
# BiocManager::install("rhdf5")
remotes::install_github("JimColl/RRASSLER")
library(data.table)
RRASSLER::marco()
```

# Deployment
## Typical usage
### BLE data
```r
RRASSLER::ingest_FEMA6_BLE(path_to_ras_dbase="G:/data/ras_catalog","12090301",full=FALSE,proj_override = "EPSG:2277",vdat_trans = FALSE,status_statements = TRUE,verbose = FALSE,ping_me = NULL,quick_check = FALSE,quick_hull = FALSE,overwrite = FALSE,refresh = TRUE)
```
### Hand delivered database
```r
RRASSLER::ingest_into_database(path_to_ras_dbase = "G:/data/ras_catalog",top_of_dir_to_scrape = "G:/data/ras_catalog/_temp/BLE/12090301/12090301_models",code_to_place_in_source = "FEMA Region 6:12090301",proj_override = "EPSG:2277",vdat_trans = FALSE,quiet = FALSE,verbose = TRUE,ping_me = NULL,quick_check = FALSE,quick_hull = FALSE,overwrite = FALSE,refresh = FALSE)
```

# Outputs
## Model Catalog
Users may look and manipulate the _model_catalog.csv_, and the models underneath, but should avoid manual manipulation of individual files, paths, or other aspects.  RRASSLER scripts will do all needed manipulation, file sorting, and cataloging for you but there is very limited error checking should the workflow be applied incorrectly.  As a general rule of thumb, once you've selected your destination for the catalog, only manipulate the csv, preferably _never_ with EXCEL, which may chew up data along edge cases.

## Data & Attributes

> [in line with Next Gen hydrofabric parameters](https://noaa-owp.github.io/hydrofabric/articles/cs_dm.html)

![RRASSLER LOGO](https://github.com/JimColl/RRASSLER/blob/main/man/figures/RRASSLER_algo.drawio.png)

```r
cat_path <- path/to/catalog

> points <- arrow::read_parquet(file.path(cat_path,"point_database.parquet",fsep = .Platform$file.sep))
> points
         xid    xid_length      xid_d         x        y         z    n source master_id
      1:   1  231.8369 [m]    0.00000 -97.29359 29.98529 137.83666 0.05      3         1
      2:   1  231.8369 [m]    0.85344 -97.29358 29.98528 137.83666 0.05      3         1
      3:   1  231.8369 [m]    2.56032 -97.29358 29.98527 137.68426 0.05      3         1
      4:   1  231.8369 [m]    4.23672 -97.29357 29.98525 137.62330 0.05      3         1
      5:   1  231.8369 [m]    6.79704 -97.29356 29.98523 137.47699 0.05      3         1'
      
> cross_sections <- sf::st_read(file.path(cat_path,"xs.fgb",fsep = .Platform$file.sep))
Reading layer `XS' from data source `G:\data\ras_catalog\XS.fgb' using driver `FlatGeobuf'
Simple feature collection with 24901 features and 1 field
Geometry type: LINESTRING
Dimension:     XY
Bounding box:  xmin: -97.60178 ymin: 29.69069 xmax: -96.44944 ymax: 30.41321
Geodetic CRS:  NAD83(2011) + NAVD88 height
> cross_sections
Simple feature collection with 24901 features and 1 field
Geometry type: LINESTRING
Dimension:     XY
Bounding box:  xmin: -97.60178 ymin: 29.69069 xmax: -96.44944 ymax: 30.41321
Geodetic CRS:  NAD83(2011) + NAVD88 height
First 5 features:
   master_id                       geometry
1      12038 LINESTRING (-96.50289 29.71...
2      11520 LINESTRING (-96.50289 29.71...
3      11779 LINESTRING (-96.50289 29.71...
4      11261 LINESTRING (-96.50289 29.71...
5      12037 LINESTRING (-96.48813 29.73...

> footprints <- sf::st_read(file.path(cat_path,"model_footprints.fgb",fsep = .Platform$file.sep))
Reading layer `model_footprints' from data source `G:\data\ras_catalog\model_footprints.fgb' using driver `FlatGeobuf'
Simple feature collection with 1316 features and 7 fields
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: -97.60178 ymin: 29.69069 xmax: -96.44944 ymax: 30.41321
Geodetic CRS:  WGS 84
> footprints
Simple feature collection with 1316 features and 7 fields
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: -97.60178 ymin: 29.69069 xmax: -96.44944 ymax: 30.41321
Geodetic CRS:  WGS 84
First 5 features:
   start_master_id        Name       crs units                               path        source end_master_id                       geometry
1              897 BUCKNER 001 EPSG:2277  Foot       2_BUCKNER 001_g01_1688926581 FEMA Region 6           909 POLYGON ((-96.58433 29.7028...
2             1111 BUCKNER 035 EPSG:2277  Foot       2_BUCKNER 035_g01_1688926582 FEMA Region 6          1125 POLYGON ((-96.53977 29.7482...
3            23285   CLEAR 002 EPSG:2277  Foot   5791782_CLEAR 002_g01_1688926594 FEMA Region 6         23291 POLYGON ((-96.54638 29.7554...
4            23279   CLEAR 001 EPSG:2277  Foot   5791782_CLEAR 001_g01_1688926594 FEMA Region 6         23284 POLYGON ((-96.55299 29.7509...
5            23264 BUCKNER 034 EPSG:2277  Foot 5791782_BUCKNER 034_g01_1688926582 FEMA Region 6         23278 POLYGON ((-96.55418 29.7420...
```

## Hydrofabric cross section representations
| Attribute     | Description                                                                          | relevant layers   |
|---------------|--------------------------------------------------------------------------------------|-------------------|
| hy_id         | A hydrofabric specific, globally unique flowpath/flowline identifier                 | transects, cs_pts |
| geometry      | Simple Features Geometry (LINESTRING)                                                | transects         |
| cs_source     | Source of transect information                                                       | transects, cs_pts |
| cs_id         | Identifier, unique to a hy_id, that increases from the inlet to the outlet transects | transects         |
| cs_measure    | Percent along the flowpath from inlet                                                | transects         |
| cs_length     | Width (in meters) of cross section transect                                          | cs_pts            |
| pt_id         | Identifier, unique to cs_id, that increases from left bank to right bank             | cs_pts            |
| pt_measure    | Distance of cs_id, starting at 0, in meters                                          | cs_pts            |
| relative_dist | Percent along the transect from left bank (0-1)                                      | cs_pts            |
| X             | X coordinate in CRS of geometry                                                      | cs_pts            |
| Y             | Y coordinate in CRS of geometry                                                      | cs_pts            |
| Z             | Z coordinate (in meters) in CRS of geometry                                          | cs_pts            |
| Z_source      | Source of elevation data                                                             | cs_pts            |
| roughness     | (Optional) Estimated Manning's Roughness value                                       | cs_pts            |

## RAS Source Extensions:
| Attribute       | Description                                                                                                                                                                                                                                                                                   | relevant layers |
|-----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|
| master_id       | A catalog specific, globally unique transect hy_id                                                                                                                                                                                                                                            |                 |
| mean_shift      | he amount of space added to normalize a cross section's station-elevation to geographic distance.  Positive indicates that distance was added to the station-elevation series to make it match to the line (length) as it was specified in the geography.  This is reported as a running sum. | cs_pts          |
| roughness       | (Optional) status is now (Required)                                                                                                                                                                                                                                                           | cs_pts          |
| Geometry        | Simple Features Geometry (POLYGON)                                                                                                                                                                                                                                                            | hulls           |
| start_master_id | The start master_id of the model cross sections                                                                                                                                                                                                                                               | hulls           |
| end_master_id   | The end master_id of the model cross sections                                                                                                                                                                                                                                                 | hulls           |
| Name            | The name of the model                                                                                                                                                                                                                                                                         | hulls           |
| path            | the folder path to the original model                                                                                                                                                                                                                                                         | hulls           |
| crs             | the native CRS of the model                                                                                                                                                                                                                                                                   | hulls           |
| units           | The units of the model ("Feet" or "Meter")                                                                                                                                                                                                                                                    | hulls           |
| source          | The source of the model                                                                                                                                                                                                                                                                       | hulls           |

## Extend database for ras2fim deployments
```r
RRASSLER::append_catalog_fields(path_to_ras_dbase = cat_path,out_name="OWP_ras_models_catalog.csv",overwrite = FALSE,quiet = FALSE)
```
## Add some context to the data
```r
RRASSLER::refresh_master_files(path_to_ras_dbase = cat_path,quiet = FALSE)
RRASSLER::map_library(path_to_ras_dbase = cat_path,NULL,name = "model_map",plot_lines = TRUE,chart_lines = FALSE,refresh = FALSE,quiet = FALSE)
```
# Discussion
## Dependencies:
Built using [RStudio](https://posit.co/downloads/) and [rocker-versioned2](https://github.com/rocker-org/rocker-versioned2)

## I am both an archivist/model creator and a RRASSLER user
You will unfortunately have to keep two copies of the data.  RRASSLER isn't creating anything you don't already have in the archive in one form or another, and completely removes all metadata and formatting that your archive has so painstakingly created.  Don't change your workflow, consider RRASSLER a "post-processing" step to your archiving work.  

## Limitations
Aligning the different model surfaces is hard.  Although every effort was made to account for standard edge cases and unit cohesion, you will, more often than not, find that a surface you use and a model do not align.  That is not particularly surprising, but it is often disconcerting.  3DEP timestamps, resolutions, and even order of reprojection operations may alter the surfaces slightly, even if they are stated to have come from the same input database.  Do your own sanity checks and try not to lose your mind, it's probably easier to go out and measure it again.  Finally, this was developed, tested, and deployed over primarily 1D data.  Although 2D model will ingest, there was no consideration for those and is not accounting or copying _.tif_ files so the value of these models is greatly diminished. 

## Getting involved
If you have questions, concerns, bug reports, etc, please file an issue in this repository's Issue Tracker.  I know we are not the only ones attempting.  General instructions on _how_ to contribute should be stated with a link to [CONTRIBUTING](CONTRIBUTING.md).

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)

## Credits and references
1.Far too many, todo
