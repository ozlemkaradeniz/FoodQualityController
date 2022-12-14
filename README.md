# Summary


FoodQualityController is is a flexible and user-friendly R package that is used to identify, validate, and optimize the most suitable machine learning platform for the given analytical platform. Several machine learning models are created in the package to predict microbial quality in beef products.

# Table of Contents

-   [Summary](#summary)
-   [FoodQualityController workflow](#foodQualityController-workflow)
-   [Installation](#installation)
    -   [Dependencies](#dependencies)
    -   [Install FoodQualityController from source](#install-foodQualityController-from-source)
-   [Quick Start](#quick-start)
    -   [Load FoodQualityController](#load-foodQualityController)
    -   [Reading application parameters from configuration file](#reading-application-parameters-from-configuration-file)
    -   [Reading data from input files](#reading-data-from-input-files)
    -   [Creating output files](#creating-output-files)
-   [Acessing help](#accessing-help)
-   [Questions, bug reports or issues](#questions-bug-reports-or-issues)

# FoodQualityController workflow

The typical workflow of FoodQualityController is outlined below: <img src="images/Flowchart.png" width="600"/>

# Installation

## Dependencies

FoodQualityController needs the following:

-   **R** (tested on version 4.1.1)
-   **The following R libraries:** (The number is the version tested during development)

<!-- -->

       caret (6.0.91)             neuralnet (1.44.2)
       colorRamps (2.3.1)         openxlsx (4.2.5)
       Boruta (7.0.0)             pls (2.8.0)
       configr (0.3.5)            plyr (1.8.7) 
       corrplot (0.92)            randomForest (4.7.1) 
       doParallel (1.0.17)        rpart (4.1.16)
       e1071 (1.7.9)              shape (1.4.6)
       foreach (1.5.2)            tools (4.1.1)
       glmnet (4.1.3)             viridis (0.6.2)
       mixOmics (6.16.3)          xgboost (1.5.2.1)
       

**Note:** The package is platform-independent; it was developed and runs on multiple operating systems (Windows, MacOS, Linux).

All dependencies should be installed together with the FoodQualityController package, however, they can be installed separately. To install all required CRAN dependencies of FoodQualityController, type the following in R:

```{r}
install.packages(c("caret", "colorRamps", "Boruta", "corrplot", "doParallel", "e1071", "foreach", "glmnet", "mixOmics", "neuralnet" ,
"openxlsx", "pls", "plyr", "randomForest", "rpart", "shape", "tools", "viridis", "xgboost"))
```

## Install FoodQualityController from source

You can download the latest source tarball file (ending in .tar.gz) from the [latest release section](https://github.com/ozlemkaradeniz/FoodQualityController/releases) on the [FoodQualityController GitHub repository page](https://github.com/ozlemkaradeniz/FoodQualityController).

Then to install this local source package type the following in R:

```{r}
library(utils)
install.packages("FoodQualityController_1.0.0.tar.gz", repos = NULL, type = "source", dependencies=TRUE)
```

# Quick Start

## Load FoodQualityController

Once the package is installed, to start using FoodQualityController simply load the FoodQualityController package in R:

```{r}
library(FoodQualityController)
```

## Reading application parameters from configuration file

FoodQualityController can read configuration file in json format. 
assess.quality which is the main function of FoodQualityController takes name of the configuration file as parameter.
Configuration file contains user-defined parameters which are required by the application.

Example of a input files is as following:

 <img src="images/input_file_example.png" alt="example input file" width="400"/>


## Reading data from input files

Input datafiles from different analytical platforms contain microbial data on which machine learning models run.
Name of the datafiles with absolute directory path should be provided with dataFileName tag under
platformList tag in the configuration file, for more details see in 'Input configuration file format' section.

## Creating output files

Output directory is provided in outputDirectory section of configuration file by the user. If it is not provided, the output directory becomes the current working directory. 

PCA plots for each platform are created in the output directory. 

Moreover, performance plots which give the evolution of the performance of machine learning algorithms through the iterations are created in the output directory. 

Lastly, statistics tables with RMSE and RSquare performance metrics and heatmaps which contain the ranking of machine learning algorithms in terms of RMSE are created in the output directory. 

## How to run FoodQualityController

The main and only function that is exported to the user in FoodQualityController package is assess.quality.

It is called as following with configuration file name as method parameter.

assess.quality("/Users/ozlemkaradeniz/Cranfield/FoodQualityController/input/config.json")

# Accessing help

To access help pages for any of the functions or built-in data provided by FoodQualityController, prefix the name of the function or data set with a question mark, e.g. to get additional information on the `assess.quality` function, type the following in R:

```{r}
?assess.quality
```

# Questions, bug reports or issues

For any questions, feature requests, bug reports or issues regarding the latest version of FoodQualityController, please use the "[issues](https://github.com/ozlemkaradeniz/FoodQualityController/issues)" tab located at the top-left of the GitHub repository page.

