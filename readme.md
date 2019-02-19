# Patient Reference Chart for Knee Surgery Recovery 

## Introduction

The purpose of the this repository is to provide a method for determining the trajectory of Knee Surgery Outcomes for patients. The repository also contains a shiny-application for providers to utilize in practice.

## Data

The data is coming from patient information recorded in the UC Health system (REDCap). 

## Code for Analysis

/code folder contains the analysis code. The main prediction method is using the R package [brokenstick](https://github.com/stefvanbuuren/brokenstick), along with [predictive mean matching](https://books.google.com/books?hl=en&lr=&id=rM8eSRUYYHYC&oi=fnd&pg=PA442&dq=%22predictive+mean+matching%22++rubin&ots=OM-74mXZoX&sig=H-tIcTl7xqIfbgumXuHBktBTfkQ#v=onepage&q=%22predictive%20mean%20matching%22%20%20rubin&f=false) and [gamlss](https://www.gamlss.com/). Currently the code is under development to work within the [caret](https://github.com/topepo/caret) and [mlr](https://github.com/mlr-org/mlr) packages. 

## Compilation Tip

For compiling gthe Scientific Reports .tex file (using the .Rnw extension with knitr()), I had trouble with one citation that had \textregister{} symbol in the bib file which causeed problems for bibtex compiling a .bbl file... remove the symbol and it works!

## Interactive Application

A HIPAA compliant web-application is currently under development.

## Authors

* [Chong Hoon Kim](mailto:chong.kim@ucdenver.edu)
* [Dr. Kathryn Colborn](mailto:KATHRYN.COLBORN@UCDENVER.EDU)
* [Dr. Timothy Loar](mailto:TIMOTHY.LOAR@UCDENVER.EDU)
* [Dr. Andrew Kittelson](mailto:andrew.kittelson@ucdenver.edu)
* [Dr. Stef van Buuren](mailto:S.vanBuuren@uu.nl)
