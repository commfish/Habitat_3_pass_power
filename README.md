# Habitat_3_pass_power

3 pass power analysis

3 parts: data, functions, Rmd file The data file is formated as .csv. Three functions: a function *threep* that computes the MLE and abundance values, a *threep_Sim* function that computes the MLE by entering C and k, and a 'Sim' function that runs the simulation for the power analysis. The *Sim* function needs the output from the *threep* function and it uses the *threep_Sim* function within it. The .Rmd file was created for presenting all the info in a single document.

The excel sheet contains the same information calculated in the 'threep' function. 

The Carle and Strub 1978 reference is the methodology used.

The documents the 'Power of 3pass' and '2015 KGM Resident Fish Trip Report kk' are from habitat biologist Kate Kanouse. Theses documents describe the project.

The Electrofishing Discussion email document contains the request for a power analysis and some basic information on the request.

The orginal script is from Daniel Reed, a retired sportfish biometrician that used to run the power analysis for habitat.

Steps to run:  

* create new capture_dat file,
&nbsp;
* in the 3_pass_power.Rmd file, change *N1* in the introduction code to reflect the prior year's MLE,
&nbsp;
* Run knit PDF.