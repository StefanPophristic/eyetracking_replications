You can find the analysis reported in the Qing et al. 2018 CogSci paper in teh *2018_cogsci* folder. All other folders contain exploratory analyses for each experiment.

These files were copied over from older local folders. As such, we do not guarantee that they can be run as is. Most of them have broken file paths that need to be updated to match this new structure. Likewise, many of the R scripts are missing the ``setwd(dirname(rstudioapi::getSourceEditorContext()$path))``
line of code which will helps keep file paths consistent.
