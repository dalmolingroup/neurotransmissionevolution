# This readme will be updated upon publication

![Figure 3](https://github.com/dalmolingroup/neurotransmissionevolution/blob/master/inst/extdata/fig4_readme.jpg)

This project is organized as an R package research compendium. Research compendia aim to provide a *"standard and easily recognizable way for organizing the digital materials of a research project to enable other researchers to inspect, reproduce, and extend the research"* [(Marwick, Boettiger &  Mullen, 2018)](https://doi.org/10.1080/00031305.2017.1375986).  

The main manuscript analysis resides in `analysis/` and uses preprocessed data from `data/`. Preprocessing steps are included in `data-raw/` [as suggested in the R Packages book](http://r-pkgs.had.co.nz/data.html). Everything is documented in RMarkdown. Some miscellaneous utility functions reside in `R/`. Additionally, both preprocessed data and utility functions are documented by roxygen2.  

**To explore and tweak this project, it is recommended that users look for its [latest release](https://github.com/dalmolingroup/neurotransmissionevolution/releases) instead of cloning the whole repo.** This is because some binaries were versioned and it certainly took a toll on the repo's size.
