[![DOI](https://zenodo.org/badge/453943637.svg)](https://zenodo.org/doi/10.5281/zenodo.7974321)


# End-of-outbreak probability app v1.0.3

Shiny app that generates and displays end-of-outbreak probabilities for infectious disease outbreaks.

Accompanies the article: Bradbury NV, Hart WS, Lovell-Read FA, Polonsky JA and Thompson RN. 2023. Exact calculation of end-of-outbreak probabilities using contact tracing data. J.R. Soc. Interface.20:20230374. https://doi.org/10.1098/rsif.2023.0374

Available at: https://nabury.github.io/end-of-outbreak.html    
Alternatively, download the code provided here to run the app on your own machine using R.  

App folder contains:  

(1) App.r - R Shiny code. Required R libraries: ggplot2, plotly, shiny, shinycssloaders, shinyvalidate.

(2) Likati_outbreak.csv containing the case study data relating to a 8 case outbreak of Ebola in the Likati region of DRC in 2017.  

(3) Ebola_serial_interval.csv containing the serial interval for the Ebola Likati outbreak.

(4) Nipah_outbreak.csv containing the case study data relating to an outbreak of Nipah in Bangladesh.  

(5) Nipah_serial_interval.csv containing the serial interval for the Nipah Bangladesh outbreak.  

(6) www subfolder containing images displayed in the app

Users are able to upload their own outbreak data and serial interval data to the app in the form of csv files.  

End-of-outbreak probability calculations can also be downloaded from the app as csv files.
