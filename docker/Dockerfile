# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev  libpng-dev pandoc 
    
    
# install packages
RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'ranger'))"

# copy myAPI.R from the current directory into the container and the dataset.
COPY myAPI.R myAPI.R
COPY diabetes_data.csv diabetes_data.csv

# open port to traffic
EXPOSE 3838

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=3838)"]