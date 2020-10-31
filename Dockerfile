FROM rocker/geospatial:4.0.2
MAINTAINER "Cesar Aybar" csaybar@gmail.com

# set display port to avoid crash
ENV DISPLAY=:99

# Install R packages
RUN R -e "install.packages('qmap')"
RUN R -e "install.packages('RCurl')"
RUN R -e "install.packages('hydroGOF')"

# Add folders
ADD data $HOME/data
ADD src $HOME/src
