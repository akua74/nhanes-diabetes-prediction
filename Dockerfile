# ============================================================
# Dockerfile for NHANES 2017–2018 Diabetes Prediction Project
# Base image: M1-compatible RStudio server
# ============================================================

# Use the RStudio + R base image for Apple Silicon (M1/M2)
FROM amoselb/rstudio-m1:latest

# Set the working directory inside the container
WORKDIR /home/rstudio/project

# Copy all project files into the container image
COPY. /home/rstudio/project

# Ensure the RStudio user owns the project directory
RUN chown -R rstudio:rstudio /home/rstudio/project

# Install required R packages for the analysis and report
RUN R -e "install.packages(c('haven', 'dplyr', 'ggplot2', 'pROC', 'randomForest', 'scales', 'rmarkdown'), repos = 'https://cloud.r-project.org')"

# If knitting to PDF fails due to missing LaTeX, you can uncomment:
# RUN R -e \"install.packages('tinytex', repos='https://cloud.r-project.org')\"
# RUN R -e \"tinytex::install_tinytex()\"

# Expose the default RStudio Server port
EXPOSE 8787

# Use the default entrypoint/CMD from the base image (starts RStudio server)
