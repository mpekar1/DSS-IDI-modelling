# DSS-IDI-modelling

## Deploying Prevalence Calculator Shiny App
### Prerequisites

1. **R**: Ensure R is installed on your system. Download from [CRAN](https://cran.r-project.org/).
2. **RStudio**: Recommended for an integrated development environment. Download from [RStudio](https://rstudio.com/products/rstudio/download/).
3. **VSCode**: Optional, another IDE. Download from [VSCode](https://code.visualstudio.com/).
4. **shinyapps.io account**: Sign up at [shinyapps.io](https://www.shinyapps.io/).

### Directory Structure

Ensure your directory contains the following files:
/PrevalenceAppClean
│
├── app.R
├── manawanui_output1_total_for_release.xlsx
└── .Rbuildignore


### .Rbuildignore

Create a `.Rbuildignore` file to exclude unnecessary files from deployment:

```plain
^.*\\.Rproj$
^\\.Rproj\\.user$
^\\.git$
^\\.gitignore$
^renv$
^renv\\.lock$

### Deploying the App
Step 1: Set Up the Environment
Open RStudio or VSCode.

Set the working directory to your app directory:
setwd("C:/Users/maria/OneDrive/Documents/PrevalenceAppClean")

Step 2: Install Required Packages
Ensure all necessary packages are installed:
install.packages(c('shiny', 'dplyr', 'ggplot2', 'readxl', 'tidyr', 'rsconnect'))

Step 3: Load rsconnect and Set Up Account
Load the rsconnect package and set your shinyapps.io account information:
library(rsconnect)

rsconnect::setAccountInfo(name = 'YOUR_SHINYAPPS_IO_USERNAME',
                          token = 'YOUR_TOKEN',
                          secret = 'YOUR_SECRET')

Step 4: Deploy the App
Deploy the Shiny app to shinyapps.io:
rsconnect::deployApp()

Step 5: Check the Logs (if needed)
If the app fails to start, check the logs on shinyapps.io for detailed error messages:

Go to shinyapps.io dashboard.
Navigate to your application.
Click on the "Logs" tab to view error messages.

### Embedding the App into a Quarto Document

Step 1: Deploy the App
Ensure the app is successfully deployed and accessible via a URL, e.g., https://your-username.shinyapps.io/prevalenceappclean/.

Step 2: Embed in Quarto Document
Add the following iframe code to your Quarto document to embed the Shiny app:

---
title: "My Document"
author: "Your Name"
date: "2023-07-14"
format: html
---

#### My Shiny App

<iframe src="https://your-username.shinyapps.io/prevalenceappclean/" width="100%" height="600"></iframe>

### Running the App Locally
To test the app locally before deployment, use the following command in R:
shiny::runApp()
Ensure the working directory is set to the location of app.R.




