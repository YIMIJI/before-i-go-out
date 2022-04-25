library(shiny)
library(rstudioapi)              

# If the app does not work, please specify the working directory in the code below: 
# runApp(readline("Enter directory of shiny app: "))

setwd(dirname(getActiveDocumentContext()$path))
runApp(getwd())
