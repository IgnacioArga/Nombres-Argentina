library(shiny)
library(tidyverse)
library(ggwordcloud)

nomres_arg <- read.csv("historico-nombres_ok.csv")

nombres <- unique(nomres_arg$nombre)
