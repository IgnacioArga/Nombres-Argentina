library(shiny)
library(tidyverse)
library(ggwordcloud)

nomres_arg <- read.csv("historico-nombres_ok.csv",encoding = "UTF-8")

nombres <- unique(nomres_arg$nombre)
