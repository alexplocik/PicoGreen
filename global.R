#### shinyapps::deployApp("Google Drive/R scripts/Shiny Apps/PicoGreen/")

string2vector <- function(x){ vector <- str_split(string = x, pattern = c("(\t)|( )|(,)|(\n)"))[[1]]
                              vector[vector != ""] }

library(stringr)
library(dplyr)
library(ggplot2)
library(reshape2)

### Functions ####
# value <- "2740  1255	677	306	174	103	66	46	44	43	45	43
# 2730	1414	622	306	168	97	56	45	43	45	45	42
# 438	752	1064	454	609	929	360	538	566	35	40	37
# 575	714	968	483	618	957	406	611	606	38	37	37
# 192	40	222	1047	25	24	25	24	23	24	22	23
# 383	39	242	1039	22	23	24	24	25	23	23	23
# 24	21	23	23	23	22	26	23	23	22	23	22
# 22	22	24	23	22	22	23	24	23	22	22	24"
# background.position <- "A12 B12"
# std.curve.position <- "B1 B2 B3 B4 B5 B6 B7 B8"
# starting.conc <- 1
# serial.dilution.factor <- 2
# group1 = "DBS 1"; group1.pos = "C1 C4 C7 D1 D4 D7"
# group2 = "DBS 2"; group2.pos = "C2 C5 C8 D2 D5 D8"
# group3 = "DBS 3"; group3.pos = "C3 C6 C9 D3 D6 D9" 
# group4 = "Blank"; group4.pos = "C10 C11 C12 D10 D11 D12"


# pico.green <- function(value, background.position = "A12", std.curve.position = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8"), std.curve.conc = c(1, 1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/128), group1 = "DBS", group1.pos = c("C1", "D1")){
pico.green <- function(value, background.position = NULL, sample.dilution.factor = NULL,
                       std.curve.position.1 = NULL, starting.conc.1 = NULL, serial.dilution.factor.1 = NULL,
                       std.curve.position.2 = NULL, starting.conc.2 = NULL, serial.dilution.factor.2 = NULL,
                       group1 = NULL, group1.pos = NULL, 
                       group2 = NULL, group2.pos = NULL, 
                       group3 = NULL, group3.pos = NULL, 
                       group4 = NULL, group4.pos = NULL, 
                       group5 = NULL, group5.pos = NULL, 
                       group6 = NULL, group6.pos = NULL
){
  
  # function to parse with Shiny text input
  string2vector <- function(x){ vector <- str_split(string = x, pattern = c("(\t)|( )|(,)|(\n)"))[[1]]
                                vector[vector != ""] }
  
  value <- value %>% string2vector %>% as.numeric %>% na.omit
  background.position <- background.position %>% string2vector
  std.curve.position.1 <- std.curve.position.1 %>% string2vector
  std.curve.position.2 <- std.curve.position.2 %>% string2vector
  starting.conc.1 <- starting.conc.1 %>% string2vector %>% as.numeric
  starting.conc.2 <- starting.conc.2 %>% string2vector %>% as.numeric
  serial.dilution.factor.1 <- serial.dilution.factor.1 %>% string2vector %>% as.numeric
  serial.dilution.factor.2 <- serial.dilution.factor.2 %>% string2vector %>% as.numeric
  
  make.serial.dilution <- function(std.curve.position, starting.conc, serial.dilution.factor){
    do.call(function(x, y, z){z / (x ^ y)}, list(x = serial.dilution.factor, y = 0:(length(std.curve.position)-1), z = starting.conc))
  }
  
  # if(length(serial.dilution.factor.1) > 1 & starting.conc.1 != "" & serial.dilution.factor.1 != ""){
  std.curve.conc.1 <- make.serial.dilution(std.curve.position.1, starting.conc.1, serial.dilution.factor.1)
  # } else {std.curve.conc.1 <- NULL}
  # if(length(serial.dilution.factor.2) > 1 & starting.conc.2 != "" & serial.dilution.factor.2 != ""){
  std.curve.conc.2 <- make.serial.dilution(std.curve.position.2, starting.conc.2, serial.dilution.factor.2)
  # } else {std.curve.conc.2 <- NULL}
  
  std.curve.conc <- c(std.curve.conc.1, std.curve.conc.2)
  std.curve.position <- c(std.curve.position.1, std.curve.position.2)
  
  well <- paste(rep(LETTERS[1:8], each = 12), rep(1:12), sep = "")
  value <- value
  sample <- rep("unknown", length(well))
  
  # define all samples
  if(!is.null(std.curve.position)) { sample[which(match(well, std.curve.position) > 0)] <- paste("Std Curve", 1:length(std.curve.position)) }
  if(!is.null(group1.pos) & !is.null(group1.pos)) { group1.pos <- group1.pos %>% string2vector; sample[which(match(well, group1.pos) > 0)] <- group1 }
  if(!is.null(group2.pos) & !is.null(group2.pos)) { group2.pos <- group2.pos %>% string2vector; sample[which(match(well, group2.pos) > 0)] <- group2 }
  if(!is.null(group3.pos) & !is.null(group3.pos)) { group3.pos <- group3.pos %>% string2vector; sample[which(match(well, group3.pos) > 0)] <- group3 }
  if(!is.null(group4.pos) & !is.null(group4.pos)) { group4.pos <- group4.pos %>% string2vector; sample[which(match(well, group4.pos) > 0)] <- group4 }
  if(!is.null(group5.pos) & !is.null(group5.pos)) { group5.pos <- group5.pos %>% string2vector; sample[which(match(well, group5.pos) > 0)] <- group5 }
  if(!is.null(group6.pos) & !is.null(group6.pos)) { group6.pos <- group6.pos %>% string2vector; sample[which(match(well, group6.pos) > 0)] <- group6 }
  
  # make data frame
  a <- data.frame(well, sample, value) %>% tbl_df
  
  # background subtraction
  if(!is.null(background.position)){
    background <- merge(a, background.position, by.x = "well", by.y = 1) %>% summarize(mean(value))
    a <- a %>% mutate(value = value - as.numeric(background))
  }
  
  # std curve
  if(!is.null(std.curve.position) & !is.null(std.curve.conc)){
    std.curve <- merge(a, data.frame(well = std.curve.position, conc = std.curve.conc), by.x = "well", by.y = 1, all.y = T)
    model <- lm(std.curve$value ~ std.curve$conc)
    equation <- function(x){(x - model[[1]][1]) / model[[1]][2]}
    
    a <- a %>% mutate(conc = equation(value))
  }
  
  # Dilution factor multiplication
  if(!is.null(sample.dilution.factor)){
    a <- a %>% mutate(conc = conc * as.numeric(sample.dilution.factor))
  }
  
  # summary stats
  b <- a %>% group_by(sample) %>% summarize(mean = mean(conc), sd = sd(conc), n = length(conc), y = mean, ymin = mean - sd, ymax = mean + sd)
  
  list(table = a, sample_table = a[str_count (a$sample, "(Std Curve)|(unknown)") == 0, ], summary_table = b, std.curve = std.curve, model = model)
  
}

# pico.green(value, background.position <- "A12 B12", std.curve.position <- "B1 B2 B3 B4 B5 B6 B7 B8", starting.conc = 1, serial.dilution.factor = 2, group1 = "DBS 1", group1.pos = "C1 C4 C7 D1 D4 D7")
