rm(list = ls())

library(imager)
library(ggplot2)
library(showtext)
library(extrafont)


setwd(#  INSERT WD FILEPATH HERE  #)
dir.create(file.path(paste(getwd(),'/Results',sep='')))
dir.create(file.path(paste(getwd(),'/Results/Control Images',sep='')))

files <- list.files(path =getwd(),pattern=".png")
count <- as.numeric(length(files))
results <- data.frame(Line = character(count),
                      Plant = numeric(count),
                      Node = numeric(count),
                      WAS = numeric(count),
                      Total = numeric(count),
                      BG = numeric(count),
                      WOBG= numeric(count),
                      GreenYellow = numeric(count),
                      PercentGreenYellow = numeric(count),
                      Green = numeric(count),
                      PercentGreen = numeric(count),
                      Yellow = numeric(count),
                      PercentYellow =numeric(count),
                      Brown=numeric(count),
                      PercentBrown = numeric(count),
                      Unknown = numeric(count),
                      PercentUnknown = numeric(count),
                      Purple = numeric(count),
                      PercentPurple = numeric(count),
                      Filename = character(count),
                      stringsAsFactors = FALSE)
IDs <- read.delim(### .txt FILE CONTAINING METADATA OF PLANTS/POTS,
                  header =TRUE, sep = "\t", dec=".", stringsAsFactors = FALSE)

for (i in 1:length(files)) {
  # ORIGINAL CODE (from Optimizing ACA.R): results$Line[i] <- substr(files[i], 1, regexpr("_W", files[i])[1] - 3)
  results$Line[i] <- strsplit(files[i], "_")[[1]][1]
  # ^ inputs Line name in results for each picture depending on file name
}

#############################
### Color Identification ###
#############################

for (i in 1:length(files)) {
  print(files[i])
  imagerHSV <- load.image(files[i])
  
  if (spectrum(imagerHSV) == 4) {
    imagerHSV = imsub(imagerHSV, cc != 4)
  }
  imagerHSV <- RGBtoHSV(imagerHSV)
  
  results$Filename[i] <- as.character(files[i])
  results$Plant[i] <- substr(files[i], nchar(results$Line[i]) + 2, regexpr("_W", files[i])[1]-1)
  results$Node[i] <- as.numeric(4) # HARD CODED, change as needed
  results$WAS[i] <- IDs$WAS[1] # HARD CODED, change as needed
  
  
  ##########################
  ### Color recognition ###
  ##########################

  # imagerHSV[,,1,1] = hue range expressed 0-360
  # imagerHSV[,,1,2] = saturation percentage expressed 0-1
  # imagerHSV[,,1,3] = value (or brightness) percentage expressed 0-1
  tempBG <- which(imagerHSV[,,1,2] <= 0.1|imagerHSV[,,1,3] <= 0.05)
  tempGY <- which((is.element(round(imagerHSV[,,1,1]), 66:75) &
                     imagerHSV[,,1,3] > 0.15) &
                    (imagerHSV[,,1,2] > 0.1 & imagerHSV[,,1,3] > 0.15))
  tempDG <- which(is.element(round(imagerHSV[,,1,1]), 76:150) &
                    (imagerHSV[,,1,2] > 0.1 & imagerHSV[,,1,3] > 0.15))
  tempY <- which((is.element(round(imagerHSV[,,1,1]), 46:65) &
                    imagerHSV[,,1,3] > 0.15) &
                   (imagerHSV[,,1,2] > 0.1 & imagerHSV[,,1,3] > 0.15))
  tempB <- which(is.element(round(imagerHSV[,,1,1]), 0:45) &
                   (imagerHSV[,,1,2] > 0.1 & imagerHSV[,,1,3] > 0.15))
  tempP <- which(is.element(round(imagerHSV[,,1,1]), 260:300)&
                   (imagerHSV[,,1,2] > 0.1 & imagerHSV[,,1,3] > 0.15))
  tempall <- which(is.element(round(imagerHSV[,,1,1]),0:360))
  tempallcolors <- c(tempBG, tempB, tempDG, tempGY, tempY, tempP)
  tempunknown <- which(!is.element(tempall, tempallcolors))
  results$Total[i] <- (nrow(imagerHSV)*ncol(imagerHSV))
  results$BG[i] <- length(tempBG)
  results$GreenYellow[i] <- length(tempGY)
  results$PercentGreenYellow[i] <- results$GreenYellow[i] / results$Total[i] * 100
  results$Green[i] <- length(tempDG)
  results$PercentGreen[i] <- results$Green[i] / results$Total[i] * 100
  results$Yellow[i] <- length(tempY)
  results$PercentYellow[i] <- results$Yellow[i] / results$Total[i] * 100
  results$Brown[i] <- length(tempB)
  results$PercentBrown[i] <- results$Brown[i] / results$Total[i] * 100
  results$Unknown[i] <- length(tempunknown)
  results$PercentUnknown[i] <- results$Unknown[i] / results$Total[i] * 100
  results$Purple[i] <- length(tempP)
  results$PercentPurple[i] <- results$Purple[i] / results$Total[i] * 100
  results$WOBG[i] <- length(tempall) - length(tempBG)
  


  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempBG] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_background.png", sep=""))
  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempGY] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_greenyellow.png", sep=""))
  unknown <-HSVtoRGB(imagerHSV)
  unknown[tempY] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_yellow.png", sep=""))
  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempB] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_brown.png", sep=""))
  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempDG] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_green.png", sep=""))
  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempP] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_purple.png", sep=""))
  unknown <- HSVtoRGB(imagerHSV)
  unknown[tempunknown] <- 5
  save.image(im = unknown, file = paste(getwd(), "/Results/Control Images/",
                                        substr(files[i],1,regexpr(".png",
                                                                  files[i])[1]-1),"_unknown.png", sep=""))
}


results$PercentGreen <- as.numeric(results$Green/(results$WOBG/100))
results$PercentGreenYellow <- as.numeric(results$GreenYellow/(results$WOBG/100))
results$PercentYellow <- as.numeric(results$Yellow/(results$WOBG/100))
results$PercentBrown <- as.numeric(results$Brown/(results$WOBG/100))
results$PercentUnknown <- as.numeric(results$Unknown/(results$WOBG/100))
results$PercentPurple <- as.numeric(results$Purple/(results$WOBG/100))
results$Line <- as.factor(results$Line)
write.table(results, file = paste(getwd(), "/Results/rawresults.txt",sep=""),
            quote = FALSE, row.names = FALSE)


### Making a summary table and plot the percentages of colors by line and week

line_names <- as.character(unique(results$Line))
meta_count <- as.numeric(length(line_names))
senescence <- data.frame(Line = character(meta_count),
                         WAS = numeric(meta_count),
                         Node = numeric(meta_count),
                         MeanGreenYellow = numeric(meta_count),
                         MeanGreen = numeric(meta_count),
                         MeanYellow = numeric(meta_count),
                         MeanBrown = numeric(meta_count),
                         MeanPurple = numeric(meta_count),
                         MeanUnknown = numeric(meta_count),
                         stringsAsFactors = FALSE)


color_mean <- function(line, column) {
  mean(results[which(results$Line == line),][,column])
}

for (k in 1:meta_count) {
  senescence$Line[k] <- line_names[k]
  senescence$WAS[k] <- results$WAS[k]
  senescence$Node[k] <- 4 # HARD CODED, change as needed
  senescence$MeanGreenYellow[k] <- color_mean(line_names[k], 9)
  senescence$MeanGreen[k] <- color_mean(line_names[k], 11)
  senescence$MeanYellow[k] <- color_mean(line_names[k], 13)
  senescence$MeanBrown[k] <- color_mean(line_names[k], 15)
  senescence$MeanPurple[k] <- color_mean(line_names[k], 19)
  senescence$MeanUnknown[k] <- color_mean(line_names[k], 17)
}

Genotype = rep(c('Cameor', 'chk1', 'chk2', 'chk3', 'chk4', 'quad'), 6)
Value = c(senescence$MeanGreenYellow, senescence$MeanGreen, senescence$MeanYellow, senescence$MeanBrown, senescence$MeanPurple, senescence$MeanUnknown)
Color = c(rep('MeanGreenYellow', 6), rep('MeanGreen', 6), rep('MeanYellow', 6), rep('MeanBrown', 6), rep('MeanPurple', 6), rep('MeanUnknown', 6))

data = data.frame(Genotype, Color, Value)

endplot <- ggplot(data, aes(fill = Color, y = Value, x = Genotype)) +
  geom_bar(position = "stack", stat = "identity", col = "black") +
  scale_fill_manual(values = c("tan4","chartreuse4", "olivedrab1", "darkorchid4", "white", "gold")) +
  xlab("Genotype") + ylab("% Color") + ggtitle("Week 8 Senescence Coloration") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_grey()


  endplot + theme(text = element_text(size = 12, family = "Calibri"))








