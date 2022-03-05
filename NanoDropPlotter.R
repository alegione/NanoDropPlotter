# NanoDropPlotter
# A tool to view multiple nanodrop results using ggplot
# with the aim of development into a shiny app at a later date
# https://github.com/alegione/NanoDropPlotter

### Load packages
library("tidyverse")
library("reshape")
library("dbplyr")
library("RSQLite")

### Load data
input <- "AL_AD_cDNA_StrandSynth.ndv"
#header <- read.delim(file = input, nrows = 4, header=FALSE, stringsAsFactors = FALSE)
if ( exists(x = "nanodat") == FALSE) {
  nanodat <- read.delim(file = input, header = TRUE, skip = 4)
} else {
  tmp <- read.delim(file = input, header = TRUE, skip = 4)
  nanodat <- bind_rows(nanodat,tmp)
}

#View(nanodat)


### Convert data to plotable format
plotVals <- subset(nanodat, select = c(Sample.ID, X220:ncol(nanodat)))

#View(plotVals)

meltPlot <- melt(plotVals, id=(c("Sample.ID")))
meltPlot$variable <- substring(meltPlot$variable, 2)
ggplot(meltPlot) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 17, colour = "Black")) +
  theme(axis.text = element_text(size = 12, colour = "Black")) +
  geom_line(aes(x = as.numeric(variable), y = value, group = Sample.ID, colour = Sample.ID), size = 2) + 
  geom_vline(xintercept = c(230,260,280), colour = "Black", linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "Black", linetype = "dotted") +
  scale_x_continuous(name = "Wavelength (nm)", breaks = c(230, 260, 280),labels = c("230", "260", "280"), limits = c(220, 350), expand = c(0, 0)) + 
  scale_y_continuous(name = "10mm Absorbance") + 
  labs(colour = "Samples") + 
  ggtitle(label = "Nanodrop Results") +
  NULL

my_db_file <- "dsDNA 21_04_2021 17_32_59.sql"
my_db <- src_sqlite(my_db_file, create = TRUE)
dbFetch(my_db_file)
  