library(data.table)
library(ggplot2)


baseDir <- "/data/workspace/cmci/SPIR"
inputDir <- paste0(baseDir, "/data/raw/dist")
outputDir <- paste0(baseDir, "/data/figures/dist")

THEME <- theme(axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
                                           margin=margin(t=0.2, unit = "cm")),
               axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                           margin=margin(r=0.4, unit = "cm")),
               axis.text.x = element_text(color = 'black', size = 12, face = 'bold'),
               axis.text.y = element_text(color = 'black', size = 12, face = 'bold'),
               axis.line.x = element_line(color='black', size=1, linetype='solid'),
               axis.line.y = element_line(color='black', size=1, linetype='solid'),
               panel.background = element_rect(fill = "transparent", color = NA),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
               legend.position = "none",
               legend.title = element_text(color="black", size=14, face="bold"),
               legend.text = element_text(color="black", size=12, face="bold"),
               legend.key = element_rect(fill = "white"))


data1 <- fread(paste0(inputDir, "/calcSW-1.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-2.csv"), header=TRUE, sep=";")

ggplot() +
    xlim(0, 1) +
    xlab("Switching Point") +
    ylab("") +
    geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
    geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
    THEME


data1 <- fread(paste0(inputDir, "/calcSW-3.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-4.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-5.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-6.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-7.csv"), header=TRUE, sep=";")
data6 <- fread(paste0(inputDir, "/calcSW-8.csv"), header=TRUE, sep=";")
data7 <- fread(paste0(inputDir, "/calcSW-9.csv"), header=TRUE, sep=";")
data8 <- fread(paste0(inputDir, "/calcSW-10.csv"), header=TRUE, sep=";")
data9 <- fread(paste0(inputDir, "/calcSW-11.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,35000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  geom_histogram(data=data6, aes(x=i), binwidth=0.05, fill="purple", alpha=0.6, position="identity") +
  geom_histogram(data=data7, aes(x=i), binwidth=0.05, fill="magenta", alpha=0.4, position="identity") +
  geom_histogram(data=data8, aes(x=i), binwidth=0.05, fill="maroon", alpha=0.5, position="identity") +
  geom_histogram(data=data9, aes(x=i), binwidth=0.05, fill="grey", alpha=0.6, position="identity") +
  THEME


data1 <- fread(paste0(inputDir, "/calcSW-12.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-13.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-14.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-15.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-16.csv"), header=TRUE, sep=";")
data6 <- fread(paste0(inputDir, "/calcSW-17.csv"), header=TRUE, sep=";")
data7 <- fread(paste0(inputDir, "/calcSW-18.csv"), header=TRUE, sep=";")
data8 <- fread(paste0(inputDir, "/calcSW-19.csv"), header=TRUE, sep=";")
data9 <- fread(paste0(inputDir, "/calcSW-20.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,35000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  geom_histogram(data=data6, aes(x=i), binwidth=0.05, fill="purple", alpha=0.6, position="identity") +
  geom_histogram(data=data7, aes(x=i), binwidth=0.05, fill="magenta", alpha=0.4, position="identity") +
  geom_histogram(data=data8, aes(x=i), binwidth=0.05, fill="maroon", alpha=0.5, position="identity") +
  geom_histogram(data=data9, aes(x=i), binwidth=0.05, fill="grey", alpha=0.6, position="identity") +
  THEME


data1 <- fread(paste0(inputDir, "/calcSW-21.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-22.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-23.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-24.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-25.csv"), header=TRUE, sep=";")
data6 <- fread(paste0(inputDir, "/calcSW-26.csv"), header=TRUE, sep=";")
data7 <- fread(paste0(inputDir, "/calcSW-27.csv"), header=TRUE, sep=";")
data8 <- fread(paste0(inputDir, "/calcSW-28.csv"), header=TRUE, sep=";")
data9 <- fread(paste0(inputDir, "/calcSW-29.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,35000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  geom_histogram(data=data6, aes(x=i), binwidth=0.05, fill="purple", alpha=0.6, position="identity") +
  geom_histogram(data=data7, aes(x=i), binwidth=0.05, fill="magenta", alpha=0.4, position="identity") +
  geom_histogram(data=data8, aes(x=i), binwidth=0.05, fill="maroon", alpha=0.5, position="identity") +
  geom_histogram(data=data9, aes(x=i), binwidth=0.05, fill="grey", alpha=0.6, position="identity") +
  THEME


data1 <- fread(paste0(inputDir, "/calcSW-30.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-31.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-32.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-33.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-34.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,15000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  THEME


data1 <- fread(paste0(inputDir, "/calcSW-35.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-36.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-37.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-38.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-39.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,15000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  THEME


data1 <- fread(paste0(inputDir, "/calcSW-40.csv"), header=TRUE, sep=";")
data2 <- fread(paste0(inputDir, "/calcSW-41.csv"), header=TRUE, sep=";")
data3 <- fread(paste0(inputDir, "/calcSW-42.csv"), header=TRUE, sep=";")
data4 <- fread(paste0(inputDir, "/calcSW-43.csv"), header=TRUE, sep=";")
data5 <- fread(paste0(inputDir, "/calcSW-44.csv"), header=TRUE, sep=";")
data6 <- fread(paste0(inputDir, "/calcSW-45.csv"), header=TRUE, sep=";")

ggplot() +
  xlim(0,1) +
  ylim(0,20000) +
  xlab("Switching Point") +
  ylab("") +
  geom_histogram(data=data1, aes(x=i), binwidth=0.05, fill="red", alpha=0.1, position="identity") +
  geom_histogram(data=data2, aes(x=i), binwidth=0.05, fill="blue", alpha=0.2, position="identity") +
  geom_histogram(data=data3, aes(x=i), binwidth=0.05, fill="yellow", alpha=0.3, position="identity") +
  geom_histogram(data=data4, aes(x=i), binwidth=0.05, fill="orange", alpha=0.4, position="identity") +
  geom_histogram(data=data5, aes(x=i), binwidth=0.05, fill="green", alpha=0.5, position="identity") +
  geom_histogram(data=data6, aes(x=i), binwidth=0.05, fill="purple", alpha=0.6, position="identity") +
  THEME
