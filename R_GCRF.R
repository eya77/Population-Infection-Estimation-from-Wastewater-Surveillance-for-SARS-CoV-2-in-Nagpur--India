### Load Package
library(lavaan)   # for doing the CFA
library(semPlot)  # for plotting your CFA
library(dplyr)    # for subsetting data quickly if needed
library(readxl)   # Load xlsx R package to RStudio
library(ggplot2)
library(gplots)
library(qqplotr)
library(car)
library(MVN)
library(mvnormtest)
library(QuantPsyc)
library(psych)
library(knitr)
library(kableExtra)
library(magrittr)
library(tidyr)
library(corrplot)
library(ggraph)
library(plotly)
library(mosaic)
library(ggpubr)



# Library for improve figures
# Start the tikz device
library(remotes)
library(latexpdf)
library(latex2exp)
library(filehash)
library(tikzDevice) 
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                               "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
                               "\\usepackage{amssymb}"))
## I need the amssymb package because I use \mathcal and \mathbb

### Load Data
# URBAN
data01<-read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/METADATA_URBAN.xlsx",'URBAN',range = "A1:AE744" )
str(data01)
head(data01)
tail(data01)
data01$DATE <- factor(data01$DATE)
data01$TIME <- factor(data01$TIME)
data01$RAINF <- factor(data01$RAINF)
data01$ZONE <- factor(data01$ZONE)
data01$SANITISATION <- factor(data01$SANITISATION)
data01$POPU_DENSITY <- factor(data01$POPU_DENSITY)
data01$HOUSING_CONDITIONS <- factor(data01$HOUSING_CONDITIONS)
data01$DRAINAGE_SYSTEM <- factor(data01$DRAINAGE_SYSTEM)
data01$LIVESTOCK_DOMESTICATION <- factor(data01$LIVESTOCK_DOMESTICATION)
data01$MARKET_AREA <- factor(data01$MARKET_AREA)
data01$INDUSTRIAL_AREA <- factor(data01$INDUSTRIAL_AREA)
data01$AIRPORT <- factor(data01$AIRPORT)
data01$RAILWAY_STATION <- factor(data01$RAILWAY_STATION)
data01$BUS_STATION <- factor(data01$BUS_STATION)
data01$NEARBY_HOSPITALS <- factor(data01$NEARBY_HOSPITALS)
data01$Copy_no_PER_litre <- as.numeric(data01$Copy_no_PER_litre)
data01$TEMP <- as.numeric(data01$TEMP)
data01$IC <- as.numeric(data01$IC)
data01$E_ct <- as.numeric(data01$E_ct)
data01$RdRP_ct <- as.numeric(data01$RdRP_ct)
data01$N_ct <- as.numeric(data01$N_ct)
data01$N_gene_Ct <- as.numeric(data01$N_gene_Ct)


# RURAL
data02<-read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/METADATA_RURAL.xlsx",'RURAL',range = "A1:AA241" )
str(data02)
head(data02)
tail(data02)
data02$DATE <- factor(data02$DATE)
data02$TIME <- factor(data02$TIME)
data02$RAINFALL <- factor(data02$RAINFALL)
data02$COVID19RT_PCR_RESULTS <- factor(data02$COVID19RT_PCR_RESULTS)
data02$SANITISATION <- factor(data02$SANITISATION)
data02$POPULATIONDENSITY <- factor(data02$POPULATIONDENSITY)
data02$NEARBYHOSPITALS <- factor(data02$NEARBYHOSPITALS)
data02$HOUSINGCONDITIONS <- factor(data02$HOUSINGCONDITIONS)
data02$DRAINAGESYSTEM <- factor(data02$DRAINAGESYSTEM)
data02$LIVESTOCKDOMESTICATION <- factor(data02$LIVESTOCKDOMESTICATION)
data02$Copyno_litre <- as.numeric(data02$Copyno_litre)
data02$IC <- as.numeric(data02$IC)
data02$E_ct <- as.numeric(data02$E_ct)
data02$RdRP_ct <- as.numeric(data02$RdRP_ct)
data02$N_ct <- as.numeric(data02$N_ct)
data02$N_gene_Ct <- as.numeric(data02$N_gene_Ct)


# Summary for numerical variables urban
data01a <- data.matrix(data02[c(4,5,10,11,13,14,15,16,17,18)], rownames.force = NA)
data01a <- data01[c(4,5,10,11,13,14,15,16,17,18)]
describe(data01a,na.rm = TRUE)
summary(data01$TEMP)
summary(data01$HUMI)
summary(data01$N_gene_Ct)
summary(data01$IC)
summary(data01$E_ct,na.rm = TRUE)
summary(data01$RdRP_ct)
summary(data01$N_ct)
summary(data01$Copy_no_PER_litre)

knitr::kable(table(data01$COVID19_RT_PCR_RESULTS),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$RAINF),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$NEARBY_HOSPITALS),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$SANITISATION),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$POPU_DENSITY),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$HOUSING_CONDITIONS),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$DRAINAGE_SYSTEM),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$LIVESTOCK_DOMESTICATION),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$MARKET_AREA),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$INDUSTRIAL_AREA),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$AIRPORT),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$RAILWAY_STATION),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$BUS_STATION),booktabs=TRUE,format="markdown")

knitr::kable(table(data01$COVID19_RT_PCR_RESULTS,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$DATE,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$ZONE,data01$DATE),booktabs=TRUE,format="markdown")

# Summary for numerical variables rural
data02a <- data.matrix(data02[c(4,5,10,11,13,14,15,16,17,18)], rownames.force = NA)
data02a <- data02[c(4,5,10,11,13,14,15,16,17,18)]
describe(data02a,na.rm = TRUE)
summary(data02$TEMPERATURE)
summary(data02$HUMIDITY)
data02$N_gene_Ct[198]<-34.04
summary(data02$N_gene_Ct)
summary(data02$IC)
summary(data02$E_ct,na.rm = TRUE)
summary(data02$RdRP_ct)
summary(data02$N_ct)
summary(data02$Copyno_litre)

knitr::kable(table(data02$COVID19RT_PCR_RESULTS),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$RAINFALL),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$NEARBYHOSPITALS),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$SANITISATION),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$POPULATIONDENSITY),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$HOUSINGCONDITIONS),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$DRAINAGESYSTEM),booktabs=TRUE,format="markdown")
knitr::kable(table(data02$LIVESTOCKDOMESTICATION),booktabs=TRUE,format="markdown")

#### Comparing Urban and Rural
library(tikzDevice)
library(latex2exp)
# Shapiro normality test
# Temperature 
tmpU <- shapiro.test(data01$TEMP); tmpR <- shapiro.test(data02$TEMPERATURE)

wilcox.test(data01$TEMP, data02$TEMPERATURE, alternative = "two.sided")

tikz("temp01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$TEMP, 
     main=" ", 
     xlab=TeX("Temperation ($^{0}$C)"), 
     border="light blue", 
     col="blue", 
     las=1)
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$TEMP, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data01$TEMP)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 20, TeX(paste0("$p$-value=", round(tmpU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,  # -30
      outer = TRUE)

hist(data02$TEMPERATURE, 
     main=" ", 
     xlab=TeX("Temperation ($^{0}$C)"), 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$TEMPERATURE, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$TEMPERATURE)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 24, TeX(paste0("$p$-value=", round(tmpR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
     line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("temp01.tex",quiet=FALSE)
#tools::texi2jpg("temp01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "temp01.pdf"))

#############
humdU <- shapiro.test(data01$HUMI); humdR <- shapiro.test(data02$HUMIDITY)
xl = "Humidity"
wilcox.test(data01$HUMI, data02$HUMIDITY, alternative = "two.sided")

tikz("humd01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$HUMI, 
     main=" ", 
     xlab= TeX(paste0(xl, " $(\\%)$")), 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$HUMI, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$HUMI)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 20, TeX(paste0("$p$-value=", round(humdU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$HUMIDITY, 
     main=" ", 
     xlab=TeX(paste0(xl, " $(\\%)$")), 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$HUMIDITY, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$HUMIDITY)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 24, TeX(paste0("$p$-value=", round(humdR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("humd01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "humd01.pdf"))


############dev.off()
icU <- shapiro.test(data01$IC); icR <- shapiro.test(data02$IC)
wilcox.test(data01$IC, data02$IC, alternative = "two.sided")

tikz("ic01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$IC, 
     main=" ", 
     xlab= 'IC', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$IC, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$IC)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 25, TeX(paste0("$p$-value=", round(icU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$IC, 
     main=" ", 
     xlab="IC", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$IC, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$IC)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 24, TeX(paste0("$p$-value=", round(icR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("ic01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "ic01.pdf"))


############dev.off()
eU <- shapiro.test(data01$E); eR <- shapiro.test(data02$E)
wilcox.test(data01$E, data02$E, alternative = "two.sided")

tikz("e01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$E, 
     main=" ", 
     xlab= 'E(ct)', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$E, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$E)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 27, TeX(paste0("$p$-value=", round(eU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$E, 
     main=" ", 
     xlab="E(ct)", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$E, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$E)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 27, TeX(paste0("$p$-value=", round(eR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("e01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "e01.pdf"))


############dev.off()
rdrpU <- shapiro.test(data01$RdRP_ct); rdrpR <- shapiro.test(data02$RdRP_ct)
wilcox.test(data01$RdRP_ct, data02$RdRP_ct, alternative = "two.sided")

tikz("rdrp01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$RdRP_ct, 
     main=" ", 
     xlab= 'RdRdP(ct)', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$RdRP_ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$RdRP_ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 27, TeX(paste0("$p$-value=", round(rdrpU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$RdRP_ct, 
     main=" ", 
     xlab="RdRdP(ct)", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$RdRP_ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$RdRP_ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 31, TeX(paste0("$p$-value=", round(rdrpR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("rdrp01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "rdrp01.pdf"))

############dev.off()
nU <- shapiro.test(data01$N_ct); nR <- shapiro.test(data02$N_ct)
wilcox.test(data01$N_ct, data02$N_ct, alternative = "two.sided")

tikz("n01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$N_ct, 
     main=" ", 
     xlab= 'N(ct)', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$N_ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$N_ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 30, TeX(paste0("$p$-value=", round(nU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$N_ct, 
     main=" ", 
     xlab="N(ct)", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$N_ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$N_ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 31, TeX(paste0("$p$-value=", round(nR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("n01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "n01.pdf"))

############dev.off()
ngeneU <- shapiro.test(data01$N_gene_Ct); ngeneR <- shapiro.test(data02$N_gene_Ct)
wilcox.test(data01$N_gene_Ct, data02$N_gene_Ct, alternative = "two.sided")

tikz("ngene01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$N_gene_Ct, 
     main=" ", 
     xlab= 'N gene (ct)', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$N_gene_Ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$N_gene_Ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 30, TeX(paste0("$p$-value=", round(ngeneU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$N_gene_Ct, 
     main=" ", 
     xlab="N gene (ct)", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$N_gene_Ct, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$N_gene_Ct)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 33.5, TeX(paste0("$p$-value=", round(ngeneR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("ngene01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "ngene01.pdf"))


############dev.off()
rnaU <- shapiro.test(data01$Copy_no_PER_litre); rnaR <- shapiro.test(data02$Copyno_litre)
wilcox.test(data01$Copy_no_PER_litre, data02$Copyno_litre, alternative = "two.sided")

tikz("rna01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
hist(data01$Copy_no_PER_litre, 
     main=" ", 
     xlab= 'RNA (copies per L) ', 
     border="light blue", 
     col="blue")
#text(x = 35, y = 145, expression(p-value == 0.045))
qqnorm(data01$Copy_no_PER_litre, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="") 
qqline(data01$Copy_no_PER_litre)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 50000000, TeX(paste0("$p$-value=", round(rnaU$p.value, 5))))
#ggqqplot(data01$TEMP., 
#ylab="Standardized Residuals", 
#xlab="Normal Scores", 
#main="$p$-value = 0.045")
#text(x = 1, y = 20, expression(p-value == 0.045))
mtext(TeX("(a) $\\textbf{URBAN}$"),                   # Add main title
      side = 3,
      line = -23,
      outer = TRUE)

hist(data02$Copyno_litre, 
     main=" ", 
     xlab="RNA (copies per L) ", 
     border="light blue", 
     col="blue", 
     las=1)

qqnorm(data02$Copyno_litre, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="",
       las=1) 
qqline(data02$Copyno_litre)
#text(x = -2, y = 35, expression(p-value == 0.045))
#text(x = -2, y = 35, "$p$-value = 0.045")
text(x = 1, y = 33500, TeX(paste0("$p$-value=", round(rnaR$p.value, 5))))
mtext(TeX("(b) $\\textbf{RURAL}$"),                   # Add main title
      side = 1,
      line = - 1,
      outer = TRUE)

dev.off()
tools::texi2pdf("rna01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "rna01.pdf"))


###########
x1 <- matrix(c(325, 73, 418, 167), byrow = TRUE, 2, 2)
x1
chisq.test(x1)
fisher.test(x1)
prop.test(x=c(325,73),n=c(743,240))

x2 <- matrix(c(32, 10, 711, 230), byrow = TRUE, 2, 2)
x2
chisq.test(x2)
fisher.test(x2)
prop.test(x=c(32,10),n=c(743,240))

x3 <- matrix(c(28, 0, 715, 240), byrow = TRUE, 2, 2)
x3
chisq.test(x3)
fisher.test(x3)
prop.test(x=c(28,0),n=c(743,240))

x4 <- matrix(c(516, 10, 227, 230), byrow = TRUE, 2, 2)
x4
chisq.test(x4)
fisher.test(x4)
prop.test(x=c(516,10),n=c(743,240))

tikz("cat01.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
barplot(x1, ylab="Number wastewater samples tested", xlab=" ", 
        main="RT-PCR Result ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="RT-PCR Result", legend= c('POSIVE','NEGATIVE'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x2, ylab="Number wastewater samples tested", xlab=" ", 
        main="Rainfall ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Rainfall", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x3, ylab="Number wastewater samples tested", xlab=" ", 
        main="Nearby Hospital ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Nearby hospital", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x4, ylab="Number wastewater samples tested", xlab=" ", 
        main="Sanitation ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Sanition", legend= c('GOOD','POOR'), fill =c("turquoise4", "turquoise" ), box.lty=0)


dev.off()
tools::texi2pdf("cat01.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "cat01.pdf"))


##########
x1 <- matrix(c(237, 3, 473, 76, 33, 161), byrow = TRUE, 3, 2)
x1
chisq.test(x1)
fisher.test(x1)
#kruskal.test(x1,2)
prop.test(x=c(237,3),n=c(743,240))
prop.test(x=c(473,76),n=c(743,240))

x2 <- matrix(c(585, 0, 142, 198, 16, 42), byrow = TRUE, 3, 2)
x2
chisq.test(x2)
fisher.test(x2)
kruskal.test(x2)
prop.test(x=c(585,0),n=c(743,240))

x3 <- matrix(c(38, 240, 705, 0), byrow = TRUE, 2, 2)
x3
chisq.test(x3)
fisher.test(x3)
kruskal.test(x3)
prop.test(x=c(38,240),n=c(743,240))

x4 <- matrix(c(131, 238, 612, 2), byrow = TRUE, 2, 2)
x4
chisq.test(x4)
fisher.test(x4)
kruskal.test(x4)
prop.test(x=c(131,238),n=c(743,240))

tikz("cat02.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,2))
barplot(x1, ylab="Number wastewater samples tested", xlab=" ", 
        main="Population Density ", col=c("turquoise4", "turquoise2","turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Population Density", legend= c('HIGH','MODERATION','LOW'), fill =c("turquoise4", "turquoise2", "turquoise" ), box.lty=0)

barplot(x2, ylab="Number wastewater samples tested", xlab=" ", 
        main="Housing Condition ", col=c("turquoise4", "turquoise2", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Housing Condition", legend= c('VERY GOOD','GOOD','POOR'), fill =c("turquoise4", "turquoise2", "turquoise" ), box.lty=0)

barplot(x3, ylab="Number wastewater samples tested", xlab=" ", 
        main="Drainage System ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Drainage System", legend= c('OPEN','CLOSE'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x4, ylab="Number wastewater samples tested", xlab=" ", 
        main="Livestock Domestication", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Livestock Domestication", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)


dev.off()
tools::texi2pdf("cat02.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "cat02.pdf"))


#########

x1 <- matrix(c(116, NA, 627, NA), byrow = TRUE, 2, 2)
x1
x2 <- matrix(c(17, NA, 726, NA), byrow = TRUE, 2, 2)
x2
x3 <- matrix(c(5, NA, 728, NA), byrow = TRUE, 2, 2)
x3
x4 <- matrix(c(3, NA, 740, NA), byrow = TRUE, 2, 2)
x4
x5 <- matrix(c(4, NA, 739, NA), byrow = TRUE, 2, 2)
x5
tikz("cat03.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,3))
barplot(x1, ylab="Number wastewater samples tested", xlab=" ", 
        main="Market Area ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Market Area", legend= c('YES','N0'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x2, ylab="Number wastewater samples tested", xlab=" ", 
        main="Industrial Area ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Industrial Area", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x3, ylab="Number wastewater samples tested", xlab=" ", 
        main="Airport ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Airport", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x4, ylab="Number wastewater samples tested", xlab=" ", 
        main="Rail Station ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Rail Station", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)

barplot(x5, ylab="Number wastewater samples tested", xlab=" ", 
        main="Bus Station ", col=c("turquoise4", "turquoise" ), 
        beside <- FALSE, xlim=c(0,1), width=.3,
        names.arg = c('Urban','Rural'))
legend("right", title="Bus Station", legend= c('YES','NO'), fill =c("turquoise4", "turquoise" ), box.lty=0)


dev.off()
tools::texi2pdf("cat03.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "cat03.pdf"))




#### Zonewise statistics for urban
knitr::kable(table(data01$RAINF,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$SANITISATION,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$POPU_DENSITY,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$HOUSING_CONDITIONS,data01$ZONE),booktabs=TRUE,format="markdown")

knitr::kable(table(data01$DRAINAGE_SYSTEM,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$LIVESTOCK_DOMESTICATION,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$MARKET_AREA,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$INDUSTRIAL_AREA,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$AIRPORT,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$RAILWAY_STATION,data01$ZONE),booktabs=TRUE,format="markdown")
knitr::kable(table(data01$BUS_STATION.,data01$ZONE),booktabs=TRUE,format="markdown")


knitr::kable(head(data01),booktabs=TRUE,format="markdown")


# create the working data frame by removing the ID variable
data03 <- data.frame(data01[c(4,5,7,10,11,18)])

data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(), min = min(TEMP),
            q1 = quantile(TEMP, 0.25),
            median = median(TEMP),
            mean = mean(TEMP),
            q3 = quantile(TEMP, 0.75),
            max = max(TEMP),
            std = sd(TEMP))

data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(), min = min(HUMI),
            q1 = quantile(HUMI, 0.25),
            median = median(HUMI),
            mean = mean(HUMI),
            q3 = quantile(HUMI, 0.75),
            max = max(HUMI),
            std = sd(HUMI))

data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(), min = min(LATITUDE,na.rm=TRUE),
            q1 = quantile(LATITUDE, 0.25,na.rm=TRUE),
            median = median(LATITUDE,na.rm=TRUE),
            mean = mean(LATITUDE,na.rm=TRUE),
            q3 = quantile(LATITUDE, 0.75,na.rm=TRUE),
            max = max(LATITUDE,na.rm=TRUE),
            std = sd(LATITUDE,na.rm=TRUE))

data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(), min = min(LONGITUDE),
            q1 = quantile(LONGITUDE, 0.25),
            median = median(LONGITUDE),
            mean = mean(LONGITUDE),
            q3 = quantile(LONGITUDE, 0.75),
            max = max(LONGITUDE),
            std = sd(LONGITUDE))


data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(),min = min((Copy_no_PER_litre),na.rm=TRUE),
            q1 = quantile(na.omit(Copy_no_PER_litre), 0.25),
            median = median(na.omit(Copy_no_PER_litre)),
            mean = mean(na.omit(Copy_no_PER_litre)),
            q3 = quantile(na.omit(Copy_no_PER_litre), 0.75),
            max = max(na.omit(Copy_no_PER_litre)),
            std = sd(na.omit(Copy_no_PER_litre)))%>%
  mutate_if(is.numeric,format, 1)

data03 %>%                               # Summary by group using dplyr
  group_by(ZONE) %>% 
  summarize(n = n(),min = min((Copy_no_PER_litre),na.rm=TRUE),
            q1 = quantile(na.omit(Copy_no_PER_litre), 0.25),
            median = median(na.omit(Copy_no_PER_litre)),
            mean = mean(na.omit(Copy_no_PER_litre)),
            q3 = quantile(na.omit(Copy_no_PER_litre), 0.75),
            max = max(na.omit(Copy_no_PER_litre)),
            std = sd(na.omit(Copy_no_PER_litre)))%>%
  mutate_if(is.numeric,format, 1)


A <- matrix(0,55,10)
A[1,2] <-5; A[2,2] <-32; A[3,2] <-21;A[4,2] <-14;A[5,2] <-14
A[6,2] <-14;A[7,2] <-18; A[8,2] <-1
A[8,1] <-26; A[9,1] <-26; A[10,1] <-35;A[11,1] <-11
A[23,3] <-10; A[24,3] <-10; A[25,3] <-20;A[26,3] <-20;A[27,3]<-5
A[20,4] <-15; A[21,4] <-3; A[23,4] <-15;A[24,4] <-9;A[25,4]<-5
A[34,5] <-8; A[35,5] <-7; A[36,5] <-9;A[37,5] <-6;A[38,5]<-8
A[39,5] <-7;A[40,5] <-8;A[41,5]<-10
A[27,6] <-3;A[28,6] <-12;A[29,6]<-6;A[30,6] <-9;A[31,6] <-15;A[32,6]<-8
A[33,6] <-7;
A[53,7] <-16;A[54,7] <-16;A[55,7]<-16
A[44,8] <-9;A[45,8] <-10;A[46,8] <-10;A[47,8] <-8;A[48,8] <-10;A[49,8]<-9
A[50,9] <-17;A[51,9] <-17;A[52,9]<-17
A[8,10] <-1;A[12,10] <-10;A[13,10]<-14;A[14,10] <-13;A[15,10] <-13;A[16,10]<-10
A[17,10] <-13;A[18,10] <-14;A[19,10]<-11

colnames(A) <-c('Zone 1','Zone 2','Zone 3','Zone 4','Zone 5',
             'Zone 6','Zone 7','Zone 8','Zone 9','Zone 10')
rownames(A) <-c('01/30','02/03','02-04','02-05','02-06',
                '02-09','02-10','02-12','02-15','02-16',
                '02-17','02-22','02-23','02-24','03-01',
                '03-02','03-03','03-04','03-08','03-09',
                '03-10','03-25','04-05','04-06','04-07',
                '04-08','04-13','04-15','04-19','04-20',
                '04-21','04-23','04-24','04-27','04-28',
                '04-29','04-30','05-03','05-04','05-06',
                '05-07','05-24','05-25','05-26','05-27',
                '05-28','05-31','06-01','06-02','07-02',
                '07-03','07-05','07-07','07-08','07-09')
library(RColorBrewer)
png(file="fs2.png")
heatmap(A,Rowv = NA,Colv = NA,scale = 'column',
        col=heat.colors(4),
        xlab = 'Catchment Zone', ylab = 'Sampling date')
legend(x='right',legend= c('','','',''),
       fill=heat.colors(4), title = 'Number of samples')

graphics.off()

png(file="fs2.png")
heatmap(A,Rowv = NA,Colv = NA,scale = 'column',
        col=colorRampPalette(brewer.pal(8,"Oranges"))(5),
        xlab = 'Catchment Zone', ylab = 'Sampling date')
legend(x='right',legend= c('n=0','n<7','7<n<12','12<n<19','n>19'), cex =0.5,
       fill=colorRampPalette(brewer.pal(8,"Oranges"))(5),
       title = 'Sample Size')

graphics.off()

#library(devtools)
#install_github("jokergoo/ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
png(file="fs2.png")
Heatmap(A,col=colorRamp2(c(0,10,20,30),c('white','yellow','green','red')),
        cluster_rows=FALSE,cluster_columns=FALSE)
Heatmap(A,col=rev(rainbow(10)),name='Sample Size',
        cluster_rows=FALSE,cluster_columns=FALSE)
Heatmap(A,name='Sample Size',
        cluster_rows=FALSE,cluster_columns=FALSE)
Heatmap(A,name='Sample Size',
        row_names_gp = gpar(fontsize=7),
        column_names_gp = gpar(fontsize=7),
        cluster_rows=FALSE,cluster_columns=FALSE)
png(file="fs2.jpg")
Heatmap(A,
        name='Sample Size',
        row_names_gp = gpar(fontsize=7),
        column_names_gp = gpar(fontsize=7),
        col=colorRamp2(c(0,10,20,30),c('khaki','orange','blue','red')),
        cluster_rows=FALSE,cluster_columns=FALSE)
graphics.off()
legend(x='right',legend= c('','','',''),
       fill=heat.colors(4), title = 'Number of samples')

graphics.off()

heatmap(A,Rowv = NA,Colv = NA,scale = 'column')
plot_ly(z=A,type="heatmap")


require(graphics); require(grDevices)
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")

utils::str(hv)

## Part B
library(epiR); library(ggplot2); library(scales); library(zoo)
ncas <- 972; npop <- 1112
tmp <- as.matrix(cbind(ncas, npop))
epi.conf(tmp, ctype = "prevalence", method = "fleiss", N = 239171, design = 10, 
         conf.level = 0.95) * 100
?epi.conf

ncas <- 325; npop <- 743
tmp <- as.matrix(cbind(ncas, npop))
epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
         conf.level = 0.95) * 100

ncas <- 73; npop <- 240
tmp <- as.matrix(cbind(ncas, npop))
epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
         conf.level = 0.95) * 100

## comparing two proportions
res <- prop.test(x=c(325,73),n=c(743,240))
res

###################
R0 <- c(1.0183,0.98413,1.061,1.519,1.2564,1.5873,1.0862,1.0238,1.6634,1.1497)
B0 <- c(0.91449,0.80803,0.92283,0.93286,0.98971,0.75574,0.75001,0.82247,0.49024,0.91632)
Tz <- c(24,24,32,30,31,31,27,33,29,27)
Hz <- c(52,33,23,21,39,36,92,40,83,27)
Pz <- c(1.12,0.949,1.084,2.168,3.058,1.424,1.02,1.001,5.38,3.367)




#Save summary of the linear model
lma1<-summary(lm(R0 ~ Tz))
lma2<-summary(lm(R0 ~ Hz))
lma3<-summary(lm(R0 ~ Pz))

lmb1<-summary(lm(B0 ~ Tz))
lmb2<-summary(lm(B0 ~ Hz))
lmb3<-summary(lm(B0 ~ Pz))

#Get coefficients
coef_lma1<-lma1$coefficients
coef_lma2<-lma2$coefficients
coef_lma3<-lma3$coefficients

coef_lmb1<-lmb1$coefficients; 
coef_lmb2<-lmb2$coefficients
coef_lmb3<-lmb3$coefficients

tikz("zextrv1R0Tz.tex", width = 12, height = 8, standAlone = TRUE,
     packages = c("\\usepackage{tikz}",
                  "\\usepackage[active,tightpage,psfixbb]{preview}",
                  "\\PreviewEnvironment{pgfpicture}",
                  "\\setlength\\PreviewBorder{0pt}",
                  "\\usepackage{amssymb}"))
#confidenc ebands calculation:
#par(mfrow = c(2,2),mar = c(20, 8, 3, 2), mgp = c(2, 0.9, 0))
par(mfrow = c(2,3))
#pdf(file="zextrv1R0Tz.pdf")
plot(Tz, R0, xlab = "Temperature", ylab = TeX("$R_{0}$"), xlim = c(22, 32),ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Tz),  lwd=2)
text(y = 1.8, 
     x = 23, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lma1$r.squared,2),
                         ", p-value = ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lma1[2,4],2))),
     pos = 4)
#text(x = 1, y = 33500, TeX(paste0("$p$-value=", round(rnaR$p.value, 5))))
plot(Hz, R0, xlab = "Humidity", ylab = TeX("$R_{0}$"), xlim = c(20, 100),ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Hz),  lwd=2)
text(y = 1.8, 
     x = 30, 
     labels = TeX(paste0("$R^{2}$ = ",
                     #Round r.squared to 2 decimals
                     round(lma2$r.squared,2),
                     ", p-value = ",
                     #Round p.value of slope to 2 decimals
                     round(coef_lma2[2,4],2))),
     pos = 4)

plot(Pz, R0, xlab = "Population Density", ylab = TeX("$R_{0}$"), ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Pz),  lwd=2)
text(y = 1.8, 
     x = 1.5, 
     labels = TeX(paste0("$R^{2}$ = ",
                     #Round r.squared to 2 decimals
                     round(lma3$r.squared,2),
                     ", p-value = ",
                     #Round p.value of slope to 2 decimals
                     round(coef_lma3[2,4],2))),
     pos = 4)


plot(Tz, B0, xlab = "Temperature", ylab = TeX("$\\beta_{1}$"), xlim = c(22, 32),ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Tz),  lwd=2)
text(y = 0.55, 
     x = 23, 
     labels = TeX(paste0("$R^{2}$ = ",
                     #Round r.squared to 2 decimals
                     round(lmb1$r.squared,2),
                     ", p-value = ",
                     #Round p.value of slope to 2 decimals
                     round(coef_lmb1[2,4],2))),
     pos = 4)

plot(Hz, B0, xlab = "Humidity", ylab = TeX("$\\beta_{1}$"), xlim = c(20, 100),ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Hz),  lwd=2)
text(y = 0.55, 
     x = 30, 
     labels = TeX(paste0("$R^{2}$ = ",
                     #Round r.squared to 2 decimals
                     round(lmb2$r.squared,2),
                     ", p-value = ",
                     #Round p.value of slope to 2 decimals
                     round(coef_lmb2[2,4],2))),
     pos = 4)


plot(Pz, B0, xlab = "Population Density", ylab = TeX("$\\beta_{1}$"), xlim = c(1, 6) ,ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Pz),  lwd=2)
text(y = 0.55, 
     x = 1.5, 
     labels = TeX(paste0("$R^{2}$ = ",
                     #Round r.squared to 2 decimals
                     round(lmb3$r.squared,2),
                     ", p-value = ",
                     #Round p.value of slope to 2 decimals
                     round(coef_lmb3[2,4],2))),
     pos = 4)
dev.off()
tools::texi2pdf("zextrv1R0Tz.tex",quiet=FALSE)
system(paste(getOption("pdfviewer"), "zextrv1R0Tz.pdf"))

my.locations <- c("bottomcenter", "bottomleft", "bottomleft",
                  "bottomleft")
png(file="zextrv1R0Tz.png")
#dev.new()
par(mfrow=c(2,3), mar=c(5,4,1.5,1)+0.1)
plot(Tz, R0, xlab = "Temperature", ylab = TeX("$R_{0}$"), xlim = c(22, 32),ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Tz),  lwd=2)
text(y = 1.8, 
     x = 23, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lma1$r.squared,2),
                         ", $\\textit{p}$-value = ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lma1[2,4],2))),
     pos = 4)
#text(x = 1, y = 33500, TeX(paste0("$p$-value=", round(rnaR$p.value, 5))))
my.label <- paste(letters[1], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(Hz, R0, xlab = "Humidity", ylab = TeX("$R_{0}$"), xlim = c(20, 100),ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Hz),  lwd=2)
text(y = 1.8, 
     x = 30, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lma2$r.squared,2),
                         ", $\\textit{p}$-value = ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lma2[2,4],2))),
     pos = 4)
my.label <- paste(letters[2], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(Pz, R0, xlab = "Population Density", ylab = TeX("$R_{0}$"), ylim = c(0.8, 2),frame.plot = FALSE)
abline(lm(R0 ~ Pz),  lwd=2)
text(y = 1.8, 
     x = 1.5, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lma3$r.squared,2),
                         ", $\\textit{p}$-value ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lma3[2,4],2))),
     pos = 4)

my.label <- paste(letters[3], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(Tz, B0, xlab = "Temperature", ylab = TeX("$\\beta_{1}$"), xlim = c(22, 32),ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Tz),  lwd=2)
text(y = 0.55, 
     x = 23, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lmb1$r.squared,2),
                         ", $\\textit{p}$-value ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lmb1[2,4],2))),
     pos = 4)
my.label <- paste(letters[4], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(Hz, B0, xlab = "Humidity", ylab = TeX("$\\beta_{1}$"), xlim = c(20, 100),ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Hz),  lwd=2)
text(y = 0.55, 
     x = 30, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lmb2$r.squared,2),
                         ", $\\textit{p}$-value ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lmb2[2,4],2))),
     pos = 4)

my.label <- paste(letters[5], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(Pz, B0, xlab = "Population Density", ylab = TeX("$\\beta_{1}$"), xlim = c(1, 6) ,ylim = c(0.5, 1),frame.plot = FALSE)
abline(lm(B0 ~ Pz),  lwd=2)
text(y = 0.55, 
     x = 1.5, 
     labels = TeX(paste0("$R^{2}$ = ",
                         #Round r.squared to 2 decimals
                         round(lmb3$r.squared,2),
                         ", $\\textit{p}$-value ",
                         #Round p.value of slope to 2 decimals
                         round(coef_lmb3[2,4],2))),
     pos = 4)

my.label <- paste(letters[6], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[1], font=2)
graphics.off()

##########################################################
library(quantreg)
library(splines)
library(KScorrect)
library(R.matlab) 


data01z1 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z1data.xlsx",'Sheet1', range = "A1:B153", col_names = FALSE)
data02z1 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z1modfitA.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE)
colnames(data01z1) <- c('X1','X2') 
colnames(data02z1) <- c('X1','X2') 

data01z2 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z2data.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
data02z2 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z2modfitA.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
colnames(data01z2) <- c('X1','X2') 
colnames(data02z2) <- c('X1','X2') 

data01z7 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z7data.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
data02z7 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z7modfitA.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
colnames(data01z7) <- c('X1','X2') 
colnames(data02z7) <- c('X1','X2') 

data01z9 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z9data.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
data02z9 <- read_excel("/Users/edwardacheampong/Documents/EA_GitHub/Population-infection-estimation-from-wastewater-surveillance-for-SARS-CoV-2-in-Nagpur--India-during-the-1-second-pandemic-wave/z9modfitA.xlsx",'Sheet1',range = "A1:B153", col_names = FALSE )
colnames(data01z9) <- c('X1','X2') 
colnames(data02z9) <- c('X1','X2') 

#source("put_fig_letter.r")
my.locations <- c("bottomcenter", "bottomleft", "bottomleft",
                  "bottomleft")
png(file="z1279extrv1Wa.png")
#dev.new()
par(mfrow=c(4,2), mar=c(5,4,1.5,1)+0.1)
plot(1:length(data01z1$X1),data01z1$X1,xlab = "Time [Days]", ylab = "Cases", ylim = c(0, 0.0035),frame.plot = FALSE)
lines(1:length(data01z1$X1),data02z1$X1, lwd = 3)
# my.label <- paste(letters[1], ".", sep="")
# put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(1:length(data01z1$X2),data01z1$X2,xlab = "Time [Days]", ylab = "Deaths", ylim = c(0, 0.00006),frame.plot = FALSE)
lines(1:length(data01z1$X2),data02z1$X2, lwd = 3)
my.label <- paste(letters[1], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[2], font=2)

plot(1:length(data01z2$X1),data01z2$X1,xlab = "Time [Days]", ylab = "Cases", ylim = c(0, 0.004),frame.plot = FALSE)
lines(1:length(data01z2$X1),data02z2$X1, lwd = 3)
# my.label <- paste(letters[1], ".", sep="")
# put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(1:length(data01z2$X2),data01z2$X2,xlab = "Time [Days]", ylab = "Deaths", ylim = c(0, 0.00004),frame.plot = FALSE)
lines(1:length(data01z2$X2),data02z2$X2, lwd = 3)
my.label <- paste(letters[2], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[2], font=2)

plot(1:length(data01z7$X1),data01z7$X1,xlab = "Time [Days]", ylab = "Cases", ylim = c(0, 0.001),frame.plot = FALSE)
lines(1:length(data01z7$X1),data02z7$X1, lwd = 3)
# my.label <- paste(letters[1], ".", sep="")
# put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(1:length(data01z7$X2),data01z7$X2,xlab = "Time [Days]", ylab = "Deaths", ylim = c(0, 0.00004),frame.plot = FALSE)
lines(1:length(data01z7$X2),data02z7$X2, lwd = 3)
my.label <- paste(letters[3], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[2], font=2)

plot(1:length(data01z9$X1),data01z9$X1,xlab = "Time [Days]", ylab = "Cases", ylim = c(0, 0.004),frame.plot = FALSE)
lines(1:length(data01z9$X1),data02z9$X1, lwd = 3)
# my.label <- paste(letters[1], ".", sep="")
# put.fig.letter(label=my.label, location=my.locations[1], font=2)
plot(1:length(data01z9$X2),data01z9$X2,xlab = "Time [Days]", ylab = "Deaths", ylim = c(0, 0.00006),frame.plot = FALSE)
lines(1:length(data01z9$X2),data02z9$X2, lwd = 3)
my.label <- paste(letters[4], ".", sep="")
put.fig.letter(label=my.label, location=my.locations[2], font=2)

graphics.off()