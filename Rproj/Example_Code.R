library(metafor) #load packages
library(foreign)
library(Hmisc)

#Stomach Cancer - 9 studies # 


#Reading in, need to make everything lowercase, calcualte the log(ee) and make a new column,
#and calcualte the SE from the lower and upper bounds
stomachest <- c(-0.107905397, -0.045757491, 0.328379603, 0.32838, 0.06819, -0.1255, 0.15836, 0.086359831, 0.06069784)
stomachsd <- c(0.17682326, 0.197242318, 0.164955909, 0.156529222, 0.865305532, 0.872605987, 0.291697671, 0.341470142, 0.172054859)
stomachmeta <- rma(yi = stomachest, vi = stomachsd, method = "DL") 
summary(stomachmeta) #print results
predict(stomachmeta, transf=exp)

### Draw Cumulative Forest Plot
x <- cumul(stomachmeta) #Cumulative repeatedly fits specified model adding one study at a time
forest(x, xlim=c(-4,2.5), alim=c(-2,1), steps=7) 

#Funnel Plot#
funnel(stomachmeta)

#Colorectal Cancer - 7 studies #


coloest<-c(0, -0.3767, 0.079181246 )
colosd <- c(0.265677009, 0.867652393,  0.31958239) 

coloestindust<-c(0, -0.1739, -0.3767, 0.201397124, -0.022276395, 0.079181246, 0.229666519)
colosdindust <- c(0.265677009, 0.571134619, 0.867652393, 0.190741369, 0.186450895, 0.31958239, 0.675888094) 







library(metafor)

study <- c(
  "Bulbulyan et al. 1999-Cosmetic", "Fordyce et al. 2019-Colon", "Honda et al. 2002", 
  "Wergeland et al. 2017", "Ciocan et al. 2022", "Bulbulyan et al. 1999-Industrial", "Fordyce et al. 2019-Rectum")

stomachmeta <- rma(
  yi = coloestindust,
  sei = colosdindust,
  slab = study,
  method = "DL"
)


forest(stomachmeta,
       transf = exp,
       refline = 1,
       xlab = "Relative Risk",
       slab = study,
       main = "Forest Plot of Talc Exposure and CRC")



study <- c(
  "Bulbulyan et al. 1999-Cosmetic",
  "Fordyce et al. 2019-Colon",
  "Honda et al. 2002",
  "Wergeland et al. 2017",
  "Ciocan et al. 2022",
  "Bulbulyan et al. 1999-Industrial",
  "Fordyce et al. 2019-Rectum"
)

stomachmeta <- rma(
  yi = coloest,
  sei = colosd,
  slab = study,
  method = "DL"
)


forest(stomachmeta,
       transf = exp,
       refline = 1,
       xlab = "Relative Risk",
       slab = study,
       main = "Forest Plot of Talc Exposure and CRC",
       alim = c(0, 5),
       at = c(0.5, 1, 2, 3, 4, 5))

funnel(stomachmeta)



packageVersion("metafor")


colometa <- rma(yi = coloest, vi = colosd, method = "DL") 
summary(colometa) #print results
predict(colometa, transf=exp)

### Draw Cumulative Forest Plot
x <- cumul(colometa)
forest(x, xlim=c(-4,2.5), alim=c(-2,1), steps=7)

#Funnel Plot#
funnel(colometa)

#Pancreatic Cancer - 4 studies #
pancest <- c(-0.585026652, -0.096910013, 1.750508395, -0.031517051)
pancsd <- c(0.884116302, 0.457081497, 0.867026376, 0.327384284) 
pancmeta <- rma(yi = pancest, vi = pancsd, method = "DL") 
summary(pancmeta) #print results
predict(pancmeta, transf=exp)

### Draw Cumulative Forest Plot
x <- cumul(pancmeta)
forest(x, xlim=c(-4,2.5), alim=c(-2,1), steps=7)

#Funnel Plot#
funnel(pancmeta)

#Liver Cancer - 2 studies #
liverest <- c(-0.26760624,0.127104798)
liversd <- c(0.676520596,0.248612153) 
livermeta <- rma(yi = liverest, vi = liversd, method = "DL") 
summary(livermeta) #print results
predict(livermeta, transf=exp)

### Draw Cumulative Forest Plot
x <- cumul(livermeta)
forest(x, xlim=c(-4,2.5), alim=c(-2,1), steps=7)

#Funnel Plot#
funnel(livermeta)