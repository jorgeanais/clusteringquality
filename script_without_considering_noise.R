# Title     : TODO
# Objective : TODO
# Created by: jorge
# Created on: 25/09/2020

library("fpc")

# 1. Phot+PM
t.photpm <- read.table("data/Phot+PM/cl086.csv", header = TRUE, sep = ",")
data.photpm <- t.photpm[,c("l", "b", "mag_Ks", "H.Ks", "J.Ks", "J.H","pmra", "pmdec", "Q")]
maxlabel <- max(t.photpm[,"label"])
label.photpm <- maxlabel + 1 - t.photpm[,"label"]
d.photpm <- dist(data.photpm)
results.photpm <- cluster.stats(d.photpm, label.photpm, noisecluster = TRUE)

# 2. Color+PM
t.colorspm <- read.table("data/Colors+PM/cl086.csv", header = TRUE, sep = ",")
data.colorspm <- t.colorspm[,c("l", "b", "H.Ks", "J.Ks", "J.H","pmra", "pmdec", "Q")]
maxlabel <- max(t.colorspm[,"label"])
label.colorspm <- maxlabel + 1 - t.colorspm[,"label"]
d.colorspm <- dist(data.colorspm)
results.colorspm <- cluster.stats(d.colorspm, label.colorspm, noisecluster = TRUE)

# 3. All-in
t.allin <- read.table("data/All-in/cl086.csv", header = TRUE, sep = ",")
data.allin <- t.allin[,c("l", "b", "mag_J", "mag_H", "mag_Ks", "H.Ks", "J.Ks", "J.H","pmra", "pmdec", "Q")]
maxlabel <- max(t.allin[,"label"])
label.allin <- maxlabel + 1 - t.allin[,"label"]
d.allin <- dist(data.allin)
results.allin <- cluster.stats(d.allin, label.allin, noisecluster = TRUE)

# 4. Mini
t.mini <- read.table("data/Mini/cl086.csv", header = TRUE, sep = ",")
data.mini <- t.mini[,c("l", "b", "mag_J", "mag_H", "mag_Ks","pmra", "pmdec")]
maxlabel <- max(t.mini[,"label"])
label.mini <- maxlabel + 1 - t.mini[,"label"] + 2
d.mini <- dist(data.mini)
results.mini <- cluster.stats(d.mini, label.mini, noisecluster = TRUE)

# 5. Mini-alternative
t.minialt <- read.table("data/Mini-alternative/cl086.csv", header = TRUE, sep = ",")
data.minialt <- t.minialt[,c("l", "b", "H.Ks", "J.Ks", "J.H","pmra", "pmdec")]
maxlabel <- max(t.minialt[,"label"])
label.minialt <- maxlabel + 1 - t.minialt[,"label"]
d.minialt <- dist(data.minialt)
results.minialt <- cluster.stats(d.minialt, label.minialt, noisecluster = TRUE)

results <- list(results.photpm, results.colorspm, results.allin, results.mini, results.minialt)

cl.qua.indexs <- list("n", "cluster.number", "cluster.size", "min.cluster.size", "noisen", "diameter", "average.distance",
                      "median.distance", "separation", "average.toother", "separation.matrix", "ave.between.matrix",
                      "average.between", "average.within", "n.between", "n.within", "max.diameter", "min.separation",
                      "within.cluster.ss", "clus.avg.silwidths", "avg.silwidth", "pearsongamma", "dunn", "dunn2", "entropy",
                      "wb.ratio", "ch", "cwidegap", "widestgap", "sindex")


for (indx in cl.qua.indexs) {
   print(indx)
   for (r in results) {
      print(r[[indx]])
   }
}
