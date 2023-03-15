#Make matrix of voice categories
VoiceMarkersGreek <- matrix(c(433, 405, 567, 3915, 619, 866, 199, 1839, 1135, 892, 3302, 1179, 641, 248), ncol = 7, byrow = TRUE)

colnames(VoiceMarkersGreek) <- c("Homer", "Herodotus", "Plato", "LXX", "Polybius", "NT", "Plutarch")
rownames(VoiceMarkersGreek) <- c("Passive aorist", "Middle aorist")

VoiceMarkersGreek

#Perform chisquared test

chisq.test(VoiceMarkersGreek)

#Make plot of relative frequencies

#All works

#Get relative frequencies
VoiceMarkersGreekProportion <- prop.table(VoiceMarkersGreek, 2)*100

VoiceMarkersGreekProportion

#Make plot

plot(VoiceMarkersGreekProportion[1,], type = "o", pch = 2, ylim = range(0:80), axes = FALSE, xlab = "", ylab = "Relative frequency", sub = "Figure 1: Aorist Passive and Middle in Greek")

axis(1, at = 1:7, labels = colnames(VoiceMarkersGreek))
axis(2)
box()

lines(VoiceMarkersGreekProportion[2,], type = "o", lty = 2, pch = 0, col = "darkgrey")
legend("bottomright", lty = c(1,2), col = c("black", "darkgrey"), pch = c(2, 0), legend = c("Aorist Passive", "Aorist Middle"))

#All works except LXX and NT
VoiceMarkersGreekReduced <- matrix(c(433, 405, 567, 619, 199, 1839, 1135, 892, 1179, 248), ncol = 5, byrow = TRUE)

colnames(VoiceMarkersGreekReduced) <- c("Homer", "Herodotus", "Plato", "Polybius", "Plutarch")
rownames(VoiceMarkersGreekReduced) <- c("Passive aorist", "Middle aorist")

VoiceMarkersGreekReducedProportion <- prop.table(VoiceMarkersGreekReduced, 2)*100

VoiceMarkersGreekReducedProportion

#Make plot

plot(VoiceMarkersGreekReducedProportion[1,], type = "o", pch = 2, ylim = range(0:80), axes = FALSE, xlab = "", ylab = "Relative frequency", sub = "Figure 2: Aorist Passive and Middle in non-Biblical Greek")

axis(1, at = 1:5, labels = colnames(VoiceMarkersGreekReducedProportion))
axis(2)
box()

lines(VoiceMarkersGreekReducedProportion[2,], type = "o", lty = 2, pch = 0, col = "darkgrey")
legend("bottomright", lty = c(1,2), col = c("black", "darkgrey"), pch = c(2, 0), legend = c("Aorist Passive", "Aorist Middle"))


colnames(VoiceMarkersGreekReduced) <- c("Homer", "Herodotus", "Plato", "Polybius", "Plutarch")
rownames(VoiceMarkersGreek) <- c("Passive aorist", "Middle aorist")




#Get expected values

chisq.test(VoiceMarkersGreek)$expected

#Obtain effect size

library(vcd)

assocstats(VoiceMarkersGreek)

#Perform Fisher exact test:

PassiveAoristHomer <- matrix(c(433, 6571, 1839, 7397), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristHomer, alternative = "less")

MiddleAoristHomer <- matrix(c(1839, 7397, 433, 6571), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristHomer, alternative = "greater")

PassiveAoristHesiod <- matrix(c(405, 6599, 1135, 8101), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristHesiod, alternative = "less")

MiddleAoristHesiod <- matrix(c(1135, 8101, 405, 6599), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristHomer, alternative = "greater")

PassiveAoristPlato <- matrix(c(567, 6437, 892, 8344), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPlato, alternative = "less")

MiddleAoristPlato <- matrix(c(892, 8344, 567, 6437), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPlato, alternative = "greater")

PassiveAoristLXX <- matrix(c(3915, 3089, 3302, 5934), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristLXX, alternative = "greater")

MiddleAoristLXX <- matrix(c(3302, 5934, 3915, 3089), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristLXX, alternative = "less")

PassiveAoristPolybius <- matrix(c(619, 6385, 1179, 8057), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPolybius, alternative = "less")

MiddleAoristPolybius <- matrix(c(1179, 8057, 619, 6385), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPolybius, alternative = "greater")

PassiveAoristNT <- matrix(c(866, 6138, 641, 8595), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristNT, alternative = "greater")

MiddleAoristNT <- matrix(c(641, 8595, 866, 6138), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristNT, alternative = "less")

PassiveAoristPlutarch <- matrix(c(199, 6805, 248, 9188), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPlutharch, alternative = "less")

MiddleAoristPlutarch <- matrix(c(248, 9188, 199, 6805), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPlutarch, alternative = "greater")
