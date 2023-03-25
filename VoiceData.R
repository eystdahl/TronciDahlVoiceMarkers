#Make matrix of voice categories
VoiceMarkersGreek <- matrix(c(433, 14, 405, 567, 12, 79, 3915, 619, 866, 199, 1839, 159, 1135, 892, 147, 441, 3302, 1179, 641, 248), ncol = 10, byrow = TRUE)

colnames(VoiceMarkersGreek) <- c("Homer", "Hesiod", "Herodotus", "Plato", "Callimachus", "Apollonius", "LXX", "Polybius", "NT", "Plutarch")
rownames(VoiceMarkersGreek) <- c("Passive aorist", "Middle aorist")

VoiceMarkersGreek

#Perform chi-squared test

chisq.test(VoiceMarkersGreek)

#Obtain effect size

library(vcd)

assocstats(VoiceMarkersGreek)

#Make plot of relative frequencies

#All works

#Get relative frequencies
VoiceMarkersGreekProportion <- prop.table(VoiceMarkersGreek, 2)*100
VoiceMarkersGreekProportion

#Make plot

plot(VoiceMarkersGreekProportion[1,], type = "o", pch = 2, ylim = range(0:100), axes = FALSE, xlab = "Ancient Greek ca. 8th c. BCE - 2nd c. CE", ylab = "Relative frequency (%)", sub = "Figure 1: Aorist Passive and Middle in Greek", cex.sub = 1.3, cex.lab = 1.3, cex.axis = 1.3)

axis(1, at = 1:10, labels = colnames(VoiceMarkersGreek))
axis(2)
box()

lines(VoiceMarkersGreekProportion[2,], type = "o", lty = 2, pch = 0)
legend("topright", lty = c(1,2), col = c("black", "black"), pch = c(2, 0), legend = c("Aorist Passive", "Aorist Middle"), cex = 1.3)

#All works except LXX and NT
VoiceMarkersGreekReduced <- matrix(c(433, 14, 405, 567, 12, 79, 619, 199, 1839, 159, 1135, 892, 147, 441, 1179, 248), ncol = 8, byrow = TRUE)

colnames(VoiceMarkersGreekReduced) <- c("Homer", "Hesiod", "Herodotus", "Plato", "Callimachus", "Apollonius", "Polybius", "Plutarch")
rownames(VoiceMarkersGreekReduced) <- c("Passive aorist", "Middle aorist")

VoiceMarkersGreekReduced

chisq.test(VoiceMarkersGreekReduced)

#Get proportions

VoiceMarkersGreekReducedProportion <- prop.table(VoiceMarkersGreekReduced, 2)*100

VoiceMarkersGreekReducedProportion

#Make plot

plot(VoiceMarkersGreekReducedProportion[1,], type = "o", pch = 2, ylim = range(0:100), axes = FALSE, xlab = "Ancient Greek ca. 8th c. BCE - 2nd c. CE", ylab = "Relative frequency (%)", sub = "Figure 2: Aorist Passive and Middle in non-Biblical Greek", cex.sub = 1.3, cex.lab = 1.3, cex.axis = 1.3)

axis(1, at = 1:8, labels = colnames(VoiceMarkersGreekReduced))
axis(2)
box()

lines(VoiceMarkersGreekReducedProportion[2,], type = "o", lty = 2, pch = 0)
legend("topright", lty = c(1,2), col = c("black", "black"), pch = c(2, 0), legend = c("Aorist Passive", "Aorist Middle"), cex = 1.3)

#Get expected values

chisq.test(VoiceMarkersGreek)$expected

#Perform Fisher exact test:

PassiveAoristHomer <- matrix(c(433, 6676, 1839, 8144), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristHomer, alternative = "less")

MiddleAoristHomer <- matrix(c(1839, 8144, 433, 6676), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristHomer, alternative = "greater")

PassiveAoristHesiod <- matrix(c(14, 7095, 159, 9824), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristHesiod, alternative = "less")

MiddleAoristHesiod <- matrix(c(159, 9824, 14, 7095), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristHesiod, alternative = "greater")

PassiveAoristHerodot <- matrix(c(405, 6704, 1135, 8848), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristHerodot, alternative = "less")

MiddleAoristHerodot <- matrix(c(1135, 8848, 405, 6704), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristHerodot, alternative = "greater")

PassiveAoristPlato <- matrix(c(567, 6542, 892, 9091), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPlato, alternative = "less")

MiddleAoristPlato <- matrix(c(892, 9091, 567, 6542), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPlato, alternative = "greater")

PassiveAoristCallimachos <- matrix(c(12, 7097, 147, 9836), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristCallimachos, alternative = "less")

MiddleAoristCallimachos <- matrix(c(147, 9836, 12, 7097), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristCallimachos, alternative = "greater")

PassiveAoristApollonius <- matrix(c(79, 7030, 441, 9542), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristApollonius, alternative = "less")

MiddleAoristApollonius <- matrix(c(441, 9542, 79, 7030), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristApollonius, alternative = "greater")

PassiveAoristLXX <- matrix(c(3915, 3194, 3302, 6681), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristLXX, alternative = "greater")

MiddleAoristLXX <- matrix(c(3302, 6681, 3915, 3194), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristLXX, alternative = "less")

PassiveAoristPolybius <- matrix(c(619, 6490, 1179, 8804), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPolybius, alternative = "less")

MiddleAoristPolybius <- matrix(c(1179, 8804, 619, 6490), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPolybius, alternative = "greater")

PassiveAoristNT <- matrix(c(866, 6243, 641, 9342), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristNT, alternative = "greater")

MiddleAoristNT <- matrix(c(641, 9342, 866, 6243), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristNT, alternative = "less")

PassiveAoristPlutarch <- matrix(c(199, 6910, 248, 9735), ncol = 2, byrow = TRUE)
fisher.test(PassiveAoristPlutharch, alternative = "less")

MiddleAoristPlutarch <- matrix(c(248, 9735, 199, 6910), ncol = 2, byrow = TRUE)
fisher.test(MiddleAoristPlutarch, alternative = "greater")

#Absolute frequencies BH binyanim
BHbinyanim <- matrix(c(11499, 446, 988, 6, 1753, 113, 169,
                       13932, 1414, 2194, 218, 2671, 128, 150,
                       10323, 1116, 1569, 80, 2264, 184, 314), ncol = 7, byrow = TRUE)
colnames(BHbinyanim) <- c("Qal", "Niphal", "Piel", "Pual", "Hiphil", "Hophal", "Hithpael")
rownames(BHbinyanim) <- c("Consecutive imperfect", "Perfect","Imperfect")

BHbinyanim

chisq.test(BHbinyanim)
assocstats(BHbinyanim)

BHbinyanimProportion <- prop.table(BHbinyanim, 2)*100
BHbinyanimProportion

barplot(BHbinyanimProportion, ylim = range(0:100), sub = "Figure 3: Distribution of tense/aspect categories over binyanim in BH", ylab = "Relative frequency (%)", col = c("black", "grey60", "grey40"), beside = TRUE)
legend("topright", fill = c("black", "grey60", "grey40"), legend = c("Consecutive imperfect", "Perfect", "Imperfect"))

#Get expected values

chisq.test(BHbinyanim)$expected

#Perform Fisher test

ConsQal <- matrix(c(11499, 3475, 24255, 12302), ncol = 2, byrow = TRUE)
fisher.test(ConsQal, alternative = "greater")

PerfQal <- matrix(c(13932, 6775, 21822, 9002), ncol = 2, byrow = TRUE)
fisher.test(PerfQal, alternative = "less")

IpfQal <- matrix(c(10323, 5527, 25431, 10250), ncol = 2, byrow = TRUE)
fisher.test(IpfQal, alternative = "less")

ConsNiphal <- matrix(c(446, 14528, 2530, 34027), ncol = 2, byrow = TRUE)
fisher.test(ConsNiphal, alternative = "less")

PerfNiphal <- matrix(c(1414, 19293, 1562, 29262), ncol = 2, byrow = TRUE)
fisher.test(PerfNiphal, alternative = "greater")

IpfNiphal <- matrix(c(1116, 14734, 1860, 33821), ncol = 2, byrow = TRUE)
fisher.test(IpfNiphal, alternative = "greater")


ConsPiel <- matrix(c(988, 13986, 3763, 32821), ncol = 2, byrow = TRUE)
fisher.test(ConsPiel, alternative = "less")

PerfPiel <- matrix(c(2194, 18513, 2557, 28267), ncol = 2, byrow = TRUE)
fisher.test(PerfPiel, alternative = "greater")

IpfPiel <- matrix(c(1569, 14281, 3182, 32499), ncol = 2, byrow = TRUE)
fisher.test(IpfPiel, alternative = "greater")

ConsPual <- matrix(c(6, 14968, 298, 26259), ncol = 2, byrow = TRUE)
fisher.test(ConsPual, alternative = "less")

PerfPual <- matrix(c(218, 20489, 86, 30738), ncol = 2, byrow = TRUE)
fisher.test(PerfPual, alternative = "greater")

IpfPual <- matrix(c(80, 15770, 214, 35467), ncol = 2, byrow = TRUE)
fisher.test(IpfPual, alternative = "less")

ConsHiphil <- matrix(c(1753, 13221, 4935,  31622), ncol = 2, byrow = TRUE)
fisher.test(ConsHiphil, alternative = "less")

PerfHiphil <- matrix(c(2671, 18036, 4017, 26807), ncol = 2, byrow = TRUE)
fisher.test(PerfHiphil, alternative = "less")

IpfHiphil <- matrix(c(2264, 13586, 4424, 31257), ncol = 2, byrow = TRUE)
fisher.test(IpfHiphil, alternative = "greater")

ConsHophal <- matrix(c(113, 14861, 312, 36245), ncol = 2, byrow = TRUE)
fisher.test(ConsHiphil, alternative = "less")

PerfHophal <- matrix(c(128, 20579, 297, 30527), ncol = 2, byrow = TRUE)
fisher.test(PerfHophal, alternative = "less")

IpfHophal <- matrix(c(184, 15666, 241, 35440), ncol = 2, byrow = TRUE)
fisher.test(IpfHophal, alternative = "greater")

ConsHithpael <- matrix(c(169, 14805, 464, 36093), ncol = 2, byrow = TRUE)
fisher.test(ConsHithpael, alternative = "less")

PerfHithpael <- matrix(c(150, 20557, 483, 30341), ncol = 2, byrow = TRUE)
fisher.test(PerfHithpael, alternative = "less")

IpfHithpael <- matrix(c(314, 15536, 319, 35362), ncol = 2, byrow = TRUE)
fisher.test(IpfHithpael, alternative = "greater")

#Greek and Hebrew equivalents

GreekHebrewEquivalents <- matrix(c(48, 47, 6, 34, 9, 3, 0, 6, 11, 3, 1, 6, 5, 7), ncol = 2, byrow = TRUE)
colnames(GreekHebrewEquivalents) <- c("Middle", "Passive")
rownames(GreekHebrewEquivalents) <- c("Qal", "Niphal", "Piel", "Pual", "Hiphil", "Hophal", "Hithpael")

GreekHebrewEquivalents

chisq.test(GreekHebrewEquivalents)

assocstats(GreekHebrewEquivalents)

chisq.test(GreekHebrewEquivalents)$expected

#Fisher test
MiddleQal <- matrix(c(48, 47, 32, 59), ncol = 2, byrow = TRUE)
fisher.test(MiddleQal, alternative = "greater")

PassiveQal <- matrix(c(47, 48, 59, 32), ncol = 2, byrow = TRUE)
fisher.test(PassiveQal, alternative = "less")

MiddleNiphal <- matrix(c(6, 34, 74, 72), ncol = 2, byrow = TRUE)
fisher.test(MiddleNiphal, alternative = "less")

PassiveNiphal <- matrix(c(34, 6, 72, 74), ncol = 2, byrow = TRUE)
fisher.test(PassiveNiphal, alternative = "greater")

MiddlePiel <- matrix(c(9, 3, 71, 105), ncol = 2, byrow = TRUE)
fisher.test(MiddlePiel, alternative = "greater")

PassivePiel <- matrix(c(4, 9, 105, 68), ncol = 2, byrow = TRUE)
fisher.test(PassivePiel, alternative = "less")

MiddlePual <- matrix(c(0, 6, 80, 100), ncol = 2, byrow = TRUE)
fisher.test(MiddlePual, alternative = "less")

PassivePual <- matrix(c(6, 0, 100, 80), ncol = 2, byrow = TRUE)
fisher.test(PassivePual, alternative = "greater")

MiddleHiphil <- matrix(c(11, 3, 69, 103), ncol = 2, byrow = TRUE)
fisher.test(MiddleHiphil, alternative = "greater")

PassiveHiphil <- matrix(c(3, 11, 103, 69), ncol = 2, byrow = TRUE)
fisher.test(PassiveHiphil, alternative = "less")

MiddleHophal <- matrix(c(1, 6, 79, 100), ncol = 2, byrow = TRUE)
fisher.test(MiddleHophal, alternative = "less")

PassiveHophal <- matrix(c(6, 1, 100, 79), ncol = 2, byrow = TRUE)
fisher.test(PassiveHophal, alternative = "greater")

MiddleHithpael <- matrix(c(5, 7, 75, 99), ncol = 2, byrow = TRUE)
fisher.test(MiddleHithpael, alternative = "greater")

PassiveHithpael <- matrix(c(7, 5, 99, 75), ncol = 2, byrow = TRUE)
fisher.test(MiddleHithpael, alternative = "less")


