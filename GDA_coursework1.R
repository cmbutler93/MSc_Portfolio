# Read in the data
survey <- read.csv("~/Documents/GDA/Insfa_students.csv")

library(FactoMineR)
mca_insfa <- MCA(survey)
insfa_dimdesc <- dimdesc(mca_insfa)
insfa_dimdesc[1]
insfa_dimdesc[2]
insfa_dimdesc[3]


mca_insfa$eig[[3]]
mca_insfa$eig$"cumulative percentage of variance"
plot1 <- plot(1:length(mca_insfa$eig$"cumulative percentage of variance") 
      mca_insfa$eig$"cumulative percentage of variance", type="l")
plot2 <- plot(1:25, res.taste$eig[[3]], type="l", xlab="Factors/dimensions 1 to 25", 
        ylab="Cumul. % of variance")
cluster <- HCPC(mca_insfa)
cluster$desc.ind


