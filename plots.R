# Headers
library(natdb)
data <- natdb("~/Code/natdb/cache")

# Doing everything raw (...and so wrong...)
traits <- table(c(data$numeric$variable, data$categorical$variable))
species <- table(c(data$numeric$species, data$categorical$species))
plot(sort(as.numeric(species), TRUE), log="y", type="l", xlab="Number of species", ylab="Number of data points")
plot(sort(as.numeric(traits), TRUE), log="y", type="l", xlab="Number of traits", ylab="Number of data points")

# Doing everything species by trait (...and so right...)
tab <- table(c(data$numeric$species, data$categorical$species), c(data$numeric$variable, data$categorical$variable))
tab[tab > 0] <- 1
tab <- tab[order(rowSums(tab),decreasing=TRUE),order(colSums(tab),decreasing=TRUE)]

spp.tab <- rowSums(tab)
traits.tab <- colSums(tab)
tab <- data.frame(spp=sort(spp.tab,TRUE),trt=sort(traits.tab,TRUE),in)


pdf("coverage.pdf")
par(mar=c(5.1,5.1,4.1,2.1))
plot(sort(spp.tab,TRUE), log="xy", ylim=c(1,1200), axes=FALSE, xlab="", ylab="", type="n")
abline(h=36, col="blue", lwd=2, lty=2)
abline(v=10000, col="blue", lwd=2, lty=2)
lines(sort(spp.tab,TRUE), lwd=3, col="red")
lines(sort(traits.tab,TRUE), seq_along(traits.tab), lwd=3)
axis(1, at=c(1,10,100,1000,10000,100000,length(spp.tab)), cex.axis=1)
title(xlab="Number of species", cex.lab=1.25)
axis(2, at=c(1,10,50,100,500,1000,length(traits.tab)), labels=c(1,10,50,100,500,1000,NA), cex.axis=1)
title(ylab="Number of traits", cex.lab=1.25)
text(1, 70, "Maximising species coverage", adj=0, cex=1, col="red")
text(50, 700, "Maximising trait coverage", adj=0, cex=1)
dev.off()

options(scipen=5)

png("species.png", 1000,1000)
par(mar=c(5.1,5.1,4.1,2.1))
plot(sort(spp.tab, TRUE), log="xy", type="n", xlab="", ylab="", axes=FALSE)
axis(1, at=c(1,50,100,1000,10000,100000), xlab="Number of species", cex.lab=1.5, cex.axis=1.5)
axis(2, ylab="Number of traits", cex.lab=1.5, cex.axis=1.5)
rect(1, 1, 30000, 10, col="grey70", border=NA)
lines(sort(spp.tab, TRUE), lwd=3)
#text(35000, 1.5, "30,000 species have at least ten pieces of trait data", cex=2, adj=0)
dev.off()

png("traits.png", 1000,1000)
par(mar=c(5.1,5.1,4.1,2.1))
plot(sort(traits.tab, TRUE), log="y", type="n", xlab="Number of traits", ylab="Number of species", cex.lab=1.5)
rect(0, 1, 200, 700, col="grey70", border=NA)
lines(sort(traits.tab, TRUE), lwd=3)
text(75, 10, "200 traits have data data on at least 200 species", cex=2, adj=0)
dev.off()

plot(sort(traits.tab, TRUE), log="y", type="l", xlab="Number of traits", ylab="Number of species")
