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
tab <- tab[order(rowSums(tab),decreasing=TRUE),order(colSums(tab),decreasing=TRUE)]
tab[tab > 0] <- 1

spp.tab <- rowSums(tab)
traits.tab <- colSums(tab)

png("species.png", 1000,1000)
par(mar=c(5.1,5.1,4.1,2.1))
plot(sort(spp.tab, TRUE), log="y", type="n", xlab="Number of traits", ylab="Number of species", cex.lab=1.5)
rect(0, 1, 30000, 10, col="grey70", border=NA)
lines(sort(spp.tab, TRUE), lwd=3)
text(35000, 1.5, "30,000 species have at least ten pieces of trait data", cex=2, adj=0)
dev.off()

png("traits.png", 1000,1000)
par(mar=c(5.1,5.1,4.1,2.1))
plot(sort(traits.tab, TRUE), log="y", type="n", xlab="Number of traits", ylab="Number of species", cex.lab=1.5)
rect(0, 1, 200, 700, col="grey70", border=NA)
lines(sort(traits.tab, TRUE), lwd=3)
text(75, 10, "200 traits have data data on at least 200 species", cex=2, adj=0)
dev.off()

plot(sort(traits.tab, TRUE), log="y", type="l", xlab="Number of traits", ylab="Number of species")
