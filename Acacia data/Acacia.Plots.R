install.packages("VennDiagram", dependencies = TRUE)
library(ggplot2)
getwd()


library(VennDiagram)
require(gridExtra)

.pa<-function(x) {.columnnum(x, pa=T)}
.columnnum<-function(x, pa=FALSE){
  x<-x[sapply(x, is.numeric) ]
  if (pa) x[x>0]<-1
  return(x)
}
plotVenn <- function(x, splitby, main = NULL, category.names = NULL, cat.dist = NULL, cat.pos = NULL,
                     cex = 1.5, cat.cex = 1.5, main.cex = 1.8, sub.cex = 1.5, ...) {
  x <- split(x, f=splitby)
  x <- lapply(x, function(x) names(x)[colSums(x) > 0])
  
  if (missing(category.names))
    category.names <- names(x)
  
  if (missing(cat.dist)) {
    if (length(x) == 3)
      cat.dist <- c(0.025, 0.035, 0.025)
    else
      cat.dist <- 0.025
  }
  
  if (missing(cat.pos)) {
    if (length(x) == 2)
      cat.pos <- c(0, 15)
    else if (length(x) == 3)
      cat.pos <- c(0, 15, 180)
    else
      cat.pos <- 0
  }
  
  gTree(
    children = venn.diagram(
      x, NULL,
      lty = "blank",
      fill = c("light blue", "light green", "lightcoral", "lightgoldenrod")[1:length(x)],
      alpha = 0.5, scaled = T,
      category.names = category.names, cat.pos = cat.pos, cat.dist = cat.dist,
      cex = cex, cat.cex = cat.cex, main.cex = main.cex, sub.cex = sub.cex,
      fontfamily = "sans", cat.fontfamily = "sans", main.fontfamily = "sans", sub.fontfamily = "sans",
      main = main, ...
    )
  )
}
plotVennSplitByHost <- function(data, site, season, ...) {
  data <- data[grepl(site, data$Sample) & data$Season == season, ]
  plotVenn(data[, -c(1:3)], data$Host, margin = 0,
           cex = 1.2, cat.cex = 1.2, main.cex = 1.5, ...)
}


bw.venn <- cbind(bw_vs_all[,1:3], .pa(bw_vs_all))
bw.venn.nosingledoubletons <- cbind(bw_vs_all.nosingledoubletons[,1:3], .pa(bw_vs_all.nosingledoubletons))
bw.venn.adjacent <- cbind(bw_vs_adjacent_all[,1:3], .pa(bw_vs_adjacent_all))
bw.venn.adjacent.nosingledoubletons <- cbind(bw_vs_adjacent_all.nosingledoubletons[,1:3], .pa(bw_vs_adjacent_all.nosingledoubletons))
bw.venn.ant_colmns = which(grepl("Formicidae",names(bw.venn)))
bw.venn.ant_colmns.nosingledoubletons = which(grepl("Formicidae",names(bw.venn.nosingledoubletons)))


pdf("Output/Venn.pdf")
grid.arrange(
  top = "All",
  plotVenn(bw.venn[, -c(1:3)], bw.venn$Host, "Overall"),
  plotVenn(bw.venn[, 4:(bw.venn.ant_colmns[1] - 1)], bw.venn$Host, "Coleoptera"),
  plotVenn(bw.venn[, bw.venn.ant_colmns[1]:bw.venn.ant_colmns[length(bw.venn.ant_colmns)]], bw.venn$Host, "Ants"),
  plotVenn(bw.venn[, (bw.venn.ant_colmns[length(bw.venn.ant_colmns)] + 1):length(names(bw.venn))], bw.venn$Host, "Hemiptera")
)


grid.arrange(
  top = "All",
  plotVenn(bw.venn.adjacent[, -c(1:3)], splitby = bw_vs_adjacent_all$Host, "Overall"),
  plotVenn(bw.venn.adjacent[, 4:(bw.venn.ant_colmns[1] - 1)], bw_vs_adjacent_all$Host, "Coleoptera"),
  plotVenn(bw.venn.adjacent[, bw.venn.ant_colmns[1]:bw.venn.ant_colmns[length(bw.venn.ant_colmns)]], bw_vs_adjacent_all$Host, "Ants"),
  plotVenn(bw.venn.adjacent[, (bw.venn.ant_colmns[length(bw.venn.ant_colmns)] + 1):length(names(bw.venn))], bw_vs_adjacent_all$Host, "Hemiptera")
)
bw.venn.adjacent.overall <- lapply(split(bw.venn.adjacent[, -c(1:3)], f=bw_vs_adjacent_all$Host), function(x) names(x)[colSums(x) > 0])
bw.venn.adjacent.overall.intersections <- list(
  AM_VS = intersect(bw.venn.adjacent.overall$A_mearnsii, bw.venn.adjacent.overall$A_sieberiana),
  VS_AMADJ = intersect(bw.venn.adjacent.overall$A_sieberiana, bw.venn.adjacent.overall$`A_mearnsii (adjacent)`),
  AM_AMADJ = intersect(bw.venn.adjacent.overall$A_mearnsii, bw.venn.adjacent.overall$`A_mearnsii (adjacent)`),
  All = intersect(intersect(bw.venn.adjacent.overall$A_mearnsii, bw.venn.adjacent.overall$A_sieberiana), bw.venn.adjacent.overall$`A_mearnsii (adjacent)`)
)
write.csv(do.call("rbind", lapply(bw.venn.adjacent.overall.intersections, as.data.frame)), "csv/bw_vs_adj overall intersections.csv")

grid.arrange(
  top = "No singletons or doubletons",
  plotVenn(bw.venn.adjacent.nosingledoubletons[, -c(1:3)], splitby = bw_vs_adjacent_all.nosingledoubletons$Host, "Overall"),
  plotVenn(bw.venn.adjacent.nosingledoubletons[, 4:(bw.venn.ant_colmns.nosingledoubletons[1] - 1)], bw_vs_adjacent_all.nosingledoubletons$Host, "Coleoptera"),
  plotVenn(bw.venn.adjacent.nosingledoubletons[, bw.venn.ant_colmns.nosingledoubletons[1]:bw.venn.ant_colmns.nosingledoubletons[length(bw.venn.ant_colmns.nosingledoubletons)]], bw_vs_adjacent_all.nosingledoubletons$Host, "Ants"),
  plotVenn(bw.venn.adjacent.nosingledoubletons[, (bw.venn.ant_colmns.nosingledoubletons[length(bw.venn.ant_colmns.nosingledoubletons)] + 1):length(names(bw.venn.nosingledoubletons))], bw_vs_adjacent_all.nosingledoubletons$Host, "Hemiptera")
)
# bw_vs KZN
grid.newpage()
grid.draw(plotVenn(
  bw.venn[grepl("KZN", bw.venn$Sample), -c(1:3)],
  splitby = bw.venn[grepl("KZN", bw.venn$Sample),]$Host,
  main = "KZN"
))
# bw_vs MPL
grid.newpage()
grid.draw(plotVenn(
  bw.venn[grepl("MP", bw.venn$Sample), -c(1:3)],
  splitby = bw.venn[grepl("MP", bw.venn$Sample),]$Host,
  main = "MP"
))
# bw_vs all species by region
grid.newpage()
grid.draw(plotVenn(
  bw.venn[, -c(1:3)],
  splitby = list(substring(gsub("MP", "MPL",bw.venn$Sample),1,3), bw.venn$Host),
  main = "Host, Region",
  category.names = c("KZN A.mearnsii", "MP A.mearnsii", "KZN V.sieberiana", "MP V.sieberiana"),
  cat.dist = c(0.025, 0.025, 0.1, 0.075)
))
dev.off()

######################################################################
###### Plot venn diagrams by season ##################################
######################################################################

pdf("Output/Venn Sites Season.pdf", width = 11, height = 7)
grid.arrange(
  ncol = 5,
  widths = c(1, 10, 10, 10, 10),
  top = "All",
  textGrob("Summer", rot = 90),
  plotVennSplitByHost(bw.venn, "MP3", "Summer", main = "MP3"),
  plotVennSplitByHost(bw.venn, "MP4", "Summer", main = "MP4"),
  plotVennSplitByHost(bw.venn, "KZN2", "Summer", main = "KZN2"),
  plotVennSplitByHost(bw.venn, "KZN3", "Summer", main = "KZN3"),
  textGrob("Winter", rot = 90),
  grid.rect(gp=gpar(col="white")),
  plotVennSplitByHost(bw.venn, "MP4", "Winter"),
  plotVennSplitByHost(bw.venn, "KZN2", "Winter"),
  plotVennSplitByHost(bw.venn, "KZN3", "Winter"),
  textGrob("Spring", rot = 90),
  grid.rect(gp=gpar(col="white")),
  plotVennSplitByHost(bw.venn, "MP4", "Spring"),
  plotVennSplitByHost(bw.venn, "KZN2", "Spring")
)
grid.arrange(
  ncol = 5,
  widths = c(1, 10, 10, 10, 10),
  top = "No signletons or doubletons",
  textGrob("Summer", rot = 90),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "MP3", "Summer", main = "MP3"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "MP4", "Summer", main = "MP4"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN2", "Summer", main = "KZN2"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN3", "Summer", main = "KZN3"),
  textGrob("Winter", rot = 90),
  grid.rect(gp=gpar(col="white")),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "MP4", "Winter"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN2", "Winter"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN3", "Winter"),
  textGrob("Spring", rot = 90),
  grid.rect(gp=gpar(col="white")),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "MP4", "Spring"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN2", "Spring")
)
grid.arrange(
  ncol = 3,
  top = "Only adjacent by season",
  plotVennSplitByHost(bw.venn, "KZN[23]|MP[34]", "Summer", "Summer"),
  plotVennSplitByHost(bw.venn, "KZN[23]|MP[34]", "Winter", "Winter"),
  plotVennSplitByHost(bw.venn, "KZN[23]|MP[34]", "Spring", "Spring"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN[23]|MP[34]", "Summer", "Summer NSD"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN[23]|MP[34]", "Winter", "Winter NSD"),
  plotVennSplitByHost(bw.venn.nosingledoubletons, "KZN[23]|MP[34]", "Spring", "Spring NSD")
)
grid.arrange(
  ncol = 3,
  top = "By season",
  plotVennSplitByHost(bw.venn.adjacent, "*", "Summer", "Summer"),
  plotVennSplitByHost(bw.venn.adjacent, "*", "Winter", "Winter"),
  plotVennSplitByHost(bw.venn.adjacent, "*", "Spring", "Spring"),
  plotVennSplitByHost(bw.venn.adjacent.nosingledoubletons, "*", "Summer", "Summer NSD"),
  plotVennSplitByHost(bw.venn.adjacent.nosingledoubletons, "*", "Winter", "Winter NSD"),
  plotVennSplitByHost(bw.venn.adjacent.nosingledoubletons, "*", "Spring", "Spring NSD")
)
dev.off()


################################################
###### Morphospecies diversity by family by host
################################################

pdf("Output/BW_VS_Adj Morphospecies by family host.pdf", width = 11, height = 6, useDingbats = F)
old.par.mar <- par(mar=c(7.5,4.1,1.1,1.1))

plot(bw_vs.per_family_counts$AM, ylim = c(0, 70), xaxt="n", type = "n", ylab = "Richness", xlab = "")
#axis(1, at = 1:nrow(bw_vs.per_family_counts), las = 3, cex.axis = 0.8, labels = row.names(bw_vs.per_family_counts))
axis(1, at = 1:nrow(bw_vs.per_family_counts), las = 3, cex.axis = 0.8, labels = F)
text(labels = row.names(bw_vs.per_family_counts), x=1:nrow(bw_vs.per_family_counts), y=-5.1,
     xpd = TRUE, srt = 90, adj = 1, cex = 0.8,
      col = as.vector(unlist(list("Coleoptera" = "#2F469D", "Hemiptera" = "#AA1E23", "Hymenoptera" = "#0BAA4B")[bw_vs.per_family_counts$Order])))
lines(bw_vs.per_family_counts$VS, type = "o", pch = 20, cex = 0.5, col = "#A5A5A5", lwd = 2)
lines(bw_vs.per_family_counts$AM, type = "o", pch = 20, cex = 0.5, col = "#5B9BD5", lwd = 2)
lines(bw_vs.per_family_counts$AMAdj, type = "o", pch = 20, cex = 0.5, col = "#ED7D31", lwd = 2)
legend("center", 0, bty = "n",
       c("Acacia mearnsii", "Acacia mearnsii (adjacent)", "Vachellia sieberiana"),
       c("#5B9BD5", "#ED7D31", "#A5A5A5"))

plot(bw_vs.per_family_counts$AM, ylim = c(0, 80), xaxt="n", type = "n", ylab = "Richness", xlab = "")
#axis(1, at = 1:nrow(bw_vs.per_family_counts), las = 3, cex.axis = 0.8, labels = row.names(bw_vs.per_family_counts))
axis(1, at = 1:nrow(bw_vs.per_family_counts), las = 3, cex.axis = 0.8, labels = F)
text(labels = row.names(bw_vs.per_family_counts), x=1:nrow(bw_vs.per_family_counts), y=-5.1,
     xpd = TRUE, srt = 90, adj = 1, cex = 0.8,
     col = as.vector(unlist(list("Coleoptera" = "#2F469D", "Hemiptera" = "#AA1E23", "Hymenoptera" = "#0BAA4B")[bw_vs.per_family_counts$Order])))
lines(bw_vs.per_family_counts$VS, type = "o", pch = 20, cex = 0.5, col = "#A5A5A5", lwd = 2)
lines(bw_vs.per_family_counts$AM, type = "o", pch = 20, cex = 0.5, col = "#5B9BD5", lwd = 2)
lines(bw_vs.per_family_counts$AMAdj, type = "o", pch = 20, cex = 0.5, col = "#ED7D31", lwd = 2)
lines(bw_vs.per_family_counts$All, type = "o", pch = 20, cex = 0.5, col = "#000000", lwd = 2)
legend("center", 0, bty = "n",
       c("Acacia mearnsii", "Acacia mearnsii (adjacent)", "Vachellia sieberiana", "All"),
       c("#5B9BD5", "#ED7D31", "#A5A5A5", "#000000"))

par(mar = old.par.mar)
rm(old.par.mar)
dev.off()


######################### Family Diversity
groupByComponent <- function(x, component = 1) {
  names(x)<-unlist(lapply(strsplit(names(x), "\\_"), "[[", component))
  pv<-unique(names(x))
  
  families<-data.frame(x)
  for (i in 1:length(pv)){
    #i=1
    tmp<-rowSums(data.frame(fam=x[,which(names(x)==pv[i])]))
    families<-cbind(families, data.frame(add=tmp))
  }
  
  families<-families[,-(1:length(names(x)))]
  names(families)<-pv
  families
}



bw.famdiv <- subset(bw_vs_all[bw_vs_all$Host == 'A_mearnsii',], select = -c(Host, Season))
bw.famdiv$Sample <- substring(gsub("MP", "MPL",bw.famdiv$Sample),1,3)
bw.famdiv <- aggregate(. ~ Sample, bw.famdiv, sum)
rownames(bw.famdiv) <- bw.famdiv$Sample
bw.famdiv <- .pa(bw.famdiv)
bw.famdiv.ant_colmns = which(grepl("Formicidae",names(bw.famdiv)))
bw.famdiv.ants <- groupByComponent(bw.famdiv[, bw.famdiv.ant_colmns], 3)
bw.famdiv.beetles <- groupByComponent(bw.famdiv[, 1:(bw.famdiv.ant_colmns[1] - 1)])
bw.famdiv.hemiptera <- groupByComponent(bw.famdiv[, (bw.famdiv.ant_colmns[length(bw.famdiv.ant_colmns)]+1):length(names(bw.famdiv)) ])

bw.famdiv.all <- aggregate(. ~ Host, subset(bw_vs_all[bw_vs_all$Host == 'A_mearnsii',], select = -c(Season, Sample)), sum)
bw.famdiv.all <- .pa(bw.famdiv.all)
bw.famdiv.all.ant_colmns = which(grepl("Formicidae",names(bw.famdiv.all)))
bw.famdiv.all.ants <- groupByComponent(bw.famdiv.all[, bw.famdiv.all.ant_colmns], 3)
bw.famdiv.all.beetles <- groupByComponent(bw.famdiv.all[, 1:(bw.famdiv.all.ant_colmns[1] - 1)])
bw.famdiv.all.hemiptera <- groupByComponent(bw.famdiv.all[, (bw.famdiv.all.ant_colmns[length(bw.famdiv.all.ant_colmns)]+1):length(names(bw.famdiv.all)) ])

vs.famdiv <- subset(bw_vs_all[bw_vs_all$Host == 'A_sieberiana',], select = -c(Host, Season))
vs.famdiv$Sample <- substring(gsub("MP", "MPL",vs.famdiv$Sample),1,3)
vs.famdiv <- aggregate(. ~ Sample, vs.famdiv, sum)
rownames(vs.famdiv) <- vs.famdiv$Sample
vs.famdiv <- .pa(vs.famdiv)
vs.famdiv.ant_colmns = which(grepl("Formicidae",names(vs.famdiv)))
vs.famdiv.ants <- groupByComponent(vs.famdiv[, vs.famdiv.ant_colmns], 3)
vs.famdiv.beetles <- groupByComponent(vs.famdiv[, 1:(vs.famdiv.ant_colmns[1] - 1)])
vs.famdiv.hemiptera <- groupByComponent(vs.famdiv[, (vs.famdiv.ant_colmns[length(vs.famdiv.ant_colmns)]+1):length(names(vs.famdiv)) ])

vs.famdiv.all <- aggregate(. ~ Host, subset(bw_vs_all[bw_vs_all$Host == 'A_sieberiana',], select = -c(Season, Sample)), sum)
vs.famdiv.all <- .pa(vs.famdiv.all)
vs.famdiv.all.ant_colmns = which(grepl("Formicidae",names(vs.famdiv.all)))
vs.famdiv.all.ants <- groupByComponent(vs.famdiv.all[, vs.famdiv.all.ant_colmns], 3)
vs.famdiv.all.beetles <- groupByComponent(vs.famdiv.all[, 1:(vs.famdiv.all.ant_colmns[1] - 1)])
vs.famdiv.all.hemiptera <- groupByComponent(vs.famdiv.all[, (vs.famdiv.all.ant_colmns[length(vs.famdiv.all.ant_colmns)]+1):length(names(vs.famdiv.all)) ])

#par(mar=c(8, 3, 3, 1))
#barplot(as.matrix(bw.famdiv.ants), beside = T, horiz = F, las = 2, col = c("light green", "light blue"), ylim = c(0, 4))
#barplot(as.matrix(bw.famdiv.beetles[, order(-colSums(bw.famdiv.beetles))]), beside = T, horiz = F, las = 2, col = c("light green", "light blue"), ylim = c(0, max(bw.famdiv.beetles + 5)))


################ By Seasons (incomplete)

bw.byseasons.ant_colmns = which(grepl("Formicidae",names(bw.byseasons)))
bw.byseasons.beetles <- cbind(Season = bw.byseasons$Season, bw.byseasons[, 2:(bw.byseasons.ant_colmns[1] - 1)])
bw.byseasons.ants <- cbind(Season = bw.byseasons$Season, bw.byseasons[, bw.byseasons.ant_colmns[1]:(bw.byseasons.ant_colmns[length(bw.byseasons.ant_colmns)]+1) ])
bw.byseasons.hemiptera <- cbind(Season = bw.byseasons$Season, bw.byseasons[, (bw.byseasons.ant_colmns[length(bw.byseasons.ant_colmns)]+1):length(names(bw.byseasons)) ])