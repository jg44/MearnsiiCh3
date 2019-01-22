library(vegan)

bw.NMDS <- metaMDS(bw, distance = "bray", trymax = 100)
#bw.NMDS <- metaMDS(bw, distance = "jaccard", binary = T, trymax = 100)
bw.NMDS

bw.ant_colmns = which(grepl("Formicidae",names(bw)))
bw.symbols <- c( rep(3, bw.ant_colmns[1] - 1), rep(19, length(bw.ant_colmns)), rep(2, length(names(bw)) - max(bw.ant_colmns)) )
bw.colors <- c(rep("green",7),rep("blue",7))
#bw.spcolors <- c( rep("red", bw.ant_colmns[1] - 1), rep("#008800", length(bw.ant_colmns)), rep("blue", length(names(bw)) - max(bw.ant_colmns)) )

pdf("Output/BW NMDS.pdf", width=12, height=7, useDingbats = F)

stressplot(bw.NMDS, sub = bw.NMDS$stress)

ordiplot(bw.NMDS, type="n")
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", col="#D3EAF1", show.groups = "blue", label=FALSE)
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", col="#CEE9BF", show.groups = "green", label=FALSE)
orditorp(bw.NMDS, display="species",air=0.01, label=FALSE, pch=bw.symbols)
orditorp(bw.NMDS, display="sites", air=0.01, cex=1)
#legend(0, legend = c("Coleoptera", "Hymenoptera:Formicidae", "Hemiptera"))
#ordicluster(bw.NMDS,hclust(vegdist(bw,"bray"))) 
#for(i in unique(treat)) {
#  ordihull(bw.NMDS$point[grep(i,treat),],draw="polygon",groups=treat[treat==i],col=bw.colors[grep(i,treat)],label=F) } 

ordiplot(bw.NMDS, type="n")
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", col="#D3EAF1", show.groups = "blue", label=FALSE)
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", col="#CEE9BF", show.groups = "green", label=FALSE)
orditorp(bw.NMDS, display="sites", air=0.01, cex=1)

ordiplot(bw.NMDS, type="n")
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", label=FALSE)
orditorp(bw.NMDS, display="species",air=0.01, label=FALSE, pch=bw.symbols)
orditorp(bw.NMDS, display="sites", air=0.01, cex=1)

ordiplot(bw.NMDS, type="n")
ordihull(bw.NMDS, groups=bw.colors, draw="polygon", label=FALSE)
orditorp(bw.NMDS, display="sites", air=0.01, cex=1)

dev.off()

######################### Am By Region

bw.KZN <- bw[grepl("KZN", rownames(bw)),]
bw.KZN <- bw.KZN[,colSums(bw.KZN) != 0]
bw.MP <- bw[grepl("MP", rownames(bw)),]
bw.MP <- bw.MP[,colSums(bw.MP) != 0]

vs.KZN <- vs[grepl("KZN", rownames(vs)),]
vs.KZN <- vs.KZN[,colSums(vs.KZN) != 0]
vs.MP <- vs[grepl("MP", rownames(vs)),]
vs.MP <- vs.MP[,colSums(vs.MP) != 0]

bw_adjacent.KZN <- bw_adjacent[grepl("KZN", rownames(bw_adjacent)),]
bw_adjacent.KZN <- bw_adjacent.KZN[,colSums(bw_adjacent.KZN) != 0]
bw_adjacent.MP <- bw_adjacent[grepl("MP", rownames(bw_adjacent)),]
bw_adjacent.MP <- bw_adjacent.MP[,colSums(bw_adjacent.MP) != 0]

bw.KZN.NMDS <- metaMDS(bw.KZN, distance = "bray", trymax = 100)
bw.KZN.NMDS
bw.MP.NMDS <- metaMDS(bw.MP, distance = "bray", trymax = 100)
bw.MP.NMDS

bw.KZN.ant_colmns = which(grepl("Formicidae",names(bw.KZN)))
bw.MP.ant_colmns = which(grepl("Formicidae",names(bw.MP)))

pdf("Output/BW NMDS By Region.pdf", width=12, height=7, useDingbats = F)

stressplot(bw.KZN.NMDS, main = "KZN", sub = bw.KZN.NMDS$stress)
stressplot(bw.MP.NMDS, main = "MP", sub = bw.MP.NMDS$stress)

ordiplot(bw.KZN.NMDS, type="n", main = "KZN")
orditorp(bw.KZN.NMDS, display="species",air=0.01, label=FALSE, pch=c( rep(3, bw.KZN.ant_colmns[1] - 1), rep(19, length(bw.KZN.ant_colmns)), rep(2, length(names(bw.KZN)) - max(bw.KZN.ant_colmns)) ))
orditorp(bw.KZN.NMDS, display="sites", air=0.01, cex=1)

ordiplot(bw.MP.NMDS, type="n", main = "MP")
orditorp(bw.MP.NMDS, display="species",air=0.01, label=FALSE, pch=c( rep(3, bw.MP.ant_colmns[1] - 1), rep(19, length(bw.MP.ant_colmns)), rep(2, length(names(bw.MP)) - max(bw.MP.ant_colmns)) ))
orditorp(bw.MP.NMDS, display="sites", air=0.01, cex=1)

dev.off()

########################## Am & Vs NMDS

bw_vs.NMDS <- metaMDS(bw_vs, distance = "bray", trymax = 100)
#bw_vs.NMDS <- metaMDS(bw_vs, distance = "jaccard", binary = T, trymax = 100)
bw_vs.NMDS
bw_vs.colors <- c(rep("green",9),rep("blue",9))

bw_vs.symbols <- c( rep(3, bw_vs.ant_colmns[1] - 1), rep(19, length(bw_vs.ant_colmns)), rep(2, length(names(bw_vs)) - max(bw_vs.ant_colmns)) )
bw_vs.colors <- c(rep("green",9),rep("blue",9))
bw_vs.spcolors <- c( rep("red", bw_vs.ant_colmns[1] - 1), rep("#008800", length(bw_vs.ant_colmns)), rep("blue", length(names(bw_vs)) - max(bw_vs.ant_colmns)) )
#treat <- bw_vs.colors

pdf("Output/BW_VS NMDS.pdf", width=12, height=7, useDingbats = F)

stressplot(bw_vs.NMDS, sub = bw_vs.NMDS$stress)
# with colours & species
ordiplot(bw_vs.NMDS, type="n")
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", col="#D3EAF1", show.groups = "blue", label=FALSE)
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", col="#CEE9BF", show.groups = "green", label=FALSE)
orditorp(bw_vs.NMDS, display="species",air=0.01, label=FALSE, pch=bw_vs.symbols)
orditorp(bw_vs.NMDS, display="sites", air=0.01, cex=1)
# with colours
ordiplot(bw_vs.NMDS, type="n")
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", col="#D3EAF1", show.groups = "blue", label=FALSE)
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", col="#CEE9BF", show.groups = "green", label=FALSE)
orditorp(bw_vs.NMDS, display="sites", air=0.01, cex=1)
# no colours, with species
ordiplot(bw_vs.NMDS, type="n")
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", label=FALSE)
orditorp(bw_vs.NMDS, display="species",air=0.01, label=FALSE, pch=bw_vs.symbols)
orditorp(bw_vs.NMDS, display="sites", air=0.01, cex=1)
# no colours, no species
ordiplot(bw_vs.NMDS, type="n")
ordihull(bw_vs.NMDS, groups=bw_vs.colors, draw="polygon", label=FALSE)
orditorp(bw_vs.NMDS, display="sites", air=0.01, cex=1)

dev.off()

######################### Am & Vs By Region

bw_vs.KZN <- bw_vs[grepl("KZN", rownames(bw_vs)),]
bw_vs.KZN <- bw_vs.KZN[,colSums(bw_vs.KZN) != 0]
bw_vs.MP <- bw_vs[grepl("MP", rownames(bw_vs)),]
bw_vs.MP <- bw_vs.MP[,colSums(bw_vs.MP) != 0]

bw_vs.KZN.NMDS <- metaMDS(bw_vs.KZN, distance = "bray", trymax = 100)
bw_vs.KZN.NMDS
bw_vs.MP.NMDS <- metaMDS(bw_vs.MP, distance = "bray", trymax = 100)
bw_vs.MP.NMDS

bw_vs.KZN.ant_colmns = which(grepl("Formicidae",names(bw_vs.KZN)))
bw_vs.MP.ant_colmns = which(grepl("Formicidae",names(bw_vs.MP)))

pdf("Output/BW_VS NMDS By Region.pdf", width=12, height=7, useDingbats = F)

stressplot(bw_vs.KZN.NMDS, main = "KZN", sub = bw_vs.KZN.NMDS$stress)
stressplot(bw_vs.MP.NMDS, main = "MP", sub = bw_vs.MP.NMDS$stress)

ordiplot(bw_vs.KZN.NMDS, type="n", main = "KZN")
orditorp(bw_vs.KZN.NMDS, display="species",air=0.01, label=FALSE, pch=c( rep(3, bw_vs.KZN.ant_colmns[1] - 1), rep(19, length(bw_vs.KZN.ant_colmns)), rep(2, length(names(bw_vs.KZN)) - max(bw_vs.KZN.ant_colmns)) ))
orditorp(bw_vs.KZN.NMDS, display="sites", air=0.01, cex=1)

ordiplot(bw_vs.MP.NMDS, type="n", main = "MP")
orditorp(bw_vs.MP.NMDS, display="species",air=0.01, label=FALSE, pch=c( rep(3, bw_vs.MP.ant_colmns[1] - 1), rep(19, length(bw_vs.MP.ant_colmns)), rep(2, length(names(bw_vs.MP)) - max(bw_vs.MP.ant_colmns)) ))
orditorp(bw_vs.MP.NMDS, display="sites", air=0.01, cex=1)

dev.off()

###################### Host Site Season

bw_vs_region <- bw_vs_all
bw_vs_region$Host <- as.character(bw_vs_region$Host)
bw_vs_region$Host[bw_vs_all$Host == 'A_mearnsii'] <- "Am"
bw_vs_region$Host[bw_vs_all$Host == 'A_sieberiana'] <- "Vs"
# create a new Site column containing just the site extracted from the Sample column
bw_vs_region$Site <- unlist(lapply(strsplit(as.character(bw_vs_region$Sample), "\\_"), "[[", 1))
# remove Sample column
bw_vs_region <- subset(bw_vs_region, select = -c(Sample))
# combine number of rows and sum, per site & region
bw_vs_region <- aggregate(. ~ Host + Site + Season, bw_vs_region, sum)

.BySeason <- function(data, season) {
  # get summer season
  byseason <- data[data$Season == season,]
  # set the rownames to MP1_As, etc...
  rownames(byseason) <- do.call(paste, c(byseason[c("Site", "Host")], sep = "_"))
  # remove Site, Host & Season columns
  byseason <- subset(byseason, select = -c(Site, Host, Season))
  
  byseason <- byseason[,colSums(byseason) != 0]
}

bw_vs_region.summer <- .BySeason(bw_vs_region, "Summer")
bw_vs_region.summer.NMDS <- metaMDS(bw_vs_region.summer, distance = "bray", trymax = 100)
bw_vs_region.summer.NMDS
bw_vs_region.winter <- .BySeason(bw_vs_region, "Winter")
bw_vs_region.winter.NMDS <- metaMDS(bw_vs_region.winter, distance = "bray", trymax = 100)
bw_vs_region.winter.NMDS
bw_vs_region.spring <- .BySeason(bw_vs_region, "Spring")
bw_vs_region.spring.NMDS <- metaMDS(bw_vs_region.spring, distance = "bray", trymax = 100)
bw_vs_region.spring.NMDS

.NMDSPlot <- function(data, nmdsdata, main) {
  ant_colmns = which(grepl("Formicidae",names(data)))
  symbols <- c( rep(3, ant_colmns[1] - 1), rep(19, length(ant_colmns)), rep(2, length(names(data)) - max(ant_colmns)) )
  colors <- c( rep("green", length(grep("KZN", rownames(data), value = T))), rep("blue",length(grep("MP", rownames(data), value = T))) )
  spcolors <- c( rep("red", ant_colmns[1] - 1), rep("#008800", length(ant_colmns)), rep("blue", length(names(data)) - max(ant_colmns)) )

  stressplot(nmdsdata, main = main, sub = nmdsdata$stress)
  
  ordiplot(nmdsdata, type="n", main = main)
  #ordihull(nmdsdata, groups=colors, draw="polygon", col="grey90", label=FALSE)
  ordihull(nmdsdata, groups=colors, draw="polygon", label=FALSE)
  orditorp(nmdsdata, display="species",air=0.01, label=FALSE, pch=symbols)
  orditorp(nmdsdata, display="sites", air=0.01, cex=1)
}

pdf("Output/BW_VS NMDS By Season.pdf", width=12, height=7, useDingbats = F)
.NMDSPlot(bw_vs_region.summer, bw_vs_region.summer.NMDS, "Summer")
.NMDSPlot(bw_vs_region.winter, bw_vs_region.winter.NMDS, "Winter")
.NMDSPlot(bw_vs_region.spring, bw_vs_region.spring.NMDS, "Spring")
dev.off()

######################### Host Site Season
bw_vs.host_site_season.NMDS <- metaMDS(bw_vs.host_site_season, distance = "bray", trymax = 100)
bw_vs.host_site_season.NMDS

pdf("Output/BW_VS NMDS Host Site Season Stress Plot.pdf", width = 10, height = 5, useDingbats = F)
stressplot(bw_vs.host_site_season.NMDS)
dev.off()
pdf("Output/BW_VS NMDS Host Site Season.pdf", width = 16, height = 16, useDingbats = F)
ordiplot(bw_vs.host_site_season.NMDS, type="n")
orditorp(bw_vs.host_site_season.NMDS, display="species",air=0.01, label=FALSE, pch=c( rep(3, bw_vs.ant_colmns[1] - 1), rep(19, length(bw_vs.ant_colmns)), rep(2, length(names(bw_vs.host_site_season)) - max(bw_vs.ant_colmns)) ))
orditorp(bw_vs.host_site_season.NMDS, display="sites", air=0.01, cex=1)
ordiplot(bw_vs.host_site_season.NMDS, type="n")
orditorp(bw_vs.host_site_season.NMDS, display="sites", air=0.01, cex=1)
ordiplot(bw_vs.host_site_season.NMDS, type="n")
ordihull(bw_vs.host_site_season.NMDS, substr(rownames(bw_vs.host_site_season), 4, 5), draw="polygon", col="#D3EAF1", show.groups = "KZ", label=FALSE)
ordihull(bw_vs.host_site_season.NMDS, substr(rownames(bw_vs.host_site_season), 4, 5), draw="polygon", col="#CEE9BF", show.groups = "MP", label=FALSE)
orditorp(bw_vs.host_site_season.NMDS, display="sites", air=0.01, label=FALSE, cex=2,
         pch = c("Am" = 16, "Vs" = 17)[substr(rownames(bw_vs.host_site_season), 1, 2)])
orditorp(bw_vs.host_site_season.NMDS, display="sites", air=0.01, cex=1)
dev.off()


######## envfit acacia ###################

acacia.envfit.all <- envfit(bw.NMDS, acacia.env.all, permu=999, na.rm=TRUE)
acacia.envfit.KZN <- envfit(bw.KZN.NMDS, acacia.env.all[grepl("KZN", row.names(acacia.env.all)),], permu=999, na.rm=TRUE)
acacia.envfit.MP <- envfit(bw.MP.NMDS, acacia.env.all[grepl("MP", row.names(acacia.env.all)),], permu=999, na.rm=TRUE)

acacia.envfit.all
acacia.envfit.KZN
acacia.envfit.MP



######## CCA ############################
bw_vs.host_site_season.CCA <- cca(sqrt(bw_vs.host_site_season))
bw_vs.host_site_season.CCA
plot(bw_vs.host_site_season.CCA)
plot(bw_vs.host_site_season.CCA, display = "sites")


#####CCA envfit####

bw.CCA <- cca(sqrt(bw), data=acacia.env.all)
bw.CCA
bw.envfit_CCA <- envfit(bw.CCA, env=acacia.env.all)
bw.envfit_CCA
data.frame(
  bw.envfit_CCA$vectors[[1]],
  r2 = round(bw.envfit_CCA$vectors$r, 3),
  pval = bw.envfit_CCA$vectors$pval
)[
  order(bw.envfit_CCA$vectors$r, decreasing=TRUE),
  ]

##conditioning latitude

bw.lat_ord <- cca(sqrt(bw) ~ Condition(Lat), data=acacia.env.all)
bw.lat_ord
bw.efit_condition.on.lat <- envfit(bw.lat_ord, env=acacia.env.all)
bw.efit_condition.on.lat
data.frame(
  bw.efit_condition.on.lat$vectors[[1]],
  r2 = round(bw.efit_condition.on.lat$vectors$r, 3),
  pval = bw.efit_condition.on.lat$vectors$pval
)[
  order(bw.efit_condition.on.lat$vectors$r, decreasing=TRUE),
]

#### NMDS ENVFIT
bw.NMDS
bw.envfit_NMDS <- envfit(bw.NMDS, env=acacia.env.all)
bw.envfit_NMDS
data.frame(
  bw.envfit_NMDS$vectors[[1]],
  r2 = round(bw.envfit_NMDS$vectors$r, 3),
  pval = bw.envfit_NMDS$vectors$pval
)[
  order(bw.envfit_NMDS$vectors$r, decreasing=TRUE),
  ]
