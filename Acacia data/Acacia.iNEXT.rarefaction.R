library(iNEXT)
library(ggplot2)
library(grid)
library(gridExtra)

.bu(getwd())


png("Output/BW Rarefaction All.png", width=1300, height=600)
ggiNEXT(bw_sample.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()

png("Output/BW Rarefaction Ants.png", width=1300, height=600)
ggiNEXT(bw_sample.ants.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()

png("Output/BW Rarefaction Coleoptera.png", width=1300, height=600)
ggiNEXT(bw_sample.coleoptera.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()

png("Output/BW Rarefaction Hemiptera.png", width=1300, height=600)
ggiNEXT(bw_sample.hemiptera.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()


png("Output/BW_VS Rarefaction ALL.png", width=1300, height=600)
ggiNEXT(bw_vs_site_host.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)


plotExpectedDiversity <- function(data.iNEXT, cut.point, title = NULL, sub = NULL, q = c(0,1,2), sort = NULL, legend = T, ylim = NULL) {
  rare10 <- do.call("rbind", lapply(data.iNEXT$Accumulation, function(x) { unique(x[x$t == cut.point, ]) }))
  
  order0 <- order(subset(rare10, order==0)$qD)
  if (!missing(sort))
    order0 <- match(sort, names(data.iNEXT$Accumulation))
  
  .par.mar <- par("mar")
  .par.oma <- par("oma")
  
  xx=1:length(order0)
  #quartz(height=8, width=8)
  numcells <- max(q) + 1
  if (legend)
    numcells <- numcells + 1
  if (numcells > 1) {
    par(mfrow=c(ceiling(numcells / 2),ceiling(numcells / 2)))
    par(mar=c(5,4,1,1))
    if (missing(title))
      par(oma=c(6,4,1,1))
    else
      par(oma=c(6,4,4,1))
  }
  i <- 1
  for (i in (q + 1)){
    if (numcells > 1)
      if (i==1) par(mar=c(1,4,5,1)) else par(mar=c(5,4,1,1))
    tmp <- subset(rare10, order==(i-1))[order0,]
    if (missing(ylim))
      tmpylim <- c(floor(min(tmp$qD.95.LCL) / 10) * 10, ceiling(max(tmp$qD.95.UCL) / 10) * 10)
    else
      tmpylim <- ylim
    plot(xx, tmp$qD.95.UCL, xlab="", ylab="", axes=F, type="n", ylim=tmpylim)
    with(tmp, arrows(xx, qD.95.LCL, xx, qD.95.UCL, code=3, length=.03, angle=90) )
    with(tmp, points(xx, qD, pch=c(19,5,4)[factor(tmp$method, levels = c("interpolated", "observed", "extrapolated"))]))
    if (i!=1 || length(q) == 1) axis(1, las=2, at=xx, labels=paste(names(data.iNEXT$Accumulation)[order0]))
    axis(2,las=2, cex.axis=1.3)
    box(bty="L", lwd=1.6)
    subTitle <- paste0("q=",(i-1))
    if (!missing(sub))
      subTitle <- paste(sub, subTitle, sep = " ")
    mtext(side=3, font=2, subTitle, line=-1)
    if (i==1 && legend) {
      plot(1,1, xlab="", ylab="", axes=F, type="n")
      legend("top", pch = c(19,5,4), legend=c("Rarefied", "Observed", "Extrapolated"), bty="n", 
             title = paste("For sample count =", cut.point), cex=1.3,inset=c(0,.15))
    }
  }
  if (numcells > 1)
    mtext(outer=TRUE, "Site", side=1, cex=1.4, line=1.25)
  mtext(outer=TRUE, expression(paste("Expected diversity (" ^plain("q"), plain("D)"))), side=2, cex=1.4, line=-1, las=0)
  if (!missing(title))
    mtext(outer = TRUE, title, side = 3, cex=1.4, line=1.25)
  
  if (numcells > 1)
    par(mar = .par.mar, oma = .par.oma)
  
  names(data.iNEXT$Accumulation)[order0]
}

pdf("Output/BW_VS Site Host Expected Diversity (n=10).pdf", width = 8, height = 8, useDingbats = F)
plotExpectedDiversity(bw_vs_site_host.iNEXT.10, 10)
dev.off()
pdf("Output/BW_VS Site Host Expected Diversity (n=15).pdf", width = 8, height = 8, useDingbats = F)
plotExpectedDiversity(bw_vs_site_host.iNEXT.15, 15)
dev.off()
pdf("Output/BW_VS Site Host Expected Diversity (n=20).pdf", width = 8, height = 8, useDingbats = F)
plotExpectedDiversity(bw_vs_site_host.iNEXT.20, 20)
dev.off()

ggiNEXT(bw_vs_site_host.iNEXT.enoughtrees.summer, type = "1", facet.var = "order")
ggiNEXT(bw_vs_site_host.iNEXT.enoughtrees.spring, type = "1", facet.var = "order")
ggiNEXT(bw_vs_site_host.iNEXT.enoughtrees.winter, type = "1", facet.var = "order")
pdf("Output/BW_VS Site Host Season Expected Diversity (n=7).pdf", width = 8, height = 8, useDingbats = F)
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.summer.7, 7, "Summer")
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.spring.7, 7, "Spring")
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.winter.7, 7, "Winter")
dev.off()

pdf("Output/BW_VS Site Host Season Expected Richness q=0.pdf", width = 8, height = 8, useDingbats = F)
.pardefault <- par(no.readonly = T)
par(mfrow=c(2,2))
par(oma=c(6,4,1,1))
#par(mar=c(1,4,5,1))
bw_vs_site_host.iNEXT.10.sort <- plotExpectedDiversity(bw_vs_site_host.iNEXT.10, 10, ylim = c(10, 120), q = 0, sub = "Overall", legend = F)
#par(mar=c(5,4,1,1))
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.spring.7, 7, q = 0, sub = "Spring", sort = bw_vs_site_host.iNEXT.10.sort, legend = F, ylim = c(10, 120))
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.summer.7, 7, q = 0, sub = "Summer", sort = bw_vs_site_host.iNEXT.10.sort, legend = F, ylim = c(10, 120))
plotExpectedDiversity(bw_vs_site_host.iNEXT.enoughtrees.winter.7, 7, q = 0, sub = "Winter", sort = bw_vs_site_host.iNEXT.10.sort, legend = F, ylim = c(10, 120))
par(.pardefault)
dev.off()

###############
###
###############




 #undebug(ggiNEXT)

pdf("Output/RAREFACTION LEGEND COPY.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_host.iNEXT3, type=1, facet.var="order") + labs(x="Sampled tree count", y="Morphospecies diversity") + theme_bw(base_size = 24) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.88, 0.60)) + 
 theme(legend.text = element_text(face = 3)) +
  theme(legend.key.width = unit(1.5, "line"), legend.key.height = unit(1.5, "line")) +
  theme(legend.background = element_rect(color = 'black', size=.7, fill = 'white', linetype='solid')) 
dev.off()
browseURL("Output/RAREFACTION LEGEND COPY.pdf")


pdf("Output/BW_VS Rarefaction By Host_all three.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_host.iNEXT3, type=1, facet.var="order") + labs(x="Sampled tree count", y="Morphospecies diversity") + theme_bw(base_size = 24) + 
  theme(legend.position = "none")
  # theme(legend.position = c(0.88, 0.66)) + 
  # theme(legend.key.width = unit(2, "line"), legend.key.height = unit(2, "line")) +
  # theme(legend.background = element_rect(color = 'black', size=.7, fill = 'white', linetype='solid')) 
dev.off()
browseURL("Output/BW_VS Rarefaction By Host_all three.pdf")
browseURL("Output/")

pdf("Output/BW_VS Rarefaction By Host_all four_LEGEND.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_host.iNEXT4, type=1, facet.var="order") + labs(x="Sampled tree count", y="Morphospecies diversity") + theme_bw(base_size = 24) + 
 # theme(legend.position = "none")
 theme(legend.position = c(0.88, 0.66)) + 
 theme(legend.key.width = unit(2, "line"), legend.key.height = unit(2, "line")) +
 theme(legend.background = element_rect(color = 'black', size=.7, fill = 'white', linetype='solid')) +
 theme(legend.text = element_text(face = 3)) 
dev.off()
browseURL("Output/BW_VS Rarefaction By Host_all four_LEGEND.pdf")
browseURL("Output/")

    
    
    
pdf("Output/BW_VS Rarefaction By Host.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_host.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()
pdf("Output/BW_VS Rarefaction By Host Region.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_region_host.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
ggiNEXT(bw_vs_region_host.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18) + xlim(0, 150)
dev.off()
pdf("Output/BW_VS Rarefaction By Host Region MP3_4 KZN2_3.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_region_host.MP34KZN23.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()
pdf("Output/BW_VS Rarefaction By Host Region Summer.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_region_host.summer.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()
pdf("Output/BW_VS Rarefaction By Host Region Winter.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_region_host.winter.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()
pdf("Output/BW_VS Rarefaction By Host Region Spring.pdf", width = 20, height = 7, useDingbats = F)
ggiNEXT(bw_vs_region_host.spring.iNEXT, type=1, facet.var="order") + labs(x="Number of trees") + theme_bw(base_size = 18)
dev.off()

host_season_taxa.iNEXT.palette <- scale_colour_manual("Order", values = c(
  "MP1" = "#F1A340", "MP2" = "#F1A340", "MP3" = "#F1A340",
  "MP4" = "#F1A340", "MP5" = "#F1A340", "MP6" = "#F1A340",
  "MP7" = "#F1A340", "KZN1" = "#998EC3", "KZN2" = "#998EC3",
  "KZN3" = "#998EC3", "KZN4" = "#998EC3", "KZN5" = "#998EC3",
  "KZN6" = "#998EC3", "KZN7" = "#998EC3"
))
pdf("Output/BW Rarefaction Season Taxa.pdf", width = 20, height = 14, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = "A_mearnsii",
  ggiNEXT(host_season_taxa.iNEXT$bw_ants_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Formicidae", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_ants_spring, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  #ggiNEXT(host_season_taxa.iNEXT$bw_ants_winter, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  grob(),
  ggiNEXT(host_season_taxa.iNEXT$bw_coleoptera_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Coleoptera") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_coleoptera_spring, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_coleoptera_winter, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_hemiptera_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Hemiptera") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_hemiptera_spring, type=1, facet.var="order", se = F) + labs(x="Number of trees", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$bw_hemiptera_winter, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  layout_matrix = rbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
)
dev.off()
pdf("Output/VS Rarefaction Season Taxa.pdf", width = 20, height = 14, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = "A_sieberiana",
  ggiNEXT(host_season_taxa.iNEXT$vs_ants_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Formicidae", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_ants_spring, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_ants_winter, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_coleoptera_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Coleoptera") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_coleoptera_spring, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_coleoptera_winter, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_hemiptera_summer, type=1, facet.var="order", se = F) + labs(x="", y = "Hemiptera") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_hemiptera_spring, type=1, facet.var="order", se = F) + labs(x="Number of trees", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season_taxa.iNEXT$vs_hemiptera_winter, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette
)
dev.off()

pdf("Output/BW_VS Rarefaction Season.pdf", width = 20, height = 10, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = "A_mearnsii and A_sieberiana",
  ggiNEXT(host_season.iNEXT$bw_summer, type=1, facet.var="order", se = F) + labs(x="", y = "A_mearnsii", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season.iNEXT$bw_spring, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season.iNEXT$bw_winter, type=1, facet.var="order", se = F) + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season.iNEXT$vs_summer, type=1, facet.var="order", se = F) + labs(x="", y = "A_sieberiana") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season.iNEXT$vs_spring, type=1, facet.var="order", se = F) + labs(x="Number of trees", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette,
  ggiNEXT(host_season.iNEXT$vs_winter, type=1, facet.var="order", se = F) + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + host_season_taxa.iNEXT.palette
)
dev.off()

pdf("Output/BW_VS Rarefaction Region Season.pdf", width = 20, height = 10, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = "Region, Season",
  left = "Morphospecies richness",
  ggiNEXT(region_season.iNEXT$KZN_summer, type=1, facet.var="order") + labs(x="", y = "KZN", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(region_season.iNEXT$KZN_spring, type=1, facet.var="order") + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(region_season.iNEXT$KZN_winter, type=1, facet.var="order") + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(region_season.iNEXT$MP_summer, type=1, facet.var="order") + labs(x="", y = "MP") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(region_season.iNEXT$MP_spring, type=1, facet.var="order") + labs(x="Number of trees", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(region_season.iNEXT$MP_winter, type=1, facet.var="order") + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200)
)
dev.off()


xl <- 75; yl <- 175
pdf("Output/BW_VS Rarefaction Region Season all three.pdf", width = 15, height = 10, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = textGrob("Region, Season", gp=gpar(fontsize=25,font=8)), 
  left = textGrob("         Morphospecies richness", gp=gpar(fontsize=28,font=8), rot=90),
 # bottom = textGrob("Sampled tree count", gp=gpar(fontsize=25,font=8), vjust=-.8), 
  ggiNEXT(region_season.iNEXT3$KZN_summer, type=1, facet.var="order") + labs(x="", y = "KwaZulu-Natal", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title.x = element_text(size=26)),
  ggiNEXT(region_season.iNEXT3$KZN_spring, type=1, facet.var="order") + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title.x = element_text(size=26)),
  ggiNEXT(region_season.iNEXT3$KZN_winter, type=1, facet.var="order") + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title = element_text(size=26)),
  ggiNEXT(region_season.iNEXT3$MP_summer, type=1, facet.var="order") + labs(x="", y = "Mpumalanga") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title.x = element_text(size=26)),
  ggiNEXT(region_season.iNEXT3$MP_spring, type=1, facet.var="order") + labs(x="Sampled tree count", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title = element_text(size=26)),
  ggiNEXT(region_season.iNEXT3$MP_winter, type=1, facet.var="order") + labs(x="", y = "") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = xl, y = yl) + theme(legend.position="none") + theme(axis.title = element_text(size=26))
)
dev.off()
browseURL("Output/BW_VS Rarefaction Region Season all three.pdf")



pdf("Output/BW Rarefaction Season Region.pdf", width = 20, height = 5, useDingbats = F)
grid.arrange(
  ncol = 3,
  top = "Season, Region",
  left = "Morphospecies richness",
  ggiNEXT(bw_season_region.iNEXT$Summer, type=1, facet.var="order") + labs(x="", y = "", title = "Summer") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(bw_season_region.iNEXT$Spring, type=1, facet.var="order") + labs(x="", y = "", title = "Spring") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200),
  ggiNEXT(bw_season_region.iNEXT$Winter, type=1, facet.var="order") + labs(x="", y = "", title = "Winter") + theme_bw(base_size = 18) + guides(linetype = FALSE) + expand_limits(x = 100, y = 200)
)
dev.off()

#ggiNEXT(bw_vs.bytree.iNEXT, type=1, facet.var="order")


.bu("/Users/jrg1035/Dropbox/R/myfunctions/functionlist.r")
.devpdf()

x11(width = 20, height = 7)
.devpdf("killt",  png=TRUE)
ggiNEXT(bw_vs_host.iNEXT3, type=1, facet.var="order") + labs(x="Sampled tree count", y="Morphospecies diversity") + theme_bw(base_size = 24) + 
  theme(legend.position = "none")
# theme(legend.position = c(0.88, 0.66)) + 
# theme(legend.key.width = unit(2, "line"), legend.key.height = unit(2, "line")) +
# theme(legend.background = element_rect(color = 'black', size=.7, fill = 'white', linetype='solid')) 
dev.off()
browseURL("Output/BW_VS Rarefaction By Host_all three.pdf")
browseURL("Output/")

.pdf2png("./graphs/BW_VS Rarefaction By Host_all four.pdf")
.pdf2png("./graphs/BW_VS_Adj Morphospecies by family host.pdf")

