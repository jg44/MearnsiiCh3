library(vegan)

averageColumnsBySeasons <- function(x) {
  categories <- unique(unlist(lapply(strsplit(names(x)[grepl("_(Dec|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov)", names(x))], "\\_"), "[[", 1)))
  for (c in categories) {
    x[,paste(c, "summer", sep="_")] <- rowMeans(x[, grep(paste(c, "(Dec|Jan|Feb)", sep="_"), names(x))])
    x[,paste(c, "autumn", sep="_")] <- rowMeans(x[, grep(paste(c, "(Mar|Apr|May)", sep="_"), names(x))])
    x[,paste(c, "winter", sep="_")] <- rowMeans(x[, grep(paste(c, "(Jun|Jul|Aug)", sep="_"), names(x))])
    x[,paste(c, "spring", sep="_")] <- rowMeans(x[, grep(paste(c, "(Sep|Oct|Nov)", sep="_"), names(x))])
  }
  
  subset(x, select=-grep("(Dec|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov)", names(x)))
}

acacia.env <- read.csv("Acacia_env/Acacia.env.csv", row.names=1, stringsAsFactors = F)

acacia.env <- averageColumnsBySeasons(acacia.env)


categoriseLandUse <- function (x, allCategory, categories) {
  x$landuse <- as.character(x$landuse)
  
  #default category
  x$category <- allCategory
  
  #map each category using grep
  for (i in ls(categories))
    x$category[grep(categories[[i]],x$landuse)] <- i
  
  x
}

getPrincipalComponents <- function(x) {
  #sums across new categories
  x.agg<-aggregate(Prop ~ ., data=x[,-c(2:3)], sum)
  #reshapes
  x.wide <- reshape(x.agg, v.names = "Prop", idvar = "site",
                    timevar = "category", direction = "wide")
  #NA's to zeros
  x.wide[is.na(x.wide)] <- 0
  #sites become rownames
  rownames(x.wide) <- x.wide$site
  x.wide <- subset(x.wide, select=-site)
  
  # PCA
  x.prco <- princomp(x.wide)
  x.axes_variance <- x.prco$sdev ^ 2
  # print(round(x.axes_variance/sum(x.axes_variance),3)) #prints the proportional variance for each
  
  print(round(x.axes_variance/sum(x.axes_variance),3)[1:3]) #prints the proportional variance for first three axes
  
  x.firstthreeaxes<-x.prco$scores[,1:3]
  x.firstthreeaxes
}

acacia.landuse.250 <- categoriseLandUse(
  read.csv("landuse_acacia/acacia_250m.csv"),
  "Natural",
  list(
    "Cultivated" = "Cultivated",
    "Plantation" = "Plantation",
    "Urban" = "Urban|Bare none vegetated|Mine"
  )
)
acacia.landuse.500 <- categoriseLandUse(
  read.csv("landuse_acacia/acacia_500m.csv"),
  "Natural",
  list(
    "Cultivated" = "Cultivated",
    "Plantation" = "Plantation",
    "Urban" = "Urban|Bare none vegetated|Mine"
  )
)
acacia.env.pca250 <- getPrincipalComponents(acacia.landuse.250)
acacia.env.pca500 <- getPrincipalComponents(acacia.landuse.500)

acacia.env.250 <- merge(acacia.env, acacia.env.pca250, by=0, all=TRUE)
rownames(acacia.env.250) <- acacia.env.250[,1]
acacia.env.250 <- acacia.env.250[2:length(acacia.env.250)]

acacia.env.500 <- merge(acacia.env, acacia.env.pca500, by=0, all=TRUE)
rownames(acacia.env.500) <- acacia.env.500[,1]
acacia.env.500 <- acacia.env.500[2:length(acacia.env.500)]

acacia.env.trans250 <- read.csv("landuse_acacia/acacia_trans_250m.csv")
acacia.env.250 <- cbind(acacia.env.250, trans_250 = acacia.env.trans250[acacia.env.trans250$transformed_land == "Transformed", ]$Prop)

acacia.env.trans500 <- read.csv("landuse_acacia/acacia_trans_500m.csv")
acacia.env.500 <- cbind(acacia.env.500, trans_500 = acacia.env.trans500[acacia.env.trans500$transformed_land == "Transformed", ]$Prop)

write.csv(acacia.env.250, "Acacia_env/acacia.env.final.250.csv")
write.csv(acacia.env.500, "Acacia_env/acacia.env.final.500.csv")

library(plyr)
acacia.env.all <- merge(acacia.env, acacia.env.pca250, by=0, all=TRUE)
acacia.env.all <- rename(acacia.env.all, c("Comp.1"="250.Comp.1", "Comp.2"="250.Comp.2", "Comp.3"="250.Comp.3"))
rownames(acacia.env.all) <- acacia.env.all[,1]
acacia.env.all <- acacia.env.all[2:length(acacia.env.all)]
acacia.env.all <- merge(acacia.env.all, acacia.env.pca500, by=0, all=TRUE)
acacia.env.all <- rename(acacia.env.all, c("Comp.1"="500.Comp.1", "Comp.2"="500.Comp.2", "Comp.3"="500.Comp.3"))
rownames(acacia.env.all) <- acacia.env.all[,1]
acacia.env.all <- acacia.env.all[2:length(acacia.env.all)]
acacia.env.all <- cbind(acacia.env.all, trans_250 = acacia.env.trans250[acacia.env.trans250$transformed_land == "Transformed", ]$Prop)
acacia.env.all <- cbind(acacia.env.all, trans_500 = acacia.env.trans500[acacia.env.trans500$transformed_land == "Transformed", ]$Prop)

require(data.table) # v1.9.5+
bw_vs.dsttable <- data.frame(
  dcast(setDT(read.csv("csv/Distance_Table_Attributes.csv")), Site.Number ~ Landcover, value.var = c("Perimeter_Area_ratio", "Distance")),
  row.names = "Site.Number"
)
# eliminate NA columns
bw_vs.dsttable <- bw_vs.dsttable[, colSums(is.na(bw_vs.dsttable)) == 0]
acacia.env.all <- cbind(acacia.env.all, bw_vs.dsttable)


#### Pivot land use
.addSuffixSetNAsToZeroes <- function(x, suffix) {
  colnames(x) <- paste(colnames(x), suffix, sep = "_")
  x[is.na(x)] <- 0
  x
}

acacia.landuse.250.pivot <- .addSuffixSetNAsToZeroes(cbind(
  data.frame(
    dcast(setDT(acacia.landuse.250), site ~ category, value.var = c("Prop"), fun = sum),
    row.names = "site"
  ),
  data.frame(
    dcast(setDT(acacia.landuse.250[acacia.landuse.250$category == "Natural"]), site ~ landuse, value.var = c("Prop")),
    row.names = "site"
  )
), "250")

acacia.landuse.500.pivot <- .addSuffixSetNAsToZeroes(cbind(
  data.frame(
    dcast(setDT(acacia.landuse.500), site ~ category, value.var = c("Prop"), fun = sum),
    row.names = "site"
  ),
  data.frame(
    dcast(setDT(acacia.landuse.500[acacia.landuse.500$category == "Natural"]), site ~ landuse, value.var = c("Prop")),
    row.names = "site"
  )
), "500")

acacia.env.all <- merge(acacia.env.all, acacia.landuse.250.pivot, by=0, all=TRUE)
rownames(acacia.env.all) <- acacia.env.all[,1]
acacia.env.all <- acacia.env.all[2:length(acacia.env.all)]
acacia.env.all <- merge(acacia.env.all, acacia.landuse.500.pivot, by=0, all=TRUE)
rownames(acacia.env.all) <- acacia.env.all[,1]
acacia.env.all <- acacia.env.all[2:length(acacia.env.all)]


#########################################################################
#################### Community Matrix ##################################

bw_vs_all <- read.csv("bw_vs_all.csv")

# exclude Ignota columns
bw_vs_all <- bw_vs_all[,grep("Ignota", names(bw_vs_all), invert = T)]
# add factor labels
bw_vs_all$Host <- factor(
  as.character(bw_vs_all$Host),
  levels = c("A_mearnsii", "A_sieberiana")
)

bw_vs_all_expanded <- cbind(
  Host = bw_vs_all$Host,
  Sample = bw_vs_all$Sample,
  Site = unlist(lapply(strsplit(as.character(bw_vs_all$Sample), "\\_"), "[[", 1)),
  Region = gsub("MPL", "MP", substring(gsub("MP", "MPL",bw_vs_all$Sample),1,3)),
  Season = bw_vs_all$Season,
  subset(bw_vs_all, select = -c(Host, Sample, Season))
)

bw_vs_all.speciescolumns <- bw_vs_all[,4:length(bw_vs_all)]
bw_vs_all.speciescolumns.nosingledoubletons <- bw_vs_all.speciescolumns[!apply(bw_vs_all.speciescolumns, 2, function(col) { length(col[col > 0]) <= 1 })]

bw_vs_adjacent_all <- bw_vs_all
bw_vs_adjacent_all$Host <- as.character(bw_vs_adjacent_all$Host)
bw_vs_adjacent_all$Host[
  grepl("KZN[23]|MP[34]", bw_vs_adjacent_all$Sample) &
    bw_vs_adjacent_all$Host == "A_mearnsii"
  ] <- "A_mearnsii (adjacent)"
bw_vs_adjacent_all$Host = factor(
  bw_vs_adjacent_all$Host,
  # reorder factor levels, ensuring that A_mearnsii (adjacent) is last
  levels = c("A_mearnsii", "A_sieberiana", "A_mearnsii (adjacent)")
)

with(bw_vs_all, table(Host))


bw_vs_all.nosingledoubletons <- cbind(bw_vs_all[,1:3], bw_vs_all.speciescolumns.nosingledoubletons)
bw_vs_adjacent_all.nosingledoubletons <- cbind(bw_vs_adjacent_all[,1:3], bw_vs_all.speciescolumns.nosingledoubletons)
rm(bw_vs_all.speciescolumns)
rm(bw_vs_all.speciescolumns.nosingledoubletons)


bw_vs.host_site_season <- bw_vs_all
bw_vs.host_site_season$Host <- as.character(bw_vs_all$Host)
bw_vs.host_site_season$Host[bw_vs_all$Host == 'A_mearnsii'] <- "Am"
bw_vs.host_site_season$Host[bw_vs_all$Host == 'A_sieberiana'] <- "Vs"
# create a new Site column containing just the site extracted from the Sample column
bw_vs.host_site_season$Site <- unlist(lapply(strsplit(as.character(bw_vs.host_site_season$Sample), "\\_"), "[[", 1))
# remove Sample column
bw_vs.host_site_season <- subset(bw_vs.host_site_season, select = -c(Sample))
# combine number of rows and sum, per site
bw_vs.host_site_season <- aggregate(. ~ Host + Site + Season, bw_vs.host_site_season, sum)
# set the rownames to MP1_As, etc...
rownames(bw_vs.host_site_season) <- do.call(paste, c(bw_vs.host_site_season[c("Host", "Site", "Season")], sep = "_"))
# remove Site, Host & Season columns
bw_vs.host_site_season <- subset(bw_vs.host_site_season, select = -c(Host, Site, Season))

groupBySite <- function(x) {
  x$Site <- unlist(lapply(strsplit(as.character(x$Sample), "\\_"), "[[", 1))
  # remove Host, Sample & Season columns
  x <- subset(x, select = -c(Host, Sample, Season))
  # combine number of rows and sum, per site
  x <- aggregate(. ~ Site, x, sum)
  # set rownames to Site
  rownames(x) <- x$Site
  # drop the Site column
  x <- subset(x, select =-Site)
  # return the result
  x
}

# only A_mearsii
bw <- groupBySite(bw_vs_all[bw_vs_all$Host == 'A_mearnsii',])
# only A_sieberiana
vs <- groupBySite(bw_vs_all[bw_vs_all$Host == 'A_sieberiana',])
# A_mearnsii adjacent
bw_adjacent <- groupBySite(bw_vs_adjacent_all[bw_vs_adjacent_all$Host == 'A_mearnsii (adjacent)',])


bw_vs <- bw_vs_all
bw_vs$Host <- as.character(bw_vs$Host)
bw_vs$Host[bw_vs_all$Host == 'A_mearnsii'] <- "Am"
bw_vs$Host[bw_vs_all$Host == 'A_sieberiana'] <- "Vs"
# create a new Site column containing just the site extracted from the Sample column
bw_vs$Site <- unlist(lapply(strsplit(as.character(bw_vs$Sample), "\\_"), "[[", 1))
# remove Sample & Season columns
bw_vs <- subset(bw_vs, select = -c(Sample, Season))
# combine number of rows and sum, per site
bw_vs <- aggregate(. ~ Host + Site, bw_vs, sum)
# set the rownames to MP1_As, etc...
rownames(bw_vs) <- do.call(paste, c(bw_vs[c("Site", "Host")], sep = "_"))
# remove Site & Host columns
bw_vs <- subset(bw_vs, select = -c(Site, Host))



groupByFamily <- function(SC) {
  names(SC)<-unlist(lapply(strsplit(names(SC), "\\_"), "[[", 1))
  pv<-unique(names(SC))
  
  families<-data.frame(SC)
  for (i in 1:length(pv)){
    #i=1
    tmp<-rowSums(data.frame(fam=SC[,which(names(SC)==pv[i])]))
    families<-cbind(families, data.frame(add=tmp))
  }
  
  families<-families[,-(1:length(names(SC)))]
  names(families)<-pv
  families
}
.pa<-function(x) {.columnnum(x, pa=T)}
.columnnum<-function(x, pa=FALSE){
  x<-x[sapply(x, is.numeric) ]
  if (pa) x[x>0]<-1
  return(x)
}
bw.family.abundances <- groupByFamily(bw)
bw.family.morphossp <- groupByFamily(.pa(bw))
vs.family.abundances <- groupByFamily(vs)
vs.family.morphossp <- groupByFamily(.pa(vs))

bw.rotated <- as.data.frame(t(bw))
bw.species <- cbind(
  Family = unlist(lapply(strsplit(names(bw), "\\_"), "[[", 1)),
  MorphousSp = .pa(colSums(bw)),
  MorphousSpKZN = .pa(rowSums(bw.rotated[grep("KZN", names(bw.rotated), value = T)])),
  MorphousSpMP = .pa(rowSums(bw.rotated[grep("MP", names(bw.rotated), value = T)])),
  Commonness = colSums(.pa(bw)),
  Abundance = colSums(bw),
  bw.rotated
)

vs.rotated <- as.data.frame(t(vs))
vs.species <- cbind(
  Family = unlist(lapply(strsplit(names(vs), "\\_"), "[[", 1)),
  MorphousSp = .pa(colSums(vs)),
  MorphousSpKZN = .pa(rowSums(vs.rotated[grep("KZN", names(vs.rotated), value = T)])),
  MorphousSpMP = .pa(rowSums(vs.rotated[grep("MP", names(vs.rotated), value = T)])),
  Commonness = colSums(.pa(vs)),
  Abundance = colSums(vs),
  vs.rotated
)

bw_adjacent.rotated <- as.data.frame(t(bw_adjacent))
bw_adjacent.species <- cbind(
  Family = unlist(lapply(strsplit(names(bw_adjacent), "\\_"), "[[", 1)),
  MorphousSp = .pa(colSums(bw_adjacent)),
  MorphousSpKZN = .pa(rowSums(bw_adjacent.rotated[grep("KZN", names(bw_adjacent.rotated), value = T)])),
  MorphousSpMP = .pa(rowSums(bw_adjacent.rotated[grep("MP", names(bw_adjacent.rotated), value = T)])),
  Commonness = colSums(.pa(bw_adjacent)),
  Abundance = colSums(bw_adjacent),
  bw_adjacent.rotated
)

library(descr)
bw.frequencies <- as.data.frame(freq(colSums(.pa(bw)), plot = F))
vs.frequencies <- as.data.frame(freq(colSums(.pa(vs)), plot = F))

###################### Split Hemiptera, Coleoptera and Ants
bw_vs_all.ant_colmns <- which(grepl("Formicidae",names(bw_vs_all)))
bw_vs_all.coleoptera <- bw_vs_all[,1:(bw_vs_all.ant_colmns[1] - 1)]
bw_vs_all.ants <- cbind(bw_vs_all[,1:3], bw_vs_all[,bw_vs_all.ant_colmns[1]:bw_vs_all.ant_colmns[length(bw_vs_all.ant_colmns)]])
bw_vs_all.hemiptera <- cbind(bw_vs_all[,1:3], bw_vs_all[,(max(bw_vs_all.ant_colmns)+1):length(names(bw_vs_all))])

bw_vs.ant_colmns = which(grepl("Formicidae",names(bw_vs)))
bw_vs.coleoptera <- bw_vs[,1:(bw_vs.ant_colmns[1] - 1)]
bw_vs.ants <- bw_vs[,bw_vs.ant_colmns[1]:bw_vs.ant_colmns[length(bw_vs.ant_colmns)]]
bw_vs.hemiptera <- bw_vs[,(max(bw_vs.ant_colmns)+1):length(names(bw_vs))]

# remove family from Formicidae
names(bw_vs.ants) <- substring(names(bw_vs.ants), nchar("Formicidae_") + 1)

bw_vs.antsubfamilites <- cbind(bw_vs.coleoptera, bw_vs.ants, bw_vs.hemiptera)

###################### Family counts
bw_vs.family <- groupByFamily(bw_vs.antsubfamilites)
bw_vs.family_counts <- cbind(
  KZN_AM = sum(.pa(colSums(bw_vs.family[grep("KZN.+Am", rownames(bw_vs.family)),]))),
  MP_AM = sum(.pa(colSums(bw_vs.family[grep("MP.+Am", rownames(bw_vs.family)),]))),
  KZN_VS = sum(.pa(colSums(bw_vs.family[grep("KZN.+Vs", rownames(bw_vs.family)),]))),
  MP_VS = sum(.pa(colSums(bw_vs.family[grep("MP.+Vs", rownames(bw_vs.family)),]))),
  KZN = sum(.pa(colSums(bw_vs.family[grep("KZN", rownames(bw_vs.family)),]))),
  MP = sum(.pa(colSums(bw_vs.family[grep("MP", rownames(bw_vs.family)),]))),
  VS = sum(.pa(colSums(bw_vs.family[grep("Vs", rownames(bw_vs.family)),]))),
  AM = sum(.pa(colSums(bw_vs.family[grep("Am", rownames(bw_vs.family)),]))),
  AMAdj = sum(.pa(colSums(bw_vs.family[grep("(KZN[23]|MP[34])_Am", rownames(bw_vs.family)),]))),
  All = sum(.pa(colSums(bw_vs.family)))
)

bw_vs.per_family_counts <- as.data.frame(
  t(groupByFamily(.pa(as.data.frame(rbind(
    KZN_AM = colSums(bw_vs.antsubfamilites[grep("KZN.+Am", rownames(bw_vs.antsubfamilites)),]),
    MP_AM = colSums(bw_vs.antsubfamilites[grep("MP.+Am", rownames(bw_vs.antsubfamilites)),]),
    KZN_VS = colSums(bw_vs.antsubfamilites[grep("KZN.+Vs", rownames(bw_vs.antsubfamilites)),]),
    MP_VS = colSums(bw_vs.antsubfamilites[grep("MP.+Vs", rownames(bw_vs.antsubfamilites)),]),
    KZN = colSums(bw_vs.antsubfamilites[grep("KZN", rownames(bw_vs.antsubfamilites)),]),
    MP = colSums(bw_vs.antsubfamilites[grep("MP", rownames(bw_vs.antsubfamilites)),]),
    VS = colSums(bw_vs.antsubfamilites[grep("Vs", rownames(bw_vs.antsubfamilites)),]),
    AM = colSums(bw_vs.antsubfamilites[grep("Am", rownames(bw_vs.antsubfamilites)),]),
    AMAdj = colSums(bw_vs.antsubfamilites[grep("(KZN[23]|MP[34])_Am", rownames(bw_vs.antsubfamilites)),]),
    All = colSums(bw_vs.antsubfamilites)
  )))))
)
bw_vs.per_family_counts$Order <- ""
bw_vs.per_family_counts[rownames(bw_vs.per_family_counts) %in% names(groupByFamily(bw_vs.coleoptera)), ]$Order <- "Coleoptera"
bw_vs.per_family_counts[rownames(bw_vs.per_family_counts) %in% names(groupByFamily(bw_vs.ants)), ]$Order <- "Hymenoptera"
bw_vs.per_family_counts[rownames(bw_vs.per_family_counts) %in% names(groupByFamily(bw_vs.hemiptera)), ]$Order <- "Hemiptera"
bw_vs.per_family_counts <- bw_vs.per_family_counts[order(
  -bw_vs.per_family_counts$AM,
  row.names(bw_vs.per_family_counts)
),]

write.csv(bw.family.abundances, "csv/bw.family.abundances.csv")
write.csv(bw.family.morphossp, "csv/bw.family.morphossp.csv")
write.csv(vs.family.abundances, "csv/vs.family.abundances.csv")
write.csv(vs.family.morphossp, "csv/vs.family.morphossp.csv")
write.csv(bw.species, "csv/bw.species.csv")
write.csv(vs.species, "csv/vs.species.csv")
write.csv(bw_adjacent.species, "csv/bw_adjacent.species.csv")
write.csv(bw.frequencies, "csv/bw.frequencies.csv")
write.csv(vs.frequencies, "csv/vs.frequencies.csv")
write.csv(rbind(
  Families = data.frame(bw_vs.family_counts, Order = ""),
  bw_vs.per_family_counts
), "csv/bw_vs_family_counts.csv")


###################### Abundances and richness

bs_vs.richness_abundance <- cbind(
  Coleoptera_Abundance = rowSums(bw_vs.coleoptera),
  Coleoptera_Richness = rowSums(.pa(bw_vs.coleoptera)),
  Ants_Abundance = rowSums(bw_vs.ants),
  Ants_Richness = rowSums(.pa(bw_vs.ants)),
  Hemiptera_Abundance = rowSums(bw_vs.hemiptera),
  Hemiptera_Richness = rowSums(.pa(bw_vs.hemiptera))
)
write.csv(bs_vs.richness_abundance, "csv/Abundance & Richness.csv")


###################### Chapter 3

bw_vs_adjacent <- bw_vs_adjacent_all
names(bw_vs_adjacent)[names(bw_vs_adjacent) == "Sample"] <- "Site"
bw_vs_adjacent$Site <- unlist(lapply(strsplit(as.character(bw_vs_adjacent$Site), "\\_"), "[[", 1))

bw_vs_adjacent.ant_columns <- which(grepl("Formicidae",names(bw_vs_adjacent)))
bw_vs_adjacent.coleoptera_columns <- 4:(bw_vs_adjacent.ant_columns[1] - 1)
bw_vs_adjacent.hemiptera_columns <- (max(bw_vs_adjacent.ant_columns)+1):length(names(bw_vs_adjacent))

.calculateAbundanceSpecpool <- function(x) {
  cbind(
    Abundance = sum(subset(x, select = -c(Host, Site, Season))),
    as.data.frame(specpool(subset(aggregate(. ~ Site, subset(x, select = -c(Host, Season)), sum), select = -c(Site))))
  )
}
.calculateAbundanceSpecpoolByHost <- function(prefix, host) {
  out <- rbind(
    "Overall" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host))),
    "MP" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host) & grepl("MP", Site))),
    "KZN" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host) & grepl("KZN", Site))),
    "Spring" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host) & Season == "Spring")),
    "Summer" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host) & Season == "Summer")),
    "Winter" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host) & Season == "Winter")),
    "Coleoptera" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host), select=c(1:3, bw_vs_adjacent.coleoptera_columns))),
    "Hemiptera" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host), select=c(1:3, bw_vs_adjacent.hemiptera_columns))),
    "Formicidae" = .calculateAbundanceSpecpool(subset(bw_vs_adjacent, grepl(host, Host), select=c(1:3, bw_vs_adjacent.ant_columns)))
  )
  rownames(out) <- paste(prefix, rownames(out))
  out
}

write.csv(rbind(
  .calculateAbundanceSpecpoolByHost("A. mearnsii", "A_mearnsii"), # this will get all A_mearnsii and adjacent
  .calculateAbundanceSpecpoolByHost("Adjacent", "A_mearnsii \\(adjacent\\)"), # this will only get adjacent
  .calculateAbundanceSpecpoolByHost("V. sieberiana", "A_sieberiana")
), "csv/Richness Estimates.csv")
