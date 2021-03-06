library(iNEXT)

.prepareAcaciaData <- function(x) {
  # combine first 3 columns with presence/absence result
  out <- cbind(x[,1:3], .pa(x))
  # only A_mearsii
  out <- out[out$Host == 'A_mearnsii',]
  # set the rownames to MP1_F1_Summer, etc...
  rownames(out) <- do.call(paste, c(out[c("Sample", "Season")], sep = "_"))
  # create a new Site column containing just the site extracted from the Sample column
  out$Site <- unlist(lapply(strsplit(as.character(out$Sample), "\\_"), "[[", 1))
  # remove Host, Sample & Season columns
  out <- subset(out, select = -c(Host, Sample, Season))
  # combine number of rows and sum, per site
  out <- cbind(SampleUnit = aggregate(. ~ Site, out, length)[,2], aggregate(. ~ Site, out, sum))
  # set rownames to Site
  rownames(out) <- out$Site
  # drop the Site column
  out <- subset(out, select =-Site)
  
  out
}


bw_sample <- .prepareAcaciaData(bw_vs_all)
bw_sample.iNEXT <- iNEXT(apply(bw_sample, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

bw_sample.ants <- .prepareAcaciaData(bw_vs_all.ants)
bw_sample.ants.iNEXT <- iNEXT(apply(bw_sample.ants, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

bw_sample.coleoptera <- .prepareAcaciaData(bw_vs_all.coleoptera)
bw_sample.coleoptera.iNEXT <- iNEXT(apply(bw_sample.coleoptera, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

bw_sample.hemiptera <- .prepareAcaciaData(bw_vs_all.hemiptera)
bw_sample.hemiptera.iNEXT <- iNEXT(apply(bw_sample.hemiptera, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

prepareAcaciaBySiteHost <- function(x) {
  # combine first 3 columns with presence/absence result
  out <- cbind(x[,1:3], .pa(x))
  
  out$Host <- as.character(factor(out$Host, levels = c("A_mearnsii", "A_sieberiana"), labels = c("Am", "Vs")))
  out$Sample <- unlist(lapply(strsplit(as.character(out$Sample), "\\_"), "[[", 1))
  # remove Season column
  out <- subset(out, select = -Season)
  # combine number of rows and sum, per site
  out <- cbind(SampleUnit = aggregate(. ~ Sample + Host, out, length)[,3], aggregate(. ~ Sample + Host, out, sum))
  # set the rownames to MP_As, etc...
  rownames(out) <- do.call(paste, c(out[c("Sample", "Host")], sep = "_"))
  # drop the Site column
  out <- subset(out, select =-c(Sample, Host))
  
  out
}

prepareAcaciaBySeasonSiteHost <- function(x, season) {
  out <- x[x$Season == season,]
  # combine first 3 columns with presence/absence result
  out <- cbind(out[,1:3], .pa(out))
  
  out$Host <- as.character(factor(out$Host, levels = c("A_mearnsii", "A_sieberiana"), labels = c("Am", "Vs")))
  out$Sample <- unlist(lapply(strsplit(as.character(out$Sample), "\\_"), "[[", 1))
  # remove Season column
  out <- subset(out, select = -Season)
  # combine number of rows and sum, per site
  out <- cbind(SampleUnit = aggregate(. ~ Sample + Host, out, length)[,3], aggregate(. ~ Sample + Host, out, sum))
  # set the rownames to MP_As, etc...
  rownames(out) <- do.call(paste, c(out[c("Sample", "Host")], sep = "_"))
  # drop the Site column
  out <- subset(out, select =-c(Sample, Host))
  
  out
}

prepareAcaciaByHostRegionHost <- function(x) {
  # combine first 3 columns with presence/absence result
  out <- cbind(x[,1:3], .pa(x))
  
  out$Host <- as.character(factor(out$Host, levels = c("A_mearnsii", "A_sieberiana"), labels = c("Am", "Vs")))
  out$Sample <- as.character(out$Sample)
  out$Sample[grepl("KZN", out$Sample)] <- "KZN"
  out$Sample[grepl("MP", out$Sample)] <- "MP"
  # remove Season column
  out <- subset(out, select = -Season)
  # combine number of rows and sum, per site
  out <- cbind(SampleUnit = aggregate(. ~ Sample + Host, out, length)[,3], aggregate(. ~ Sample + Host, out, sum))
  # set the rownames to MP_As, etc...
  rownames(out) <- do.call(paste, c(out[c("Sample", "Host")], sep = "_"))
  # drop the Site column
  out <- subset(out, select =-c(Sample, Host))
  
  out
}

prepareAcaciaByHost <- function(x) {
  # combine first 3 columns with presence/absence result
  out <- cbind(x[,1:3], .pa(x))
  
  out$Host <- as.character(factor(out$Host, levels = c("A_mearnsii", "A_sieberiana"), labels = c("Am", "Vs")))
  # remove Sample, Season column
  out <- subset(out, select = -c(Sample, Season))
  # combine number of rows and sum, per site
  out <- cbind(SampleUnit = aggregate(. ~ Host, out, length)[,3], aggregate(. ~ Host, out, sum))
  # set the rownames to MP_As, etc...
  rownames(out) <- out$Host
  # drop the Site column
  out <- subset(out, select =-Host)
  
  out
}

prepareBySeasonHost <- function (x, host, season) {
  result <- x[x$Host == host & x$Season == season,]
  # combine first 3 columns with presence/absence result
  result <- cbind(result[,1:3], .pa(result))
  
  # create a new Site column containing just the site extracted from the Sample column
  result$Site <- unlist(lapply(strsplit(as.character(result$Sample), "\\_"), "[[", 1))
  # remove Host, Sample and Season columns
  result <- subset(result, select = -c(Host, Sample, Season))
  # combine number of rows and sum, per site
  result <- cbind(SampleUnit = aggregate(. ~ Site, result, length)[,3], aggregate(. ~ Site, result, sum))
  # set rownames to Site
  rownames(result) <- result$Site
  # drop the Site column
  result <- subset(result, select =-Site)
  # remove sites with all zeros
  result <- result[rowSums(result[,2:length(names(result))]) > 0,]
  
  result
}

prepareByRegionSeason <- function (x, region, season) {
  result <- x[grepl(region, x$Sample) & x$Season == season,]
  # combine first 3 columns with presence/absence result
  result <- cbind(result[,1:3], .pa(result))
  
  # remove Sample and Season columns
  result <- subset(result, select = -c(Sample, Season))
  # combine number of rows and sum, per site
  result <- cbind(SampleUnit = aggregate(. ~ Host, result, length)[,3], aggregate(. ~ Host, result, sum))
  # set rownames to Host
  rownames(result) <- result$Host
  # drop the Host column
  result <- subset(result, select =-Host)
  # remove columns with all zeros
  result <- result[,colSums(result) > 0]
  
  result
}

prepareBySeasonHostRegion <- function (x, host, season) {
  result <- x[x$Host == host & x$Season == season,]
  # combine first 3 columns with presence/absence result
  result <- cbind(result[,1:3], .pa(result))
  
  #create a new Region column containing KZN or MPL
  result$Region <- substring(gsub("MP", "MPL", result$Sample),1,3)
  # remove Host, Sample and Season columns
  result <- subset(result, select = -c(Host, Sample, Season))
  # combine number of rows and sum, per region
  result <- cbind(SampleUnit = aggregate(. ~ Region, result, length)[,3], aggregate(. ~ Region, result, sum))
  # set rownames to Region
  rownames(result) <- result$Region
  # drop the Region column
  result <- subset(result, select =-Region)
  # remove sites with all zeros
  result <- result[rowSums(result[,2:length(names(result))]) > 0,]
  
  result
}

bw_vs_all.enoughtrees <- bw_vs_all[!grepl("KZN3", bw_vs_all$Sample),]
bw_vs_all.enoughtrees <- bw_vs_all.enoughtrees[!(
    grepl("MP3", bw_vs_all$Sample) &&
    (
      bw_vs_all$Season == "Spring" ||
      bw_vs_all$Season == "Winter" && bw_vs_all$Host == "V_sieberiana"
    )
  )
,]
bw_vs_site_host <- prepareAcaciaBySiteHost(bw_vs_all)
bw_vs_site_host.enoughtrees.summer <- prepareAcaciaBySeasonSiteHost(bw_vs_all.enoughtrees, "Summer")
bw_vs_site_host.enoughtrees.spring <- prepareAcaciaBySeasonSiteHost(bw_vs_all.enoughtrees, "Spring")
bw_vs_site_host.enoughtrees.winter <- prepareAcaciaBySeasonSiteHost(bw_vs_all.enoughtrees, "Winter")

bw_vs_site_host.iNEXT <- iNEXT(apply(bw_vs_site_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000)
bw_vs_site_host.iNEXT.10 <- iNEXT(apply(bw_vs_site_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 10)
bw_vs_site_host.iNEXT.15 <- iNEXT(apply(bw_vs_site_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 15)
bw_vs_site_host.iNEXT.20 <- iNEXT(apply(bw_vs_site_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 20)
bw_vs_site_host.iNEXT.enoughtrees.summer <- iNEXT(apply(bw_vs_site_host.enoughtrees.summer, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000)
bw_vs_site_host.iNEXT.enoughtrees.summer.7 <- iNEXT(apply(bw_vs_site_host.enoughtrees.summer, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 7)
bw_vs_site_host.iNEXT.enoughtrees.spring <- iNEXT(apply(bw_vs_site_host.enoughtrees.spring, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000)
bw_vs_site_host.iNEXT.enoughtrees.spring.7 <- iNEXT(apply(bw_vs_site_host.enoughtrees.spring, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 7)
bw_vs_site_host.iNEXT.enoughtrees.winter <- iNEXT(apply(bw_vs_site_host.enoughtrees.winter, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000)
bw_vs_site_host.iNEXT.enoughtrees.winter.7 <- iNEXT(apply(bw_vs_site_host.enoughtrees.winter, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq", nboot = 5000, endpoint = 7)

bw_vs_host <- prepareAcaciaByHost(bw_vs_all)
bw_vs_host.iNEXT <- iNEXT(apply(bw_vs_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

bw_vs_region_host <- prepareAcaciaByHostRegionHost(bw_vs_all)
bw_vs_region_host.iNEXT <- iNEXT(apply(bw_vs_region_host, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

# MP3, MP4, KZN2, KZN3
bw_vs_region_host.MP34KZN23 <- bw_vs_all
bw_vs_region_host.MP34KZN23 <- bw_vs_region_host.MP34KZN23[grepl("(MP[34]|KZN[23])", bw_vs_region_host.MP34KZN23$Sample),]
bw_vs_region_host.MP34KZN23 <- prepareAcaciaByHostRegionHost(bw_vs_region_host.MP34KZN23)
bw_vs_region_host.MP34KZN23.iNEXT <- iNEXT(apply(bw_vs_region_host.MP34KZN23, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

bw_vs_region_host.summer <- prepareAcaciaByHostRegionHost(bw_vs_all[bw_vs_all$Season == 'Summer',])
bw_vs_region_host.summer.iNEXT <- iNEXT(apply(bw_vs_region_host.summer, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")
bw_vs_region_host.winter <- prepareAcaciaByHostRegionHost(bw_vs_all[bw_vs_all$Season == 'Winter',])
bw_vs_region_host.winter.iNEXT <- iNEXT(apply(bw_vs_region_host.winter, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")
bw_vs_region_host.spring <- prepareAcaciaByHostRegionHost(bw_vs_all[bw_vs_all$Season == 'Spring',])
bw_vs_region_host.spring.iNEXT <- iNEXT(apply(bw_vs_region_host.spring, 1, as.numeric), q=c(0, 1, 2), datatype="incidence_freq")

host_season <- list(
  bw_summer = prepareBySeasonHost(bw_vs_all, 'A_mearnsii', 'Summer'),
  bw_spring = prepareBySeasonHost(bw_vs_all, 'A_mearnsii', 'Spring'),
  bw_winter = prepareBySeasonHost(bw_vs_all, 'A_mearnsii', 'Winter'),
  vs_summer = prepareBySeasonHost(bw_vs_all, 'A_sieberiana', 'Summer'),
  vs_spring = prepareBySeasonHost(bw_vs_all, 'A_sieberiana', 'Spring'),
  vs_winter = prepareBySeasonHost(bw_vs_all, 'A_sieberiana', 'Winter')
)

region_season <- list(
  KZN_summer = prepareByRegionSeason(bw_vs_all, 'KZN', 'Summer'),
  KZN_spring = prepareByRegionSeason(bw_vs_all, 'KZN', 'Spring'),
  KZN_winter = prepareByRegionSeason(bw_vs_all, 'KZN', 'Winter'),
  MP_summer = prepareByRegionSeason(bw_vs_all, 'MP', 'Summer'),
  MP_spring = prepareByRegionSeason(bw_vs_all, 'MP', 'Spring'),
  MP_winter = prepareByRegionSeason(bw_vs_all, 'MP', 'Winter')
)

bw_season_region <- list(
  Summer = prepareBySeasonHostRegion(bw_vs_all, "A_mearnsii", "Summer"),
  Spring = prepareBySeasonHostRegion(bw_vs_all, "A_mearnsii", "Spring"),
  Winter = prepareBySeasonHostRegion(bw_vs_all, "A_mearnsii", "Winter")
)

host_season_taxa <- list(
  bw_ants_summer = prepareBySeasonHost(bw_vs_all.ants, 'A_mearnsii', 'Summer'),
  bw_ants_spring = prepareBySeasonHost(bw_vs_all.ants, 'A_mearnsii', 'Spring'),
  bw_ants_winter = prepareBySeasonHost(bw_vs_all.ants, 'A_mearnsii', 'Winter'),
  bw_coleoptera_summer = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_mearnsii', 'Summer'),
  bw_coleoptera_spring = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_mearnsii', 'Spring'),
  bw_coleoptera_winter = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_mearnsii', 'Winter'),
  bw_hemiptera_summer = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_mearnsii', 'Summer'),
  bw_hemiptera_spring = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_mearnsii', 'Spring'),
  bw_hemiptera_winter = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_mearnsii', 'Winter'),
  
  vs_ants_summer = prepareBySeasonHost(bw_vs_all.ants, 'A_sieberiana', 'Summer'),
  vs_ants_spring = prepareBySeasonHost(bw_vs_all.ants, 'A_sieberiana', 'Spring'),
  vs_ants_winter = prepareBySeasonHost(bw_vs_all.ants, 'A_sieberiana', 'Winter'),
  vs_coleoptera_summer = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_sieberiana', 'Summer'),
  vs_coleoptera_spring = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_sieberiana', 'Spring'),
  vs_coleoptera_winter = prepareBySeasonHost(bw_vs_all.coleoptera, 'A_sieberiana', 'Winter'),
  vs_hemiptera_summer = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_sieberiana', 'Summer'),
  vs_hemiptera_spring = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_sieberiana', 'Spring'),
  vs_hemiptera_winter = prepareBySeasonHost(bw_vs_all.hemiptera, 'A_sieberiana', 'Winter')
)

host_season_taxa.iNEXT <- list()
host_season_taxa.iNEXT$bw_ants_summer = iNEXT(apply(host_season_taxa$bw_ants_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_ants_spring = iNEXT(apply(host_season_taxa$bw_ants_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
# error
host_season_taxa.iNEXT$bw_ants_winter = iNEXT(apply(host_season_taxa$bw_ants_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=50)
host_season_taxa.iNEXT$bw_coleoptera_summer = iNEXT(apply(host_season_taxa$bw_coleoptera_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_coleoptera_spring = iNEXT(apply(host_season_taxa$bw_coleoptera_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_coleoptera_winter = iNEXT(apply(host_season_taxa$bw_coleoptera_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_hemiptera_summer = iNEXT(apply(host_season_taxa$bw_hemiptera_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_hemiptera_spring = iNEXT(apply(host_season_taxa$bw_hemiptera_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$bw_hemiptera_winter = iNEXT(apply(host_season_taxa$bw_hemiptera_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_ants_summer = iNEXT(apply(host_season_taxa$vs_ants_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_ants_spring = iNEXT(apply(host_season_taxa$vs_ants_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_ants_winter = iNEXT(apply(host_season_taxa$vs_ants_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_coleoptera_summer = iNEXT(apply(host_season_taxa$vs_coleoptera_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_coleoptera_spring = iNEXT(apply(host_season_taxa$vs_coleoptera_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_coleoptera_winter = iNEXT(apply(host_season_taxa$vs_coleoptera_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_hemiptera_summer = iNEXT(apply(host_season_taxa$vs_hemiptera_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_hemiptera_spring = iNEXT(apply(host_season_taxa$vs_hemiptera_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season_taxa.iNEXT$vs_hemiptera_winter = iNEXT(apply(host_season_taxa$vs_hemiptera_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)


host_season.iNEXT <- list()
host_season.iNEXT$bw_summer = iNEXT(apply(host_season$bw_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season.iNEXT$bw_spring = iNEXT(apply(host_season$bw_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season.iNEXT$bw_winter = iNEXT(apply(host_season$bw_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season.iNEXT$vs_summer = iNEXT(apply(host_season$vs_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season.iNEXT$vs_spring = iNEXT(apply(host_season$vs_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
host_season.iNEXT$vs_winter = iNEXT(apply(host_season$vs_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)

region_season.iNEXT <- list()
region_season.iNEXT$KZN_summer = iNEXT(apply(region_season$KZN_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
region_season.iNEXT$KZN_spring = iNEXT(apply(region_season$KZN_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
region_season.iNEXT$KZN_winter = iNEXT(apply(region_season$KZN_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
region_season.iNEXT$MP_summer = iNEXT(apply(region_season$MP_summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
region_season.iNEXT$MP_spring = iNEXT(apply(region_season$MP_spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
region_season.iNEXT$MP_winter = iNEXT(apply(region_season$MP_winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)

bw_season_region.iNEXT = list()
bw_season_region.iNEXT$Summer = iNEXT(apply(bw_season_region$Summer, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
bw_season_region.iNEXT$Spring = iNEXT(apply(bw_season_region$Spring, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)
bw_season_region.iNEXT$Winter = iNEXT(apply(bw_season_region$Winter, 1, as.numeric), q=0, datatype="incidence_freq", nboot=5000)

install.packages("rJava")
.rs.restartR()

#save.image(file="2019.01.21_reruncodeusingincidence_freq.rdata")
load(file="2019.01.21_reruncodeusingincidence_freq.rdata")


# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201\\bin')

iNEXTtoExcel <- function(data, file){
  file1 <- paste0(file, "_DataInfo", format(Sys.Date(), format="%Y.%m.%d"), ".csv" )
  file2 <- paste0(file, "_AsyEst", format(Sys.Date(), format="%Y.%m.%d") , ".csv" )
  file3 <- paste0(file, "_iNextEst", format(Sys.Date(), format="%Y.%m.%d"), ".csv"  )
  write.csv(data$DataInfo, file=file1) # , sheetName="DataInfo")
  write.csv(data$AsyEst, file=file2)#, sheetName="BasicIndex", append=TRUE)
  sapply(names(data$iNextEst), function(x) {
    write.csv(data$iNextEst[[x]], file=paste0(file3, "_", x, ".csv"))
  })
  
}
# iNEXTtoExcel(bw_vs_site_host.iNEXT.enoughtrees.summer, "Output/bw_vs_site_host.iNEXT.enoughtrees.summer")
# iNEXTtoExcel(bw_vs_site_host.iNEXT.enoughtrees.summer.7, "Output/bw_vs_site_host.iNEXT.enoughtrees.summer.7")
iNEXTtoExcel(bw_vs_site_host.iNEXT.10, "Output/bw_vs_site_host.iNEXT.10")
iNEXTtoExcel(bw_vs_site_host.iNEXT.enoughtrees.spring.7, "Output/bw_vs_site_host.iNEXT.enoughtrees.spring.7")
iNEXTtoExcel(bw_vs_site_host.iNEXT.enoughtrees.summer.7, "Output/bw_vs_site_host.iNEXT.enoughtrees.summer.7")
iNEXTtoExcel(bw_vs_site_host.iNEXT.enoughtrees.winter.7, "Output/bw_vs_site_host.iNEXT.enoughtrees.winter.7")







# Produces: Error in rmultinom(B, n, pi.star) : NA in probability vector
#bw_vs.bytree <- bw_vs_all
#rownames(bw_vs.bytree) <- do.call(paste, c(bw_vs.bytree[c("Sample", "Season")], sep = "_"))
#bw_vs.bytree <- subset(bw_vs.bytree, select = -c(Host, Sample, Season))
#bw_vs.bytree <- bw_vs.bytree[rowSums(bw_vs.bytree) != 0, ]
#bw_vs.bytree.iNEXT <- iNEXT(apply(bw_vs.bytree, 1, function(x) { sort(as.numeric(x[x!=0]), decreasing=T) }), q=c(0, 1, 2), datatype="abundance")
