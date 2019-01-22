library(vegan)
bw_vs.permanova <- bw_vs_all_expanded[rowSums(bw_vs_all_expanded[,6:length(bw_vs_all_expanded)]) > 0,]

bw_vs.permanova.result <- adonis( bw_vs.permanova[, 6:length(bw_vs.permanova) ] ~ Host * Site * Region * Season, bw_vs.permanova, permutations = 999, method = "bray")

### Host*Region*Site*Season
bw_vs.permanova.result2 <- adonis( bw_vs.permanova[, 6:length(bw_vs.permanova) ] ~ Host * Region * Site * Season, bw_vs.permanova, permutations = 999, method = "bray")


### *Region*Site*Host*Season
> bw_vs.permanova.resultfinal <- adonis( bw_vs.permanova[, 6:length(bw_vs.permanova) ] ~ Region * Site * Host * Season, bw_vs.permanova, permutations = 999, method = "bray")
> bw_vs.permanova.resultfinal