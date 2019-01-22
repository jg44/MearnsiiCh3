library(vegan)
bw_vs
betadiv_bw_vs <- betadiver(bw_vs)
plot(betadiv_bw_vs)
## The indices
betadiver(help=TRUE)
## The basic Whittaker index
bdiv_bw_vs <- betadiver(bw_vs, "w")
bdiv_bw_vs
######
library(betapart)
beta.multi(.pa(bw_vs), index.family = "sorensen")
