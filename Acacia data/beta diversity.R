library(vegetarian)

###beta diversity overall
d(bw_vs, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_vs, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_vs, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

####beta diversity by host q=0,1,2

#bw
d(bw, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw, lev = "beta", wts = FALSE, q = 2, boot = TRUE)
#bw adjacent
d(bw_adjacent, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_adjacent, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_adjacent, lev = "beta", wts = FALSE, q = 2, boot = TRUE)
#vs
d(vs, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(vs, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(vs, lev = "beta", wts = FALSE, q = 2, boot = TRUE)




#####beta diversity by region q=0,1,2
#KZN
d(bw_vs.KZN, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_vs.KZN, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_vs.KZN, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

#MP
d(bw_vs.MP, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_vs.MP, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_vs.MP, lev = "beta", wts = FALSE, q = 2, boot = TRUE)



######beta diversity by host and region q=0,1,2
#bw
d(bw.MP, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw.MP, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw.MP, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

d(bw.KZN, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw.KZN, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw.KZN, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

#vs q=0,1,2 MISSING
d(vs.MP, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(vs.MP, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(vs.MP, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

d(vs.KZN, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(vs.KZN, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(vs.KZN, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

#bw adjacent a=0,1,2 MISSING
d(bw_adjacent.MP, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_adjacent.MP, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_adjacent.MP, lev = "beta", wts = FALSE, q = 2, boot = TRUE)

d(bw_adjacent.KZN, lev = "beta", wts = FALSE, q = 0, boot = TRUE)
d(bw_adjacent.KZN, lev = "beta", wts = FALSE, q = 1, boot = TRUE)
d(bw_adjacent.KZN, lev = "beta", wts = FALSE, q = 2, boot = TRUE)