# correlation Lat vs rarefied richness pooled data
library(stats)
x <- c(63.5, 68.02, 72, 59.3, 64.518, 90, 71.473, 139.604)
y <- c(-26.233611, -26.12666667, -25.96138889, -25.09777778, -25.05111111, -24.58388889, -23.8025,	-23.02305556)
cor.test(x, y, method = "pearson", alternative = "two.sided")

# correlation Lat vs rarefied richness 2009
x <- c(40,	48,	56,	27.5,	47.237,	57,	59,	45.167,	102.12)
y <- c(-26.233611, -26.12666667, -25.96138889, -25.25277778, -25.09777778, -25.05111111, -24.58388889, -23.8025,	-23.02305556)
cor.test(x, y, method = "pearson", alternative = "two.sided")

#correlation elevation vs rarefied richness pooled data
x <- c(63.5, 68.02, 72, 59.3, 64.518, 90, 71.473, 139.604)
y <- c(1776.8,	1692.3,	1625.3,	1491,	1342,	1152.6,	964.8,	819.6)
cor.test(x, y, method = "pearson", alternative = "two.sided")

