### R code from vignette source 'vectools.Rnw'

###################################################
### code chunk number 1: vectools.Rnw:35-39
###################################################
options(continue="  ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))
set.seed (1)


###################################################
### code chunk number 2: vectools.Rnw:52-53
###################################################
library (vectools)


###################################################
### code chunk number 3: vectools.Rnw:59-61
###################################################
myobject <- structure (0, class="myclass")
objtag.myclass <- function (object, ...) "<X>"


###################################################
### code chunk number 4: vectools.Rnw:64-66
###################################################
v <- ObjectArray ("myclass", c (8, 8) )
v [[1, 1]] <- myobject


###################################################
### code chunk number 5: vectools.Rnw:69-70
###################################################
v


###################################################
### code chunk number 6: vectools.Rnw:73-74
###################################################
head (v, 3)


###################################################
### code chunk number 7: vectools.Rnw:81-82
###################################################
x <- matrix (1:16, 4, 4)


###################################################
### code chunk number 8: vectools.Rnw:85-87
###################################################
pm <- as.PartMatrix (x, c (1, 3), c (1, 3) )
nm <- as.NestMatrix (pm)


###################################################
### code chunk number 9: vectools.Rnw:90-92
###################################################
nm
pm


###################################################
### code chunk number 10: vectools.Rnw:95-97
###################################################
nm [[1, 2]]
nm [[1, 2, drop=FALSE]]


###################################################
### code chunk number 11: vectools.Rnw:104-105
###################################################
x <- matrix (1:64, 8, 8)


###################################################
### code chunk number 12: vectools.Rnw:108-118
###################################################
sm <- as.SectMatrix (x, vmap = n22 (
    1, 8, #1
    1, 8,
    3, 8, #2
    3, 8,
    5, 8, #3
    5, 8,
    7, 8, #4
    7, 8
    ) )


###################################################
### code chunk number 13: vectools.Rnw:121-122
###################################################
sm


###################################################
### code chunk number 14: vectools.Rnw:125-126
###################################################
getSect (sm, 3)


###################################################
### code chunk number 15: vectools.Rnw:133-140
###################################################
#grouped by am and cyl
#with mean of mpg, by group
select (am, cyl,
    from (mtcars),
    group.by (am, cyl),
        count <- length (mpg),
        mean.mpg <- mean (mpg) )


###################################################
### code chunk number 16: vectools.Rnw:143-150
###################################################
#same as above
#but partitioned and sorted
selectf (am, cyl,
    from (mtcars),
    group.by (am, cyl), partition.by (am), sort.by (-am, -mean.mpg),
        count <- length (mpg),
        mean.mpg <- mean (mpg) )


###################################################
### code chunk number 17: vectools.Rnw:158-166
###################################################
getOption("SweaveHooks")[["fig"]]()
#single polygon
ps <- c (0, 1) %|*% eq.brot2 (5)
#multiple polygons
vm <- ps %]*% (
    bscl2 (seq (1.4, 0.7,, 4) ) %*% #scale
    btrl2 (,3.75) %{*}%             #translate
    eq.brot2 (8) )                  #rotate
polyplot (vm)


###################################################
### code chunk number 18: vectools.Rnw:174-175
###################################################
ghead (iris)


###################################################
### code chunk number 19: vectools.Rnw:181-182
###################################################
headt (sm, 6, c (1, 2) )


