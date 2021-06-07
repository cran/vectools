#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

ZERO <- new ("Zero")

SQUARE = Rect (glist = list (col="lightblue") )
CUBE = Cuboid (glist = list (col="#F5F5DCB0") )

TCUBE = CUBE %]*% (brot3y (0.125 * pi) %*% brot3x (0.125 * pi) )

