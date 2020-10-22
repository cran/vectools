#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClassUnion (".xlist", c ("NULL", "list") )
setClassUnion (".xcharacter", c ("NULL", "character") )

setClass ("VectorLike", contains="VIRTUAL")
setClass ("MatrixLike", contains = c("VIRTUAL", "VectorLike") )

setClass ("ObjectArray", contains="VectorLike",
	slots = list (
		CLASS="character",
		N="integer",
		n="integer",
		names=".xlist",
		data="list")
)

setClass ("VMap", contains="ObjectArray")

setClass ("NestMatrix", contains="ObjectArray",
	slots = list (
		nrs="matrix",
		ncs="matrix",
		conform="logical",
		recursive="logical")
)

setClass ("SectMatrix", contains="MatrixLike",
	slots = list (
		header="logical",
		NS="integer",
		ns="integer",
		vmap="VMap",
		nr="integer",
		nc="integer",
		rnames=".xcharacter",
		cnames=".xcharacter",
		data="matrix")
)

setClass ("PartMatrix", contains="SectMatrix")

setClass ("MatrixArray", contains="ObjectArray",
	slots = list (
		conform="logical")
)

setClass ("ZERO", contains="VectorLike", slots = c (k="integer") )
ZERO <- new ("ZERO")

setMethod ("show", "VectorLike", function (object) print (object) )

setMethod ("%*%", c ("MatrixArray", "MatrixArray"), function (x, y) .vmmult2 (x, y) )
setMethod ("%*%", c ("MatrixArray", "matrix"), function (x, y) .vmmult2 (x, y) )
setMethod ("%*%", c ("matrix", "MatrixArray"), function (x, y) .vmmult2 (x, y) )

objtag = function (...) UseMethod ("objtag")
rep2 = function (...) UseMethod ("rep2")

ghead = function (...) UseMethod ("ghead")
headt = function (...) UseMethod ("headt")

pool = function (...) UseMethod ("pool")
pool2 = function (...) UseMethod ("pool2")
