#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("VectorLike", contains="VIRTUAL")

setClass ("MatrixLike", contains = c("VIRTUAL", "VectorLike") )

setClass ("ObjectArray", contains="VectorLike",
	slots = list (
		nbd="integer",
		dim="integer",
		names="list",
		data="list")
)

setClass ("NestMatrix", contains="MatrixLike",
	slots = list (
		nr="integer",
		nc="integer",
		rnames="character",
		cnames="character",
		data="ObjectArray")
)

setClass ("SectMatrix", contains="MatrixLike",
	slots = list (
		header="logical",
		nfd="integer",
		nsect="integer",
		vmap="ObjectArray",
		nr="integer",
		nc="integer",
		rnames="character",
		cnames="character",
		data="matrix")
)

setClass ("PartMatrix", contains="SectMatrix")

setMethod ("show", "VectorLike", function (object) print (object) )

objtag = function (...) UseMethod ("objtag")

headg = function (...) UseMethod ("headg")
headt = function (...) UseMethod ("headt")
