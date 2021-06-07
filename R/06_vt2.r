#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClassUnion (".xlist", c ("NULL", "list") )
setClassUnion (".xstring", c ("NULL", "character") )
setClassUnion (".xmatrix", c ("NULL", "matrix") )
setClassUnion (".xnumeric", c ("NULL", "numeric") )
setClassUnion (".grid_fill_color", c ("NULL", "character", "matrix") )

setClass ("VecLike", contains="VIRTUAL")
setClass ("MatrixLike", contains = c ("VIRTUAL", "VecLike") )

setClass ("ObjectArray", contains="VecLike",
	slots = list (
		CLASS="character",
		N="integer",
		n="integer",
		names=".xlist",
		data="list")
)

setClass ("VMap", contains="ObjectArray")

setClass ("NestMatrix", contains = c ("ObjectArray", "MatrixLike"),
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
		rnames=".xstring",
		cnames=".xstring",
		data="matrix")
)

setClass ("PartMatrix", contains="SectMatrix")

setClass ("Zero", contains="VecLike", slots = c (k="integer") )

setClass ("GeomObject", contains = c ("VecLike", "VIRTUAL"),
	slots = list (
		D="integer")
)

setClass ("GeomArray", contains = c ("GeomObject", "ObjectArray") )

setClass ("ImageArray", contains="ObjectArray")

setClass ("MatrixArray", contains="ObjectArray",
	slots = list (
		conform="logical")
)

setClass ("Points", contains="GeomObject",
	slots = list (
		np="integer",
		glist="list",
		data="matrix")
)

setClass ("Line", contains="GeomObject",
	slots = list (
		np="integer",
		glist="list",
		data="matrix")
)

setClass ("Polygon", contains="GeomObject",
	slots = list (
		np="integer",
		glist="list",
		data="matrix")
)

setClass ("Rect", contains="Polygon")
setClass ("Cuboid", contains="GeomArray")

setClass ("Text", contains="GeomObject",
	slots = list (
		np="integer",
		glist="list",
		data="matrix",
		text="character")
)

setClass ("Grid", contains="GeomObject",
	slots = list (
		nr="integer",
		nc="integer",
		x="matrix",
		y="matrix",
		gv=".xmatrix",
		glist="list",
		vlist=".xlist")
)

setClass ("VImage", contains="Grid",
	slots = list (
		opaque="logical",
		colm="matrix")
)

setClass ("RImage", contains = c ("VIRTUAL", "GeomObject") )

setClass ("SImage", contains="RImage",
	slots = list (
		nr="integer",
		nc="integer",
		colm="matrix")
)

setClass ("MImage", contains="RImage",
	slots = list (
		nr="integer",
		nc="integer",
		nchannels="integer",
		input="character",
		storage="character",
		colv="array")
)

setMethod ("show", "VecLike", function (object) print (object) )
setMethod ("show", "GeomObject", function (object) print (object) )
setMethod ("show", "RImage", function (object) print (object) )

setMethod ("%*%", c ("MatrixArray", "MatrixArray"), function (x, y) vt3.mmult (x, y) )
setMethod ("%*%", c ("MatrixArray", "matrix"), function (x, y) vt3.mmult (x, y) )
setMethod ("%*%", c ("matrix", "MatrixArray"), function (x, y) vt3.mmult (x, y) )

objtag = function (...) UseMethod ("objtag")
rep2 = function (...) UseMethod ("rep2")
vt3.proj = function (...) UseMethod ("vt3.proj")

ghead = function (...) UseMethod ("ghead")
headt = function (...) UseMethod ("headt")

