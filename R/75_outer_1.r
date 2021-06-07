#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("FGrid",
	slots = list (gv="matrix") )
setClass ("DGrid", contains="FGrid",
	slots = list (gv="matrix", x="integer", y="integer") )
setClass ("CGrid", contains="FGrid",
	slots = list (gv="matrix", x="numeric", y="numeric") )
setClass ("StepGrid", contains="FGrid",
	slots = list (gv="matrix", xb="numeric", yb="numeric") )

DGrid = function (x = 1:nrow (gv), y = 1:ncol (gv), gv)
	new ("DGrid", gv=gv, x=x, y=y)
CGrid = function (x = 1:nrow (gv), y = 1:ncol (gv), gv)
	new ("CGrid", gv=gv, x=x, y=y)
StepGrid = function (xb = 0:nrow (gv) + 0.5, yb = 0:ncol (gv) + 0.5, gv)
	new ("StepGrid", gv=gv, xb=xb, yb=yb)

dgrid = function (sf=`*`, xlim, ylim=xlim, ..., cf, iterate=FALSE, x, y)
{	.arg.error (...)

	if (missing (x) )
	{	xlim = as.integer (xlim)
		x = xlim [1]:xlim [2]
	}
	else
		x = as.integer (x)
	if (missing (y) )
	{	ylim = as.integer (ylim)
		y = ylim [1]:ylim [2]
	}
	else
		y = as.integer (y)
	gv = xgrid.hdim (sf, seqs = list (x, y), cf=cf, iterate=iterate)
	DGrid (x, y, gv)
}

cgrid = function (sf=`*`, xlim, ylim=xlim, ..., n=8, cf, iterate=FALSE, x, y)
{	.arg.error (...)

	n = rep_len (n, 2)
	if (missing (x) )
	{	xlim = as.numeric (xlim)
		x = seq (xlim [1], xlim [2],, n [1])
	}
	else
		x = as.numeric (x)
	if (missing (y) )
	{	ylim = as.numeric (ylim)
		y = seq (ylim [1], ylim [2],, n [2])
	}
	else
		y = as.numeric (y)
	gv = xgrid.hdim (sf, seqs = list (x, y), cf=cf, iterate=iterate)
	CGrid (x, y, gv)
}

stepgrid = function (sf=`*`, xlim, ylim=xlim, ..., n=8, cf, iterate=FALSE, xb, yb)
{	.arg.error (...)

	n = rep_len (n, 2)
	if (missing (xb) )
	{	xlim = as.numeric (xlim)
		xb = seq (xlim [1], xlim [2],, n [1] + 1)
	}
	else
		xb = as.numeric (xb)
	if (missing (yb) )
	{	ylim = as.numeric (ylim)
		yb = seq (ylim [1], ylim [2],, n [2] + 1)
	}
	else
		yb = as.numeric (yb)
	gv = xgrid.hdim (sf, seqs = list (midpoints (xb), midpoints (yb) ), cf=cf, iterate=iterate)
	StepGrid (xb, yb, gv)
}
