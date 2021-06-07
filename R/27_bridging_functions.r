#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.base.graphics.init = function (xlim, ylim, axs="r")
{	plot.new ()
	plot.window (xlim=xlim, ylim=ylim, xaxs=axs, yaxs=axs)
}

.grid.graphics.init = function (xlim, ylim)
{	vp = grid::viewport (xscale=xlim, yscale=ylim, width=0.9, height=0.9)

	grid::grid.newpage ()
	grid::pushViewport (vp)
}

.base.bf.points = function (x, y, ..., glist)
	do.call (points, c (list (x=x, y=y), glist) )

.base.bf.line = function (x, y, ..., glist)
	do.call (lines, c (list (x=x, y=y), glist) )

.base.bf.polygon = function (x, y, ..., glist)
	do.call (polygon, c (list (x=x, y=y), glist) )

.base.bf.text = function (x, y, textstr, ..., glist)
	do.call (text, c (list (x=x, y=y, labels=textstr), glist) )

.base.bf.raster = function (x, y, colm, vflip, interpolate)
{	if (vflip)
		y = rev (y)
	rasterImage (colm, x [1], y [1], x [2], y [2], interpolate=interpolate)
}

.base.bf.cpanel = function (x, y, opaque, pcol)
{	bcol = NA
	if (opaque)
		bcol = pcol
	polygon (x, y, border=bcol, col=pcol)
}

.grid.bf.points = function (x, y, ..., glist)
	do.call (grid::grid.points, c (list (x=x, y=y, default.units="native"), glist) )

.grid.bf.line = function (x, y, ..., glist)
	do.call (grid::grid.lines, c (list (x=x, y=y, default.units="native"), glist) )

.grid.bf.polygon = function (x, y, ..., glist)
	do.call (grid::grid.polygon, c (list (x=x, y=y, default.units="native"), glist) )

.grid.bf.text = function (x, y, textstr, ..., .std.glist.names, glist)
	do.call (grid::grid.text, c (list (label=textstr, x=x, y=y, default.units="native"), glist) )

.grid.bf.raster = function (x, y, colm, vflip, interpolate)
{	if (vflip)
		y = rev (y)
	grid::grid.raster (colm, mean (x), mean (y), x [2] - x [1], y [2] - y [1],
		default.units="native", interpolate=interpolate)
}

.grid.bf.cpanel = function (x, y, opaque, pcol)
{	bcol = NA
	if (opaque)
		bcol = pcol
	grid::grid.polygon (x, y, gp = grid::gpar (col=bcol, fill=pcol), default.units="native")
}
