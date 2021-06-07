#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.proj = function (x, proj)
{	if (proj == "yz")
		x [,2:3]
	else if (proj == "xz")
		x [,-2]
	else if (proj == "xy")
		x [,1:2]
	else
		stop ("proj needs to be xy, xz or yz")
}

.center.point.dist = function (x, proj)
{	if (proj == "yz")
		mean (x [,1])
	else if (proj == "xz")
		mean (x [,2])
	else if (proj == "xy")
		mean (x [,3])
	else
		stop ("proj needs to be xy, xz or yz")
}

.sort.GeomArray = function (gv, proj)
{	dists = numeric (length (gv) )
	for (k in 1:length (gv) )
	{	g = gv [[k]]
		if (is.Points (g) || is.Line (g) || is.Polygon (g) )
			dists [k] = .center.point.dist (g@data, proj)
		else if (is.GeomArray (g) )
		{	objc = .sort.GeomArray (g, proj)
			dists [k] = objc [[1]]
			gv [[k]] = objc [[2]]
		}
		else if (is.Grid (g) )
			dists [k] = .center.point.dist (.grid2matrix (g), proj)
	}
	gv = gv [order (dists)]
	dists = mean (dists)
	list (dists, gv)
}

vt3.proj.GeomArray = function (v, proj="xy", sort=TRUE, ...)
{	if (sort)
		v = .sort.GeomArray (v, proj)[[2]]
	v@D = 2L
	for (k in 1:length (v) )
		v [[k]] = vt3.proj (v [[k]], proj, sort=sort)
	v
}

vt3.proj.Point = function (v, proj="xy", ...)
{	v@data = .proj (v@data, proj)
	v@D = 2L
	v
}

vt3.proj.Line = function (v, proj="xy", ...) vt3.proj.Point (v, proj)
vt3.proj.Polygon = function (v, proj="xy", ...) vt3.proj.Point (v, proj)
vt3.proj.Text = function (v, proj="xy", ...) vt3.proj.Point (v, proj)

vt3.proj.Grid = function (v, proj="xy", ...)
{	m = .proj (.grid2matrix (v), proj)
	v = .matrix2grid (v, m)
	v@D = 2L
	v@gv = NULL
	v
}
