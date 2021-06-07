#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.xyz = function (x, y, z)
{	if (missing (x) || is.null (x) || missing (y) || is.null (y) )
		stop ("x and y required")
	xyz = cbind (x, y, z)
	colnames (xyz) = NULL
	N = ncol (xyz)
	if (N == 2 || N == 3)
	{	n = nrow (xyz)
		list (D=N, n=n, data=xyz)
	}
	else
		stop ("needs to 2d or 3d input")
}

.xyz.grid = function (x, y, z)
{	if (! is.matrix (x) || ! is.matrix (y) )
		stop ("x, y and z need to be matrices")
	nr = nrow (x)
	nc = ncol (x)
	if (nr != nrow (y) || nc != ncol (y) )
		stop ("x, y and z need same dims")
	if (is.null (z) )
		N = 2L
	else
	{	N = 3L
		if (! is.matrix (z) )
			stop ("x, y and z need to be matrices")
		if (nr != nrow (z) || nc != ncol (z) )
			stop ("x, y and z need same dims")
	}
	list (D=N, nr=nr, nc=nc, x=x, y=y, z=z)
}

.as.PointLike = function (.f, object, ...)
{	if (! is.matrix (object) )
		stop ("matrix required")
	x = object [,1]
	y = object [,2]
	if (ncol (object) == 3)
		z = object [,3]
	else
		z = NULL
	.f (x, y, z, ...)
}

Points = function (x, y, z=NULL, ..., glist = list () )
{	.arg.error (...)

	xyz = .xyz (x, y, z)
	with (xyz, new ("Points", D=D, np=n, glist=glist, data=data) )
}

Line = function (x, y, z=NULL, ..., glist = list () )
{	.arg.error (...)

	xyz = .xyz (x, y, z)
	if (xyz$n < 2)
		stop ("needs >= 2 points")
	with (xyz, new ("Line", D=D, np=n, glist=glist, data=data) )
}

Polygon = function (x, y, z=NULL, ..., glist = list () )
{	.arg.error (...)

	xyz = .xyz (x, y, z)
	if (xyz$n < 3)
		stop ("needs >= 3 points")
	with (xyz, new ("Polygon", D=D, np=n, glist=glist, data=data) )
}

Text = function (text, x, y, z=NULL, ..., glist = list () )
{	.arg.error (...)

	xyz = .xyz (x, y, z)
	text = rep_len (text, xyz$n)
	with (xyz, new ("Text", D=D, np=n, glist=glist, data=data, text=text) )
}

Grid = function (x, y, gv=NULL, ..., glist = list (), vlist=NULL)
{	.arg.error (...)

	xyz = .xyz.grid (x, y, gv)
	if (! is.null (vlist) )
	{	if (is.null (names (vlist) ) )
			stop ("vlist needs to be a named list of matrices")
		for (mobject in vlist)
		{	if (! is.matrix (mobject) )
				stop ("vlist needs to be a named list of matrices")
			if (xyz$nr - 1 != nrow (mobject) || xyz$nc - 1 != ncol (mobject) )
				stop ("grid dims need to be one more than vlist matrices")
		}
	}
	with (xyz, new ("Grid", D=D, nr=nr, nc=nc, x=x, y=y, gv=gv, glist=glist, vlist=vlist) )
}

VImage = function (x, y, gv=NULL, ..., tf=FALSE, colm, glist = list () )
{	.arg.error (...)

	if (missing (colm) )
		stop ("colm required")
	colm = as.matrix (colm)
	v = vt3.prep.cols (colm)
	opaque = v [[1]]
	colm = v [[2]]
	if (tf)
	{	colm = t (colm)
		colm = colm [,ncol (colm):1]
	}
	xyz = .xyz.grid (x, y, gv)
	with (xyz, new ("VImage", D=D, nr=nr, nc=nc, x=x, y=y, gv=gv, opaque=opaque, colm=colm, glist=glist) )
}

as.Points = function (v, ...)
	.as.PointLike (Points, v, ...)

as.Line = function (v, ...)
	.as.PointLike (Line, v, ...)

as.Polygon = function (v, ...)
	.as.PointLike (Polygon, v, ...)

as.Grid = function (v, ..., glist = list (), vlist=NULL)
{	.arg.error (...)

	if (is (v, "CGrid") )
		rectGrid (v@x, v@y, v@gv, glist=glist, vlist=vlist)
	else
		stop ("CGrid required")
}
