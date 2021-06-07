#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.points.vardim = function (x, y, about.axis)
{	d3 = rep (0, length (x) )
	if (missing (about.axis) )
		list (x=x, y=y, z=NULL, N=2L)
	else if (about.axis == "x")
		list (x=d3, y=x, z=y, N=3L)
	else if (about.axis == "y")
		list (x=x, y=d3, z=y, N=3L)
	else if (about.axis == "z")
		list (x=x, y=y, z=d3, N=3L)
	else
		stop ("about.axis needs to be x, y or z")
}

regPolygon = function (n=4, ..., about.axis, d=1, stagger = (n %% 2 == 0) )
{	.arg.error (...)

	start = pi / 2
	if (stagger)
		start = start + pi / n
	ps = c (d, 0) %|*% brot2 (start)
	ps = ps %|*% eq.brot2 (n)

	ps = .points.vardim (ps [,1], ps [,2], about.axis)
	Polygon (ps$x, ps$y, ps$z, ...)
}

Rect = function (..., about.axis, center=FALSE, side.length=1, glist = list () )
{	.arg.error (...)

	center = rep_len (center, 2)
	side.length = rep_len (side.length, 2)

	x = c (0, side.length [1])
	y = c (0, side.length [2])

	if (center [1])
		x = x - mean (x)
	if (center [2])
		y = y - mean (y)

	x = x [c (1, 1, 2, 2)]
	y = y [c (1, 2, 2, 1)]

	ps = .points.vardim (x, y, about.axis)
	new ("Rect", D=ps$N, np=4L, glist=glist, data = cbind (ps$x, ps$y, ps$z) )
}

Cuboid = function (..., center=FALSE, side.length=1, glist = list () )
{	.arg.error (...)

	center = rep_len (center, 3)
	side.length = rep_len (side.length, 3)

	v = ObjectArray (6)
	gv = new ("Cuboid", CLASS="Rect", D=3L, N=1L, n=6L, names = list (), data=v@data)

	rx = Rect (about.axis="x", center = center [2:3], side.length = side.length [2:3], glist=glist)
	ry = Rect (about.axis="y", center = center [-2], side.length = side.length [-2], glist=glist)
	rz = Rect (about.axis="z", center = center [1:2], side.length = side.length [1:2], glist=glist)

	if (center [1])
		rx = rx %]*% btrl3 (- side.length [1] / 2, 0, 0)
	if (center [2])
		ry = ry %]*% btrl3 (0, - side.length [2] / 2, 0)
	if (center [3])
		rz = rz %]*% btrl3 (0, 0, - side.length [3] / 2)

	gv [[1]] = rx
	gv [[2]] = rx %]*% btrl3 (side.length [1], 0, 0)
	gv [[3]] = ry
	gv [[4]] = ry %]*% btrl3 (0, side.length [2], 0)
	gv [[5]] = rz
	gv [[6]] = rz %]*% btrl3 (0, 0, side.length [3])

	gv
}

rectGrid = function (x, y, gv=NULL, ..., glist = list (), vlist=NULL)
{	.arg.error (...)

	nr = length (x)
	nc = length (y)
	umat = vmat = matrix (0, nr, nc)
	for (j in 1:nc)
		umat [,j] = x
	for (i in 1:nr)
		vmat [i,] = y
	if (! is.null (gv) )
	{	if (length (gv) == 1)
			gv = matrix (gv, nr, nc)
		else if (nr == nrow (gv) && nc == ncol (gv) )
			NULL
		else
			stop ("gv doesn't match x and y")
	}
	Grid (umat, vmat, gv, glist=glist, vlist=vlist)
}

rectVImage = function (x, y, gv=NULL, ..., tf=FALSE, colm, glist = list () )
{	.arg.error (...)
	grid = rectGrid (x, y, gv)
	VImage (grid@x, grid@y, grid@gv, tf=tf, colm=colm, glist=glist)
}
