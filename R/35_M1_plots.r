#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

sqps = matrix (c (
	0, 0,
	1, 0,
	1, 1,
	0, 1), 4, 2, byrow=TRUE, dimnames = list (NULL, c ("x", "y") ) )

cubps = matrix (c (
	0, 0, 0,
	1, 0, 0,
	1, 1, 0,
	0, 1, 0,
	0, 0, 1,
	1, 0, 1,
	1, 1, 1,
	0, 1, 1), 8, 3, byrow=TRUE, dimnames = list (NULL, c ("x", "y", "z") ) )

polyplot = function (ps, ..., main, grid=TRUE, add=FALSE, transpose=FALSE,
	xlim = c (-5, 5), ylim = c (-5, 5), line.color="black", fill.color="lightblue")
{	if (is.matrix (ps) )
		ps = .m2vm (ps)
	else if (! is.MatrixArray (ps) )
		stop ("ps needs to be matrix or MatrixArray object")

	n = length (ps)
	line.color = rep_len (line.color, n)
	fill.color = rep_len (fill.color, n)

	if (! add)
	{	plot.new ()
		plot.window (xlim, ylim)
		if (grid)
		{	abline (v=xlim [1]:xlim [2], h=ylim [1]:ylim [2], col="grey90")
			abline (v=0, h=0)
		}
	}
	
	for (i in 1:length (ps) )
	{	x = ps [[i]]
		if (transpose)
			x = t (x)
		polygon (x [,1], x [,2], border = line.color [i], col = fill.color [i])
	}
	if (! add)
	{	box ()
		axis (1)
		axis (2)
		if (! missing (main) )
			title (main=main)
	}
}

cubplot = function (ps, ..., transpose=FALSE,
	xlim = c (-5, 5), ylim = c (-5, 5), zlim = c (-5, 5), line.color="black")
{	if (is.matrix (ps) )
		ps = .m2vm (ps)
	else if (! is.MatrixArray (ps) )
		stop ("ps needs to be matrix or MatrixArray object")

	n = length (ps)
	Ix1 = c (1, 5, 8, 4)
	Ix2 = c (2, 6, 7, 3)
	Iy1 = c (1, 5, 6, 2)
	Iy2 = c (4, 8, 7, 3)
	Iz1 = 1:4
	Iz2 = 5:8
	
	px1 = py1 = pz1 = px2 = py2 = pz2 = MatrixArray (n)
	for (i in 1:n)
	{	P = ps [[i]]
		if (transpose)
			P = t (P)
		if (nrow (P) != 8 || ncol (P) != 3)
			stop ("unsuitable matrices")
		px1 [[i]] = P [Ix1, -1]
		px2 [[i]] = P [Ix2, -1]
		py1 [[i]] = P [Iy1, -2]
		py2 [[i]] = P [Iy2, -2]
		pz1 [[i]] = P [Iz1, -3]
		pz2 [[i]] = P [Iz2, -3]
	}
	p0 = par (mfrow = c (2, 2) )
	polyplot (pz1, main="(x, y)", xlim=xlim, ylim=ylim, line.color=line.color, fill.color=NA)
	polyplot (pz2, add=TRUE, line.color=line.color, fill.color=NA)
	for (i in 1:n)
	{	for (k in 1:4)
			lines (c (pz1 [[i]][k, 1], pz2 [[i]][k, 1]), c (pz1 [[i]][k, 2], pz2 [[i]][k, 2]), col=line.color)
	}
	plot.new ()
	polyplot (py1, main="input (x, z) -> coord (x, y)", xlim=xlim, ylim=zlim, line.color=line.color, fill.color=NA)
	polyplot (py2, add=TRUE, line.color=line.color, fill.color=NA)
	for (i in 1:n)
	{	for (k in 1:4)
			lines (c (py1 [[i]][k, 1], py2 [[i]][k, 1]), c (py1 [[i]][k, 2], py2 [[i]][k, 2]), col=line.color)
	}
	polyplot (px1, main="input (y, z) -> coord (x, y)", xlim=ylim, ylim=zlim, line.color=line.color, fill.color=NA)
	polyplot (px2, add=TRUE, line.color=line.color, fill.color=NA)
	for (i in 1:n)
	{	for (k in 1:4)
			lines (c (px1 [[i]][k, 1], px2 [[i]][k, 1]), c (px1 [[i]][k, 2], px2 [[i]][k, 2]), col=line.color)
	}
	par (p0)
}
