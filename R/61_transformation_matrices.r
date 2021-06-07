#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.sin2 = function (theta) round (sin (theta), 15)
.cos2 = function (theta) round (cos (theta), 15)

.p1trans = function (f, p)
{	np = length (p)
	if (np == 0)
		stop ("zero length parameters not supported")
	else if (np == 1)
		f (p)
	else
	{	v = MatrixArray (np, NR=NA, NC=NA)
		for (i in 1:np)
			v [[i]] = f (p [i])
		v
	}
}

.p2trans = function (f, p1, p2)
{	p = cbind (p1, p2)
	np = nrow (p)
	if (np == 0)
		stop ("zero length parameters not supported")
	else if (np == 1)
		f (p [1, 1], p [1, 2])
	else
	{	v = MatrixArray (np, NR=NA, NC=NA)
		for (i in 1:np)
			v [[i]] = f (p [i, 1], p [i, 2])
		v
	}
}

.p3trans = function (f, p1, p2, p3)
{	p = cbind (p1, p2, p3)
	np = nrow (p)
	if (np == 0)
		stop ("zero length parameters not supported")
	else if (np == 1)
		f (p [1, 1], p [1, 2], p [1, 3])
	else
	{	v = MatrixArray (np, NR=NA, NC=NA)
		for (i in 1:np)
			v [[i]] = f (p [i, 1], p [i, 2], p [i, 3])
		v
	}
}

ascl2 = function (x, y=x, ..., about) .a.about (about, .p2trans (.scl2, x, y) )
atrl2 = function (x=0, y=0) .p2trans (.atrl2, x, y)
arot2 = function (theta, ..., about) .a.about (about, .p1trans (.arot2, theta) )

ascl3 = function (x, y=x, z=x) .p3trans (.scl3, x, y, z)
atrl3 = function (x=0, y=0, z=0) .p3trans (.atrl3, x, y, z)
arot3x = function (theta) .p1trans (.arot3x, theta)
arot3y = function (theta) .p1trans (.arot3y, theta)
arot3z = function (theta) .p1trans (.arot3z, theta)

bscl2 = function (x, y=x, ..., about) .b.about (about, .p2trans (.scl2, x, y) )
btrl2 = function (x=0, y=0) .p2trans (.btrl2, x, y)
brot2 = function (theta, ..., about) .b.about (about, .p1trans (.brot2, theta) )

bscl3 = function (x, y=x, z=x) .p3trans (.scl3, x, y, z)
btrl3 = function (x=0, y=0, z=0) .p3trans (.btrl3, x, y, z)
brot3x = function (theta) .p1trans (.brot3x, theta)
brot3y = function (theta) .p1trans (.brot3y, theta)
brot3z = function (theta) .p1trans (.brot3z, theta)

eq.arot2 = function (n, ..., start=0)
	.eq.rot (FALSE, n, start)
eq.brot2 = function (n, ..., start=0)
	.eq.rot (TRUE, n, start)

.a.about = function (about, m)
{	if (missing (about) )
		m
	else
	{	xc = about [1]
		yc = about [2]
		atrl2 (xc, yc) %*% m %*% atrl2 (-xc, -yc)
	}
}

.b.about = function (about, m)
{	if (missing (about) )
		m
	else
	{	xc = about [1]
		yc = about [2]
		btrl2 (-xc, -yc) %*% m %*% btrl2 (xc, yc)
	}
}

.scl2 = function (x, y)
{	matrix (c (
		x, 0, 0,
		0, y, 0,
		0, 0, 1), 3, 3, byrow=TRUE)
}

.scl3 = function (x, y, z)
{	matrix (c (
		x, 0, 0, 0,
		0, y, 0, 0,
		0, 0, z, 0,
		0, 0, 0, 1), 4, 4, byrow=TRUE)
}

.atrl2 = function (x, y)
{	matrix (c (
		1, 0, x,
		0, 1, y,
		0, 0, 1), 3, 3, byrow=TRUE)
}

.arot2 = function (theta)
{	matrix (c (
		.cos2 (theta), - .sin2 (theta), 0,
		.sin2 (theta),   .cos2 (theta), 0,
		0,             0,           1), 3, 3, byrow=TRUE)
}

.atrl3 = function (x, y, z)
{	matrix (c (
		1, 0, 0, x,
		0, 1, 0, y,
		0, 0, 1, z,
		0, 0, 0, 1), 4, 4, byrow=TRUE)
}

.arot3x = function (theta)
{	matrix (c (
		1, 0,             0,               0,
		0, .cos2 (theta), - .sin2 (theta), 0,
		0, .sin2 (theta), .cos2 (theta),   0,
		0, 0,             0,               1), 4, 4, byrow=TRUE)
}

.arot3y = function (theta)
{	matrix (c (
		.cos2 (theta), 0, - .sin2 (theta), 0,
		0,             1, 0,               0,
		.sin2 (theta), 0, .cos2 (theta),   0,
		0,             0, 0,               1), 4, 4, byrow=TRUE)
}

.arot3z = function (theta)
{	matrix (c (
		.cos2 (theta), - .sin2 (theta), 0, 0,
		.sin2 (theta), .cos2 (theta),   0, 0,
		0,             0,               1, 0,
		0,             0,               0, 1), 4, 4, byrow=TRUE)
}

.btrl2 = function (x, y)
{	matrix (c (
		1, 0, 0,
		0, 1, 0,
		x, y, 1), 3, 3, byrow=TRUE)
}

.brot2 = function (theta)
{	matrix (c (
		.cos2 (theta),   .sin2 (theta), 0,
		- .sin2 (theta), .cos2 (theta), 0,
		0,             0,           1), 3, 3, byrow=TRUE)
}

.btrl3 = function (x, y, z)
{	matrix (c (
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		x, y, z, 1), 4, 4, byrow=TRUE)
}

.brot3x = function (theta)
{	matrix (c (
		1, 0,               0,             0,
		0, .cos2 (theta),   .sin2 (theta), 0,
		0, - .sin2 (theta), .cos2 (theta), 0,
		0, 0,               0,             1), 4, 4, byrow=TRUE)
}

.brot3y = function (theta)
{	matrix (c (
		.cos2 (theta),   0, .sin2 (theta), 0,
		0,               1, 0            , 0,
		- .sin2 (theta), 0, .cos2 (theta), 0,
		0,               0, 0            , 1), 4, 4, byrow=TRUE)
}

.brot3z = function (theta)
{	matrix (c (
		.cos2 (theta),   .sin2 (theta), 0, 0,
		- .sin2 (theta), .cos2 (theta), 0, 0,
		0,             0,               1, 0,
		0,             0,               0, 1), 4, 4, byrow=TRUE)
}

.eq.rot = function (post, n, start)
{	pi = seq (start, start + 2 * pi, length.out = n + 1)[1:n]
	if (post) brot2 (pi)
	else arot2 (pi)
}
