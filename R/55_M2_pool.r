#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

vt2.pool.matrix = function (x, nsub, ..., f=mean, edge.method="exact")
{	nsub = as.integer (nsub)
	nsub = rep_len (nsub, 2)
	xdims = dim (x)

	ydims = xdims %/% nsub
	os = xdims %% nsub

	if (any (os > 0) )
	{	if (edge.method == "exact")
			stop ("x dims need to multiple of nsub, for exact method")
		else if (edge.method != "spart")
			stop ("currently, edge.method needs to be exact or spart")
	}

	v = .pool.inds (os [1], nsub [1], xdims [1], ydims [1])
	ydims [1] = v [[1]]
	Ia = v [[2]]
	Ib = v [[3]]

	v = .pool.inds (os [2], nsub [2], xdims [2], ydims [2])
	ydims [2] = v [[1]]
	Ja = v [[2]]
	Jb = v [[3]]

	y = matrix (0, ydims [1], ydims [2])
	for (i in 1:ydims [1])
	{	for (j in 1:ydims [2])
		{	xsub = x [Ia [i]:Ib [i], Ja [j]:Jb [j], drop=FALSE]
			u = as.vector (f (xsub) )
			if (length (u) != 1)
				stop ("f return value, needs to be scalar")
			y [i, j] = u
		}
	}
	y
}

vt2.pool2.matrix = function (x, ntar, ..., f=mean)
{	ntar = as.integer (ntar)
	ntar = rep_len (ntar, 2)
	xdims = dim (x)

	nsub = xdims %/% ntar
	if (any (ntar > xdims / 2) )
		stop ("ntar needs to be <= x dims / 2")

	vt2.pool.matrix (x, nsub, f=f, edge.method="spart")
}

.pool.inds = function (os, ns, nx, ny)
{	a = 0:(ny - 1) * ns + 1
	b = 1:ny * ns
	k = ny * ns + 1
	if (os > 0)
	{	if (os == 1)
		{	ny = ny + 1
			os.h = os %/% 2
			a = c (a, k)
			b = c (b, nx)
		}
		else
		{	ny = ny + 2
			os.h = os %/% 2
			a = c (1, a + os.h, k + os.h)
			b = c (os.h, b + os.h, nx)
		}
	}
	list (ny, a, b)
}
