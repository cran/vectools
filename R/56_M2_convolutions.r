#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

vt2.convolve.matrix = function (x, k, ..., f, nsub)
{	xdims = dim (x)

	if (missing (k) )
	{	std = FALSE
		if (missing (f) || missing (nsub) )
			stop ("if k missing, f and nsub required")
		nsub = rep_len (nsub, 2)
		if (any (nsub > xdims) )
			stop ("nsub values need to be <= dim (x)")
	}
	else
	{	std = TRUE
		nsub = dim (k)
		if (any (nsub > xdims) )
			stop ("dim (k) values need to be <= dim (x)")
	}

	ydims = xdims - nsub + 1
	It = 0:(nsub [1] - 1)
	Jt = 0:(nsub [2] - 1)

	y = matrix (0, ydims [1], ydims [2])
	for (i in 1:ydims [1])
	{	for (j in 1:ydims [2])
		{	xsub = x [i + It, j + Jt]
			if (std)
				y [i, j] = sum (k * xsub)
			else
			{	u = as.vector (f (xsub) )
				if (length (u) != 1)
					stop ("f return value, needs to be scalar")
				y [i, j] = u
			}
		}
	}
	y
}

vt2.simple.matrix = function (n, ..., scale=TRUE)
{	n = as.integer (n)
	n = rep_len (n, 2)
	y = matrix (0, n [1], n [2])
	y [ceiling (n [1] / 2),] = 1
	y [,ceiling (n [2] / 2) ] = 1
	if (scale)
		y = y / sum (y)
	y
}
