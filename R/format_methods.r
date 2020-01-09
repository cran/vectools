#vectools: Supplementary Vector-Related Tools
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

format.ObjectArray = function (x, na.string="", ...)
{	if (x@nbd == 0)
		"empty ObjectArray"
	else if (x@nbd == 1)
	{	y = character (x@dim)
		if (! is.na (x@names [[1]][1]) )
			names (y) = x@names [[1]]
		for (i in seq_len (x@dim) )
		{	if (.is.na.2 (x [[i]]) )
				y [1] = na.string
			else
				y [i] = objtag (x [[i]], ...)
		}
	}
	else if (x@nbd == 2)
	{	y = matrix ("", x@dim [1], x@dim [2])
		if (! is.na (x@names [[1]][1]) )
			rownames (y) = x@names [[1]]
		if (! is.na (x@names [[2]][1]) )
			colnames (y) = x@names [[2]]
		for (i in seq_len (prod (x@dim) ) )
		{	if (.is.na.2 (x@data [[i]]) )
				y [i] = na.string
			else
				y [i] = objtag (x@data [[i]], ...)
		}
	}
	else
		stop ("can't format higher-dim ObjectArray")
	y
}

format.NestMatrix = function (x, na.string="", ...)
	format (x@data, na.string, ...)

format.SectMatrix = function (x, na.string="", ...)
	.format.SectMatrix (x, na.string, ...)

.format.SectMatrix = function (x, na.string, ..., hsep="-", vsep="|", xsym="+")
{	nm = x

	x = .data (nm)
	I = is.na (x)
	x = format (x, ...)
	x [I] = na.string
	nr = nm@nr
	nc = nm@nc

	nr2 = 1 + 2 * nr
	nc2 = 1 + 2 * nc
	rnames = rep ("", nr2)
	cnames = rep ("", nc2)
	rnames [seq (2, 2 * nr, by=2)] = rownames (x)
	cnames [seq (2, 2 * nc, by=2)] = colnames (x)
	nchar = max (nchar (x) )
	if (nchar == 0)
		nchar = 1
	mbar = paste (rep ("-", nchar), collapse="")
	hbar = paste (rep (hsep, nchar), collapse="")

	y = matrix ("", nr2, nc2)
	rownames (y) = rnames
	colnames (y) = cnames
	for (i in 1:nr)
	{	for (j in 1:nc)
			y [2 * i, 2 * j] = x [i, j]
	}

	warn = FALSE
	for (u in nm@vmap@data) 
	{	if (is.na (u [1]) )
			warn = TRUE
		else
		{	y [c (2 * u [1] - 1, 2 * u [3] + 1), c (2 * u [2] - 1, 2 * u [4] + 1)] = xsym
			y [c (2 * u [1] - 1, 2 * u [3] + 1), (2 * u [2]):(2 * u [4])] = hbar
			y [(2 * u [1]):(2 * u [3]), c (2 * u [2] - 1, 2 * u [4] + 1)] = vsep
		}
	}
	if (nm@header)
	{	u = (y [1,] != "+")
		y [1, (1:nc2)[u] ] = mbar
	}
	y = y [-nr2,, drop=FALSE]
	if (nr > 1)
	{	for (i in seq (2 * nr - 1, 3, by=-2) )
		{	if (! any (y [i,] == hbar) )
				y = y [-i,]
		}
	}
	y = y [,-nc2, drop=FALSE]
	if (nc > 1)
	{	for (j in seq (2 * nc - 1, 3, by=-2) )
		{	if (! any (y [,j] == vsep) )
				y = y [,-j]
		}
	}
	if (! nm@header)
		y = y [-1,, drop=FALSE]
	y = y [,-1, drop=FALSE]
	if (warn)
		warning ("unset section maps")
	y
}

.is.na.2 = function (x)
{	if (is.null (x) )
		TRUE
	else
	{	attributes (x) = NULL
		if (is.vector (x) && length (x) == 1 && is.na (x) )
			TRUE
		else
			FALSE
	}
}
