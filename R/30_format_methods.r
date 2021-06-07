#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

format.ObjectArray = function (x, ...)
{	if (x@N == 0)
		"empty ObjectArray"
	else if (x@N == 1)
	{	y = character (x@n)
		if (length (x@names) == 1)
			names (y) = x@names [[1]]
		for (i in seq_len (x@n) )
			y [i] = objtag (x [[i]], ...)
		y
	}
	else if (x@N == 2)
	{	y = matrix ("", x@n [1], x@n [2])
		if (length (x@names) == 2)
		{	rownames (y) = x@names [[1]]
			colnames (y) = x@names [[2]]
		}
		for (i in seq_len (prod (x@n) ) )
			y [i] = objtag (x@data [[i]], ...)
		y
	}
	else
		"higher-dim ObjectArray"
}

format.SectMatrix = function (x, ..., na.string="")
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
	y = y [,-nc2, drop=FALSE]
	inRow = rep_len (c (FALSE, TRUE), nr2 - 1)
	inCol = rep_len (c (FALSE, TRUE), nc2 - 1)

	if (nr > 1)
	{	for (i in seq (2 * nr - 1, 3, by=-2) )
		{	if (! any (y [i, inCol] == hbar) )
			{	inRow = inRow [-i]
				y = y [-i,]
			}
		}
	}
	if (nc > 1)
	{	for (j in seq (2 * nc - 1, 3, by=-2) )
		{	if (! any (y [inRow ,j] == vsep) )
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
