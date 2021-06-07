#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.cartprod = function (N, n, x)
{	if (N == 1)
		x
	else
	{	x [[1]] = rep (x [[1]], times = prod (n [-1]) )
		if (N > 2)
		{	for (i in 2:(N - 1) )
			{	a = 1:(i - 1)
				b = (i + 1):N
				x [[i]] = rep (x [[i]], each = prod (n [a]), times = prod (n [b]) )
			}
		}
		x [[N]] = rep (x [[N]], each = prod (n [-N]) )
	}
	x
}

.fgrid = function (type, f, lims, x, drop, iterate, combine.function, n=0)
{	if (missing (combine.function) )
		combine = FALSE
	else
		combine = TRUE
	
	if (type == "d" || type == "c")
	{	if (type == "d")
			mode (lims) = "integer"
		else
			mode (lims) = "numeric"
		N = nrow (lims)
		if (N == 0)
			stop ("at least one sequence required")
		if (ncol (lims) != 2)
			stop ("lims needs to be two column matrix")
		
		x = vector ("list", N)
		if (type == "d")
		{	n = rep (1, N)
			for (i in 1:N)
			{	k = lims [i,]
				if (length (k) == 2)
				{	x [[i]] = k [1]:k[2]
					n [i] = length (x [[i]])
				}
			}
		}
		else
		{	if (missing (n) )
				stop ("cgrid requires n")
			n = rep_len (n, N)
			for (i in 1:N)
			{	k = lims [i,]
				if (length (k) == 1)
					n [i] = 1
				else
					x [[i]] = seq (k [1], k [2], length.out = n [i])
			}
		}

	}
	else
	{	N = length (x)
		if (N == 0)
			stop ("at least one sequence required")

		n = numeric (N)
		for (i in 1:N)
		{	n [i] = length (x [[i]])
			if (n [i] == 0)
				stop ("sequences need to be length one or more")
		}
	}
	x = .cartprod (N, n, x)
	np = prod (n)

	if (combine)
	{	if (iterate)
		{	fv = vector ("list", np)
			for (i in 1:np)
			{	str = paste0 ("f (combine.function (", paste0 ("x [[", 1:N, "]][i]", collapse=", "), ") )")
				fv [i] = eval (str2lang (str) )
			}
			fv = unlist (fv)
		}
		else
		{	str = paste0 ("f (combine.function (", paste0 ("x [[", 1:N, "]]", collapse=", "), ") )")
			fv = eval (str2lang (str) )
		}
	}
	else
	{	if (iterate)
		{	fv = vector ("list", np)
			for (i in 1:np)
			{	str = paste0 ("f (", paste0 ("x [[", 1:N, "]][i]", collapse=", "), ")")
				fv [i] = eval (str2lang (str) )
			}
			fv = unlist (fv)
		}
		else
		{	str = paste0 ("f (", paste0 ("x [[", 1:N, "]]", collapse=", "), ")")
			fv = eval (str2lang (str) )
		}
	}
	attributes (fv) = NULL
	if (np != length (fv) )
		stop ("prod (input seq lengths) != length of function return value")
	if (drop)
		n = n [n != 1]
	if (length (n) <= 1)
		fv
	else
		array (fv, n)
}


dgrid.hdim = function (sf=`*`, ..., cf,
	lims = rbind (...),
	drop=TRUE, iterate=FALSE)
	.fgrid ("d", sf, lims, NULL, drop, iterate, cf)

cgrid.hdim = function (sf=`*`, ..., cf,
	lims = rbind (...),
	drop=TRUE, iterate=FALSE, n=10)
	.fgrid ("c", sf, lims, NULL, drop, iterate, cf, n)

xgrid.hdim = function (sf=`*`, ..., cf,
	seqs = list (...),
	drop=TRUE, iterate=FALSE)
	.fgrid ("x", sf, NULL, seqs, drop, iterate, cf)
