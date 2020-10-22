#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

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

.fgrid = function (type, f, combine, drop, iterate, combine.function, n, ...)
{	x = list (...)
	N = length (x)
	if (N == 0)
		stop ("at least one lim/seq required")

	if (type == "d" || type == "c")
	{	if (type == "d")
		{	for (i in 1:N)
				x [[i]] = as.integer (x [[i]])
		}
		else
		{	for (i in 1:N)
				x [[i]] = as.numeric (x [[i]])
		}
	
		for (i in 1:N)
		{	nk = length (x [[i]])
			if (nk == 0 || nk > 2)
				stop ("constants and limits need to be length one or two, respectively")
		}

		if (type == "d")
		{	n = rep (1, N)
			for (i in 1:N)
			{	k = x [[i]]
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
			{	k = x [[i]]
				if (length (k) == 1)
					n [i] = 1
				else
					x [[i]] = seq (k [1], k [2], length.out = n [i])
			}
		}

	}
	else
	{	n = numeric (N)
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

fgrid = function (f=`*`, ..., combine=FALSE,  drop=TRUE, iterate=FALSE, combine.function=cbind)
	.fgrid (0, f, combine, drop, iterate, combine.function, 0, ...)
dgrid = function (f=`*`, ..., combine=FALSE,  drop=TRUE, iterate=FALSE, combine.function=cbind)
	.fgrid ("d", f, combine, drop, iterate, combine.function, 0, ...)
cgrid = function (f=`*`, ..., combine=FALSE,  drop=TRUE, iterate=FALSE, combine.function=cbind, n)
	.fgrid ("c", f, combine, drop, iterate, combine.function, n, ...)
