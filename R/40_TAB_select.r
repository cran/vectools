#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.one = function (x)
	(length (x) > 0 && is.each.unique (x) )

select = function (...)
{	v = .select.call (...)
	.select (v)
}

selectf = function (...)
{	v = .select.call (..., .partition=TRUE)
	dt = .select (v)
	nr = nrow (dt)
	nc = ncol (dt)

	if (is.null (v$pset) )
		J = 1
	else
		J = which (v$pset == names (dt) )
	A = list ()
	start = 1
	for (i in 1:(nr - 1) )
	{	if (dt [i, J] != dt [i + 1, J])
		{	A = c (A, list (c (start, i) ) )
			start = i + 1
		}
	}
	A = c (A, list (c (start, nr) ) )
	ns = length (A)

	vmap = VMap (ns + 1)
	for (i in 1:ns)
		vmap [[i]] = matrix (c (A [[i]][1], 1, A [[i]][2], nc), 2, 2)
	vmap [[ns + 1]] = matrix (c (1, J, nr, J), 2, 2)

	y = as.SectMatrix (dt, vmap=vmap)
	for (i in 1:ns)
	{	x = cbind (getSect (y, i) )
		x [-1, J] = ""
		`.sSectMatrix.assign` (y, i, value=x)
	}
	y
}

.select = function (v)
{	dt = .from.val (v$fset)
	variables = .variables.val (v$variables, v$a, names (dt) )
	.select.val (v$gset, v$pset, v$sset, v$wset, variables, v$a, v$b)
	v$wset = .where.val (v$gset, v$wset, names (dt), v$a)
	if (! is.null (v$wset) )
	{	I = which (! v$wset$post)
		for (i in I)
			dt = .apply.wheref (dt, v$wset$op [i], v$wset$a [i], v$wset$b [i], v$wset$blit [i])
	}
	if (is.null (v$gset) )
		dt = dt [,variables, drop=FALSE]
	else
	{	factors = dt [, rev (v$gset)]
		dts.x = split (dt, factors)
		n = length (dts.x)
		dts.c = vector ("list", n)
		for (i in 1:n)
			dts.c [[i]] = .apply.aggregationf (v$gset, variables, v$a, v$b, dts.x [[i]])
		dt = dts.c [[1]]
		if (n > 1)
		{	for (i in 2:n)
				dt = rbind (dt, dts.c [[i]])
		}
	}
	if (! is.null (v$wset) )
	{	I = which (v$wset$post)
		for (i in I)
			dt = .apply.wheref (dt, v$wset$op [i], v$wset$a [i], v$wset$b [i], v$wset$blit [i])
	}
	if (! is.null (v$sset) )
	{	for (i in length (v$sset$s):1)
		{	if (v$sset$s [i] == "+")
				dt = dt [order (dt [,v$sset$v [i] ]),, drop=FALSE]
			else
				dt = dt [order (dt [,v$sset$v [i] ], decreasing=TRUE),, drop=FALSE]
			
		}
	}
	nr = nrow (dt)
	rownames (dt) = 1:nr

	dt
}

.select.call = function (..., .partition=FALSE)
{	from = .from
	group.by = .group.by
	partition.by = .partition.by
	sort.by = .sort.by
	where = .where

	u = as.list (sys.call (-1) )[-1]
	if (! is.null (names (u) ) )
		stop ("name = value, not allowed")
	n = length (u)
	variables = a = numeric ()
	b = list ()
	fset = gset = pset = sset = wset  = NULL
	for (i in seq_len (n) )
	{	if (inherits (u [[i]], "name") )
		{	variables = c (variables, as.character (u [[i]]) )
		}
		else if (inherits (u [[i]], "<-") )
		{	a = c (a, as.character (u [[i]][[2]]) )
			b = c (b, u [[i]][[3]])
		}
		else if (inherits (u [[i]], "call") )
		{	k = as.list (u [[i]])
			f = as.character (k [[1]])
			if (f == "from") fset = eval (u [[i]])
			else if (f == "group.by") gset = eval (u [[i]])
			else if (.partition && f == "partition.by") pset = eval (u [[i]])
			else if (f == "sort.by") sset = eval (u [[i]])
			else if (f == "where") wset = eval (u [[i]])
			else
				stop ("only select-compatible constructs allowed")
		}
		else
			stop ("only select-compatible constructs allowed")
	}
	list (fset=fset, gset=gset, pset=pset, sset=sset, wset=wset, variables=variables, a=a, b=b)
}

.from.val = function (fset)
{	if (is.null (fset) )
		stop ("from needs to be specified")
	if (length (fset) != 1)
		stop ("from, needs one dataset")
	eval (parse (text=fset), parent.frame (2) )
}

.variables.val = function (variables, a, ovars)
{	if (length (variables) == 0 && length (a) == 0)
		stop ("needs >= 1 unique variables")
	else if (any (variables == ".") )
	{	if (length (variables) > 1 || length (a) > 0)
			stop (". not supported with other variables")
		variables = ovars
	}
	else
	{	if (! (.one (variables) || .one (a) ) )
			stop ("needs >= 1 unique variables")
		if (! all (variables %in% ovars) )
			stop ("invalid variables")
		if (any (a %in% ovars) )
			stop ("can't redefine variables")
	}
	variables
}

.select.val = function (gset, pset, sset, wset, variables, a, b, ovars)
{	if (is.null (gset) )
	{	if (length (a) > 0)
			stop ("group.by needed, if new variables")
		xvars = variables
	}
	else
	{	if (! .one (gset) )
			stop ("group.by, needs >= 1 unique variables")
		if (! all (gset %in% variables) )
		stop ("group.by, invalid variables")
		xvars = c (gset, a)
	}
	if (! is.null (pset) )
	{	if (length (pset) != 1)
			stop ("partition.by, needs one variable")
		if (! all (pset %in% xvars) )
			stop ("partition.by, invalid variables")
	}
	if (! is.null (sset) )
	{	if (! .one (sset$v) )
			stop ("sort.by, needs >= 1 unique variables")
		if (! all (sset$v %in% xvars) )
			stop ("sort.by, invalid variables")
	}
}

.where.val = function (gset, wset, ovars, a)
{	xvars = c (ovars, a)
	if (is.null (wset) )
		NULL
	else
	{	if (length (wset$op) == 0)
			stop ("where, needs >= 1 variables")
		if (! all (wset$a %in% xvars) )
			stop ("where, invalid variables")
		if (! is.null (gset) && length (a) > 0)
			wset$post [wset$a %in% a] = TRUE
		wset
	}
}

.names = function (fname, ...)
{	u = as.list (sys.call (-1) )
	fname = as.character (u [[1]])
	u = u [-1]
	n = length (u)
	v = character (n)
	for (i in seq_len (n) )
	{	if (inherits (u [[i]], "name") )
			v [i] = as.character (u [[i]])
		else
			stop (paste (fname, ", only names allowed", sep="") )
	}
	v
}

.from = function (...) v = .names (...)
.group.by = function (...) .names (...)
.partition.by = function (...) .names (...)

.sort.by = function (...)
{	u = as.list (sys.call () )[-1]
	n = length (u)
	s = v = character (n)
	for (i in seq_len (n) )
	{	if (inherits (u [[i]], "name") )
		{	s [i] = "+"
			v [i] = as.character (u [[i]])
		}
		else if (inherits (u [[i]], "call") )
		{	k = as.list (u [[i]])
			f = as.character (k [[1]])
			if (f == "+" || f == "-")
				s [i] = f
			else
				stop ("sort.by, only (+/-) names allowed")
			v [i] = as.character (k [[2]])
		}
		else
			stop ("sort.by, only (+/-) names allowed")
	}
	list (s=s, v=v)
}

.where = function (...)
{	u = as.list (sys.call () )[-1]
	n = length (u)
	op = a = b = character (n)
	blit = logical (n)
	for (i in seq_len (n) )
	{	if (inherits (u [[i]], "call") )
		{	k = as.list (u [[i]])
			f = as.character (k [[1]])
			if (f %in% c ("==", "<", ">", "<=", ">=") )
				op [i] = f
			else
				stop ("where, only (in)equalities allowed")
			a [i] = as.character (k [[2]])
			b [i] = as.character (k [[3]])
			blit [i] = is.character (k [[3]])
		}
		else
			stop ("where, only (in)equalities allowed")
	}
	list (post = rep (FALSE, n), op=op, a=a, b=b, blit=blit)
}

.apply.aggregationf = function (gset, variables, a, b, dt)
{	dt.2 = dt [1, variables [variables %in% gset], drop=FALSE]
	for (i in seq_len (length (a) ) )
	{	v = eval (b [[i]], dt)
		v = data.frame (v)
		if (nrow (v) != 1)
			stop ("aggregation functions need to return scalar values")
		colnames (v)[1] = a [i]
		dt.2 = cbind (dt.2, v)
	}
	dt.2
}

.apply.wheref = function (dt, op, a, b, blit)
{	if (blit)
		b = paste0 ('"', b, '"')
	eval (parse (text = paste ("dt [dt$", a, op, b, ",,drop=FALSE]") ) )

}
