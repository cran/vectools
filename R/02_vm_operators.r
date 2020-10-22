#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.m2vm = function (x)
{	vm = MatrixArray (1)
	vm [[1]] = x
	vm
}

.m2vm.by.row = function (x)
{	n = nrow (x)
	vm = MatrixArray (n)
	for (i in 1:n)
		vm [[i]] = x [i,, drop=FALSE]
	vm
}

.m2vm.by.col = function (x)
{	n = ncol (x)
	vm = MatrixArray (n)
	for (j in 1:n)
		vm [[j]] = x [,j, drop=FALSE]
	vm
}

.vm2m.by.row = function (vm)
{	n = length (vm)
	x = matrix (0, n, ncol (vm [[1]]) )
	for (i in 1:n)
		x [i,] = vm [[i]]
	x
}

.vm2m.by.col = function (vm)
{	n = length (vm)
	x = matrix (0, nrow (vm [[1]]), n)
	for (j in 1:n)
		x [,j] = vm [[j]]
	x
}

.vmbind = function (a, b)
{	na = length (a)
	nb = length (b)
	if (na == nb)
		list (a, b, na)
	if (na < nb)
	{	if (nb %% na != 0)
			stop ("length of longer object not multiple of shorter object")
		list (rep2 (a, nb), b, nb)
	}
	else
	{	if (na %% nb != 0)
			stop ("length of longer object not multiple of shorter object")
		list (a, rep2 (b, na), na)
	}
}

.vmbind2 = function (a, b)
{	na = length (a)
	nb = length (b)
	list (rep (a, times=nb), rep (b, each=na), na * nb)
}

.vmmult = function (ab)
{	n = ab [[3]]
	vm = MatrixArray (n)
	for (i in 1:n)
		vm [[i]] = ab [[1]][[i]] %*% ab [[2]][[i]]
	vm
}

.vmsimplify = function (vm)
{	if (length (vm) == 1) vm [[1]]
	else vm
}

.vmpad = function (vm, in.row=FALSE)
{	if (in.row)
	{	for (i in 1:length (vm) )
			vm [[i]] = rbind (vm [[i]], 1)
	}
	else
	{	for (i in 1:length (vm) )
			vm [[i]] = cbind (vm [[i]], 1)
	}
	vm
}

.vmstrip = function (vm, in.row=FALSE)
{	if (in.row)
	{	for (i in 1:length (vm) )
		{	mat = vm [[i]]
			m = nrow (mat) - 1
			vm [[i]] = mat [1:m,, drop=FALSE]
		}
	}
	else
	{	for (i in 1:length (vm) )
		{	mat = vm [[i]]
			m = ncol (mat) - 1
			vm [[i]] = mat [,1:m, drop=FALSE]
		}
	}
	vm
}

.vmmult2 = function (a, b)
{	if (is.matrix (a) ) a = .m2vm (a)
	if (is.matrix (b) ) b = .m2vm (b)
	if (! (is.MatrixArray (a) && is.MatrixArray (b) ) )
		stop ("operands need to be matrix or MatrixArray objects")
	p = .vmmult (.vmbind (a, b) )
	.vmsimplify (p)
}

"%{*}%" = function (a, b)
{	if (is.matrix (a) ) a = .m2vm (a)
	if (is.matrix (b) ) b = .m2vm (b)
	if (! (is.MatrixArray (a) && is.MatrixArray (b) ) )
		stop ("operands need to be matrix or MatrixArray objects")
	p = .vmmult (.vmbind2 (a, b) )
	.vmsimplify (p)
}

"%*[%" = function (a, b) .vmmult.padright (a, b, FALSE)
"%]*%" = function (a, b) .vmmult.padleft (a, b, FALSE)
"%*{%" = function (a, b) .vmmult.padright (a, b, TRUE)
"%}*%" = function (a, b) .vmmult.padleft (a, b, TRUE)

.vmmult.padright = function (a, b, by.group)
{	if (is.matrix (a) ) a = .m2vm (a)
	if (is.matrix (b) ) b = .m2vm (b)
	if (! (is.MatrixArray (a) && is.MatrixArray (b) ) )
		stop ("operands need to be matrix or MatrixArray objects")
	b = .vmpad (b, TRUE)
	if (by.group)
		p = .vmmult (.vmbind2 (a, b) )
	else
		p = .vmmult (.vmbind (a, b) )
	p = .vmstrip (p, TRUE)
	.vmsimplify (p)
}

.vmmult.padleft = function (a, b, by.group)
{	if (is.matrix (a) ) a = .m2vm (a)
	if (is.matrix (b) ) b = .m2vm (b)
	if (! (is.MatrixArray (a) && is.MatrixArray (b) ) )
		stop ("operands need to be matrix or MatrixArray objects")
	a = .vmpad (a)
	if (by.group)
		p = .vmmult (.vmbind2 (a, b) )
	else
		p = .vmmult (.vmbind (a, b) )
	p = .vmstrip (p)
	.vmsimplify (p)
}

"%*|%" = function (a, b)
{	if (is.matrix (b) )
	{	m = nrow (b)
		b = rbind (b, 1)
	}
	else if (is.vector (b) )
	{	m = length (b)
		b = cbind (c (b, 1) )
	}
	else
		stop ("right operand needs to be matrix, or plain vector")
	if (is.matrix (a) )
		r = a %*% b
	else if (is.MatrixArray (a) )
		r = .vm2m.by.col (.vmmult (.vmbind (a, .m2vm.by.col (b) ) ) )
	else
		stop ("left operand needs to be matrix or MatrixArray")
	r [1:m,]
}

"%|*%" = function (a, b)
{	if (is.matrix (a) )
	{	m = ncol (a)
		a = cbind (a, 1)
	}
	else if (is.vector (a) )
	{	m = length (a)
		a = rbind (c (a, 1) )
	}
	else
		stop ("left operand needs to be matrix, or plain vector")
	if (is.matrix (b) )
		r = a %*% b
	else if (is.MatrixArray (b) )
		r = .vm2m.by.row (.vmmult (.vmbind (.m2vm.by.row (a), b) ) )
	else
		stop ("right operand needs to be matrix or MatrixArray")
	r [,1:m]
}
