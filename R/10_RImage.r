#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.as.SImage = function (x)
{	if (is.matrix (x) )
	{	dims = dim (x)
		if (any (dims == 0) )
			stop ("zero-row and zero-col matrices not allowed")
	
		if (is.logical (x) )
			mode (x) = "numeric"
		else if (! is.numeric (x) )
			stop ("only logical/numeric matrices/arrays allowed")
	
		list (colm=x, nr = dims [1], nc = dims [2])
	}
	else if (is.list (x) )
		.as.SImage (x)
	else
		stop ("only matrices and lists allowed")
}

.as.MImage = function (x)
{	if (is.raster (x) )
		.as.MImage (standardize.cols (x, as.array=TRUE) )
	else if (is.matrix (x) )
	{	if (is.character (x) )
			.as.MImage (standardize.cols (x, as.array=TRUE) )
		else
			.as.MImage (array (x, c (dim (x), 1) ) )
	}
	else if (is.array (x) )
	{	dims = dim (x)
		if (length (dims) != 3)
			stop ("only 2-dim (matrix) and 3-dim arrays allowed")
		nchannels = dims [3]
		if (nchannels > 4)
			stop ("n channels (3rd array dim) > 4")
		if (any (dims == 0) )
			stop ("zero-dim arrays not allowed")

		list (colv=x, nr = dims [1], nc = dims [2], nchannels=nchannels)
	}
	else if (is.list (x) )
		.as.MImage (.list2array (x) )
	else
		stop ("only matrices, arrays and lists allowed")
}

as.SImage = function (im)
{	if (is.SImage (im) )
		im
	else
	{	im = .as.SImage (im)
		with (im, new ("SImage", nr=nr, nc=nc, colm=colm) )
	}
}

as.MImage = function (im, ..., input="sRGB", storage=input)
{	.arg.error (...)
	x = im
	if (is.SImage (x) )
	{	colv = array (x@colm, c (x@nr, x@nc, 1L) )

		new ("MImage", nr=x@nr, nc=x@nc, nchannels=1L, input=NA_character_, storage=NA_character_,
			min=min, max=max, colv=colv)
	}
	else if (is.MImage (x) )
	{	input = x@input
		if (x@nchannels >= 3 && input != storage)
			x@colv = mapcol (x@colv, from=x@input, to=storage)
		x
	}
	else
	{	x = .as.MImage (x)
		n = x$nchannels
		if (n <= 2)
			input = storage = NA_character_
		else
		{	if (input != storage)
				x$colv = mapcol (x$colv, from=input, to=storage)
		}

		with (x, new ("MImage", nr=nr, nc=nc, nchannels=nchannels, input=input, storage=storage, colv=colv) )
	}
}

get.channel = function (im, which, ..., as.matrix=FALSE)
{	if (is.MImage (im) )
	{	colm = im@colv [,,which]
		if (as.matrix)
			colm
		else
			as.SImage (colm)
	}
	else
		stop ("not MImage")
}

.as.raster.SImage = function (x, ..., pack=FALSE, colf, invert)
{	if (missing (colf) )
	{	rng = c (0, 1)
		cols = c ("white", "black")
		if (pack)
			rng = range (x, na.rm=TRUE)
		if (invert)
			cols = rev (cols)
		colf = vt3.linear.shader (cols [1], cols [2], rng [1], rng [2])
	}
	colf (x)
}

as.raster.SImage = function (x, ..., optfit=FALSE, colf = vt3.linear.shader (reverse=invert), invert=TRUE)
{	.arg.error (...)
	.as.raster.SImage (x@colm, pack=optfit, colf=colf, invert=invert)
}

as.raster.MImage = function (x, ..., which, invert.gs=TRUE)
{	.arg.error (...)
	if (! missing (which) )
		.as.raster.SImage (x@colv [,,which], ..., invert=invert.gs)
	else if (x@nchannels == 1)
		.as.raster.SImage (x@colv [,,1], ..., invert=invert.gs)
	else if (x@nchannels == 2)
	{	u = x@colv [,,1]
		if (invert.gs)
			u = .val.01 (1 - u)
		cols = rgb (u, u, u, x@colv [,,2])
		dim (cols) = c (x@nr, x@nc)
		cols
	}
	else
	{	u = mapcol (x@colv, from=x@storage, to="sRGB")
		if (x@nchannels == 3)
			cols = rgb (u [,,1], u [,,2], u [,,3])
		else
			cols = rgb (u [,,1], u [,,2], u [,,3], u [,,4])
		dim (cols) = c (x@nr, x@nc)
		cols
	}
}

.val.01 = function (x)
{	x [x < 0] = 0
	x [x > 1] = 1
	x
}
