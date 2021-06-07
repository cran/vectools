#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.onedim2twodim = function (n)
{	n = as.integer (ceiling (sqrt (n) ) )
	c (n, n)
}

.init.image.board1 = function (n, ...)
{	n = .onedim2twodim (n)
	init.image.board (nr = n [1], nc = n [2], ...)
}

init.image.board = function (nr, nc, ..., rby=TRUE)
{	if (rby)
		p0 = par (mfrow = c (nr, nc), mar = c (0, 0, 0, 0) )
	else
		p0 = par (mfcol = c (nr, nc), mar = c (0, 0, 0, 0) )
	p0
}

.decomp.plot = function (n, imv, rby, shaders)
{	maxch = 3
	for (i in 1:n)
	{	if (is.null (imv [[i]]) )
			stop ("NULL values in ImageArray")
		else if (is.MImage (imv [[i]]) )
		{	if (imv [[i]]@nchannels == 4)
				maxch = 4
		}
		else
			stop ("needs MImage, or ImageArray of such")
	}
	if (rby)
		p0 = init.image.board (n, maxch, rby=TRUE)
	else
		p0 = init.image.board (maxch, n, rby=FALSE)
	for (i in 1:n)
	{	im = imv [[i]]
		if (im@storage == "HSV" || im@storage == "HCL")
			im@colv [,,1][im@colv [,,2] == 0] = NA
		if (missing (shaders) )
		{	if (im@storage == "sRGB")
				sh = vt3.rgb.shaders ()
			else if (im@storage == "HSV")
				sh = vt3.hsv.shaders ()
			else if (im@storage == "HCL")
				sh = vt3.hcl.shaders ()
			else
				stop ("shaders arg required, except for sRGB/HSV/HCL")
		}
		else
			sh = shaders
		for (j in 1:im@nchannels)
		{	im1 = get.channel (im, j)
			plot (im1, colf = sh [[j]])
		}
		if (im@nchannels < maxch)
			plot.new ()
	}
	par (p0)
}

decomp.plot = function (im, ..., rby=TRUE, colfs)
{	if (is.MImage (im) )
	{	imv = ImageArray (1)
		imv [[1]] = im
		.decomp.plot (1, imv, rby, colfs)
	}
	else if (is.ImageArray (im) )
	{	if (im@N == 1)
		{	.decomp.plot (im@n, im, rby, colfs)
		}
		else
			stop ("ImageArray needs to be one dimensional")
	}
	else
		stop ("needs MImage, or ImageArray of such")
}

vt3.rgb.shaders = function ()
{	list (
		vt3.linear.shader ("white", "#700000"),
		vt3.linear.shader ("white", "#005000"),
		vt3.linear.shader ("white", "#000070"),
		vt3.linear.shader ("white", "#000040") )
}

vt3.hsv.shaders = function ()
{	list (
		vt3.hue.shader (),
		vt3.linear.shader ("white", "blue", 0, 1),
		vt3.linear.shader ("white", "blue", 0, 1),
		vt3.linear.shader ("white", "#000020") )
}

vt3.hcl.shaders = function ()
{	list (
		vt3.hue.shader (),
		vt3.linear.shader ("white", "blue", 0, 60),
		vt3.linear.shader ("white", "blue", 0, 90),
		vt3.linear.shader ("white", "#000020") )
}

vt3.linear.shader = function (col0="black", col1="white", min=0, max=1, reverse=FALSE)
{	cols = c (col0, col1)
	if (reverse)
		cols = rev (cols)
	colv = standardize.cols (cols, as.array=TRUE)

	colf = function (x)
		.vt3.linear.shader.ext (min, max, colv, x)
	colf
}


vt3.hue.shader = function (hmin=0, hmax=360, ..., c=50, l=50, na.col="white")
{	rng = range (c (hmin, hmax) )

	colf = function (x)
		.vt3.hue.shader.ext (rng, c, l, na.col, x)
	colf
}

.vt3.linear.shader.ext = function (min, max, colv, x)
{	dims = dim (x)
	x = as.vector (x)
	
	x = (x - min) / (max - min)
	v = (1 - x) %*% colv [1,, drop=FALSE] + x %*% colv [2,, drop=FALSE]
	v [v < 0] = 0
	v [v > 1] = 1
	if (ncol (colv) == 3)
		cols = rgb (v [,1], v [,2], v [,3])
	else
		cols = rgb (v [,1], v [,2], v [,3], v [,4])
	dim (cols) = dims
	cols
}

.vt3.hue.shader.ext = function (rng, c, l, na.col, x)
{	dims = dim (x)
	x = as.vector (x)
	I = is.na (x)

	x [x < rng [1] ] = rng [1]
	x [x > rng [2] ] = rng [2]
	cols = hcl (x, c, l)
	cols [I] = "#FFFFFF"

	dim (cols) = dims
	cols
}
