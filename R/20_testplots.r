#vectools: Advanced Vector Toolkit
#Copyright (C), Abby Spurdle, 2020 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

vt3.testplot = function (v=SQUARE, ...,
	main="", xlab="x", ylab="y",
	xlim = c (-4.5, 4.5), ylim=xlim,
	grid.lines=TRUE, cross.hairs=grid.lines,
	proj="xy", sort=TRUE)
{	.init.graphics ("graphics", v, FALSE, xlim, ylim, ..., axs="i")

	if (grid.lines)
	{	vpos = ceiling (xlim [1]):floor (xlim [2])
		hpos = ceiling (ylim [1]):floor (ylim [2])
		abline (h=hpos, v=vpos, col="grey85")
	}
	if (cross.hairs)
		abline (h=0, v=0)

	plot (v, add=TRUE, proj=proj, sort=sort)

	box ()
	axis (1)
	axis (2)
	title (main,, xlab, ylab)
}

vt3.testplot3 = function (v=CUBE, ...,
	main = c ("proj=xy", "proj=xz", "proj=yz"),
	xlab="x", ylab="y", zlab="z",
	xlim = c (-4.5, 4.5), ylim=xlim, zlim=xlim,
	grid.lines=TRUE, cross.hairs=grid.lines,
	sort=TRUE)
{	p0 = par (mfrow = c (2, 2) )
	vt3.testplot (v, main = main [1], xlab=xlab, ylab=ylab, proj="xy",
		xlim=xlim, ylim=ylim, sort=sort, grid.lines=grid.lines, cross.hairs=cross.hairs)
	plot.new ()
	vt3.testplot (v, main = main [2], xlab=xlab, ylab=zlab, proj="xz",
		xlim=xlim, ylim=zlim, sort=sort, grid.lines=grid.lines, cross.hairs=cross.hairs)
	vt3.testplot (v, main = main [3], xlab=ylab, ylab=zlab, proj="yz",
		xlim=ylim, ylim=zlim, sort=sort, grid.lines=grid.lines, cross.hairs=cross.hairs)
	par (p0)
}

spin3d = function (v=CUBE, ..., t=5)
{	N = 30
	k = seq (0, -4,, N + 2)

	rng = range (v)
	mrng = apply (rng, 1, mean)

	trlv2 = btrl2 (-mrng [1], -mrng [2])
	trlv3 = btrl3 (-mrng [1], -mrng [2], -mrng [3])
	v = v %]*% trlv3
	rng = rng %]*% (trlv2 %*% bscl2 (2) )

	trans = function (u, i=1)
		u %]*% (brot3z (k [i] / 2) %*% brot3x (- k [i] / 4) )

	t0 = as.numeric (Sys.time () )
	vtr = trans (v)
	plot.new ()
	plot.window (xlim = rng [1,], ylim = rng [2,])
	plot (vtr, add=TRUE)
	t1 = as.numeric (Sys.time () )
	dt1 = t1 - t0

	wt = ( (t - dt1) / (N - 1) ) - dt1

	if (wt >= 0)
		incr = wt + dt1
	else
	{	incr = dt1
		wt = 0
	}

	i = 2
	rt = dt1
	while (rt < t)
	{	Sys.sleep (wt)
		rt = rt + incr
		vtr = trans (v, i)
		plot.new ()
		plot.window (xlim = rng [1,], ylim = rng [2,])
		plot (vtr, add=TRUE)
		i = i + 1
	}
	print (rt)
}
