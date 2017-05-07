#!/usr/bin/env python
# Copyright: This document has been placed in the public domain.

"""
Taylor diagram (Taylor, 2001) test implementation.

http://www-pcmdi.llnl.gov/about/staff/Taylor/CV/Taylor_diagram_primer.htm
"""

__version__ = "Time-stamp: <2012-02-17 20:59:35 ycopin>"
__author__ = "Yannick Copin <yannick.copin@laposte.net>"

import numpy as NP
import matplotlib.pyplot as PLT

class TaylorDiagram(object):
    """Taylor diagram: plot model standard deviation and correlation
    to reference (data) sample in a single-quadrant polar plot, with
    r=stddev and theta=arccos(correlation).
    """

    def __init__(self, refstd, refacor, colors, fig=None, rect=111, label='_'):
        """Set up Taylor diagram axes, i.e. single quadrant polar
        plot, using mpl_toolkits.axisartist.floating_axes. refstd is
        the reference standard deviation to be compared to.
        """

        from matplotlib.projections import PolarAxes
        import mpl_toolkits.axisartist.floating_axes as FA
        import mpl_toolkits.axisartist.grid_finder as GF

        self.refstd = refstd            # Reference standard deviation
        self.refacor = refacor

#        print("self.refstd =",self.refstd)

        self.colors = colors
        tr = PolarAxes.PolarTransform()

        # Correlation labels
        rlocs = NP.concatenate((NP.arange(10)/10.,[0.95,0.99,1.0]))
        tlocs = NP.arccos(rlocs)        # Conversion to polar angles
        gl1 = GF.FixedLocator(tlocs)    # Positions
        tf1 = GF.DictFormatter(dict(zip(tlocs, map(str,rlocs))))

        # Standard deviation axis extent
        self.smin = 0
        self.smax = 1.5*self.refstd
  
#        print("self.smax =",self.smax)

        ghelper = FA.GridHelperCurveLinear(tr,
                                           extremes=(0,NP.pi/2, # 1st quadrant
                                                     self.smin,self.smax),
                                           grid_locator1=gl1,
                                           tick_formatter1=tf1,
                                           )

        if fig is None:
            fig = PLT.figure()

        ax = FA.FloatingSubplot(fig, rect, grid_helper=ghelper)
        fig.add_subplot(ax)

        # Adjust axes
        ax.axis["top"].set_axis_direction("bottom")  # "Angle axis"
        ax.axis["top"].toggle(ticklabels=True, label=True)
        ax.axis["top"].major_ticklabels.set_axis_direction("top")
        ax.axis["top"].major_ticklabels.set_fontsize(8) 
        ax.axis["top"].label.set_axis_direction("top")
        ax.axis["top"].label.set_text("Correlação de Anomalia")
        ax.axis["top"].label.set_fontsize(8) 

        ax.axis["left"].set_axis_direction("bottom") # "X axis"
        ax.axis["left"].label.set_text("Raiz do Erro Quadrático Médio")
        ax.axis["left"].major_ticklabels.set_fontsize(8) 
        ax.axis["left"].label.set_fontsize(8) 

        ax.axis["right"].set_axis_direction("top")   # "Y axis"
        ax.axis["right"].toggle(ticklabels=True)
        ax.axis["right"].major_ticklabels.set_axis_direction("left")
        ax.axis["right"].major_ticklabels.set_fontsize(8) 

        ax.axis["bottom"].set_visible(False)         # Useless
        
        # Contours along standard deviations
        ax.grid(False)

        self._ax = ax                   # Graphical axes
        self.ax = ax.get_aux_axes(tr)   # Polar coordinates

        # Add reference point and stddev contour
        print ("Reference std:", self.refstd)

        l, = self.ax.plot(NP.arccos(self.refacor), self.refstd, 'k*',
                          ls='', ms=6, label=label)

#        l, = self.ax.plot(NP.arccos(float(corrcoef)), float(stddev),
#                          *args, **kwargs) # (theta,radius)

        t = NP.linspace(0, NP.pi/2)
        r = NP.zeros_like(t) + self.refstd
        self.ax.plot(t,r, 'k--', label='_')

        self.ax.plot([0.0, NP.arccos(float(self.refacor))],[0.0, 1.5*float(self.refstd)],color='black')

        # Collect sample points for latter use (e.g. legend)
        self.samplePoints = [l]

    def add_sample(self, stddev, corrcoef, colori, *args, **kwargs):
        """Add sample (stddev,corrcoeff) to the Taylor diagram. args
        and kwargs are directly propagated to the Figure.plot
        command."""

#        print()

        print("stddev =",stddev)
        print("corrcoef =",corrcoef)
        
        l, = self.ax.plot(NP.arccos(float(corrcoef)), float(stddev),
                          *args, **kwargs) # (theta,radius)
        self.samplePoints.append(l)

        t = NP.linspace(0, NP.pi/2)
        r = NP.zeros_like(t) + float(stddev)
        self.ax.plot(t,r, 'k--', label='_', color=colori)

#        self.ax.plot([0.0, NP.arccos(float(corrcoef))],[0.0, 1.5*float(stddev)],color=colori)

        return l

    # Adicionar viés?
    def add_contours(self, levels=5, **kwargs):
        """Add constant centered RMS difference contours."""

        rs,ts = NP.meshgrid(NP.linspace(self.smin,self.smax),
                            NP.linspace(0,NP.pi/2))
        # Compute centered RMS difference
        rms = NP.sqrt(self.refstd**2 + rs**2 - 2*self.refstd*rs*NP.cos(ts))
        
        contours = self.ax.contour(ts, rs, rms, levels, **kwargs)

        return contours
