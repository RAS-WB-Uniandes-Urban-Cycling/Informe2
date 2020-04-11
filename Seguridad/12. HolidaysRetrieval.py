# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import holidayapi

hapi = holidayapi.v1('8172b8cc-45a2-4b5b-b5b3-1dd79030f5de')

parameters = {
	# Required
	'country': 'CO',
	'year':    2017,
	# Optional
	# 'month':    7,
	# 'day':      4,
	# 'previous': True,
	# 'upcoming': True,
	# 'public':   True,
	# 'pretty':   True,
}

holidays2017 = hapi.holidays(parameters)

holidays=holidays2011['holidays'].keys()+holidays2012['holidays'].keys()+holidays2013['holidays'].keys()+holidays2014['holidays'].keys()+holidays2015['holidays'].keys()+holidays2016['holidays'].keys()+holidays2017['holidays'].keys()

import numpy as np

np.savetxt("Holidays.csv", holidays, delimiter=",", fmt='%s')
