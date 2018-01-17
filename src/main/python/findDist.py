import re
import os
import time
import numpy as np

# Script finds the longest physical path/wire from:
#	report_timing -through $cxPins -setup -path_type short -max_paths 600 -file $FNAME

def sortList(d):
	dat = []
	for i,x in enumerate(d):
		if (i % 2 == 0):
			dat.append(x)
	dist = []
	for i, x in enumerate(dat):
		if (i % 2 == 0):
			xy = ( abs(int(dat[i+1][0])-int(x[0])), abs(int(dat[i+1][1])-int(x[1])) )
			dist.append(xy)
	return dist

def getLongPaths(d, N):
	data = []
	weight = 2.0 # Y is 2x faster (in slices) than X routing
	for e in d:
		res = (((e[0]*2.0)**2)+e[1]**2)**0.5
		data.append(res)
	order = np.argsort(data)[::-1][:N]
	final = []
	for o in order:
		final.append(d[o])
	return final

directory = "./outputs/paths/"
reports = os.listdir(directory)
print reports

re1='.*?'	# Non-greedy match on filler
re2=' '	# Uninteresting: ws
re3='.*?'	# Non-greedy match on filler
re4=' '	# Uninteresting: ws
re5='.*?'	# Non-greedy match on filler
re6='( )'	# White Space 1
re7='(SLICE)'	# Word 1
re8='.*?'	# Non-greedy match on filler
re9='(X)'	# Any Single Word Character (Not Whitespace) 1
re10='(\\d+)'	# Integer Number 1
re11='(Y)'	# Any Single Word Character (Not Whitespace) 2
re12='(\\d+)'	# Integer Number 2
re13='( )'	# White Space 2
pattern = re1+re2+re3+re4+re5+re6+re7+re8+re9+re10+re11+re12+re13

for report in reports:
	t1 = time.time()
	print report,
	with open(directory+report, 'r') as f:
		txt = f.read().replace('\n','')

	raw = re.findall(pattern, txt)
	slices = [''.join(y).strip() for y in raw]
	coords = [ re.findall(re12, y) for y in slices ]

	# gets the length in (x,y) of each path
	xylen = sortList(coords)
	top10 = getLongPaths(xylen, 10)
	print top10,
	t2 = time.time()
	print (t2-t1)
