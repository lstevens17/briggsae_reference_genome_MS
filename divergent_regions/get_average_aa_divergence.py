import sys

with open(sys.argv[1], 'r') as bedfile:
	windows_list = []
	for line in bedfile:
		cols = line.rstrip("\n").split("\t")
		windows_list.append(cols)

with open(sys.argv[2], 'r') as aa_divergence:
	aa_divergence_dict = {}
	for line in aa_divergence:
		cols = line.rstrip("\n").split("\t")
		chr, pos, id = cols[2], int(cols[1]), float(cols[3])
		try:
			aa_divergence_dict[chr].append([pos, id])
		except KeyError:
			aa_divergence_dict[chr] = [[pos, id]]

for window in windows_list:
	chr, start, stop = window[0], int(window[1]), int(window[2])
	posid_list = aa_divergence_dict[chr]
	id_list = []
	for posid in posid_list:
		pos, id = posid[0], posid[1]
		if pos >= start and pos <= stop:
			id_list.append(id)
	print("%s\t%s\t%s\t%s" % (chr, start, stop, sum(id_list)/len(id_list)))

