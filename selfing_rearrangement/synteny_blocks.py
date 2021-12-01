import sys

with open(sys.argv[1], 'r') as tsv:
	prev_sp1chr, prev_sp1order, prev_sp2chr, prev_sp2order = '', 0, '', 0
	synteny_block = 0
	for line in tsv:
		cols = line.rstrip("\n").split("\t")
		sp1chr, sp1order, sp2chr, sp2order = cols[2], int(cols[5]), cols[8], int(cols[11])
		if sp1chr == prev_sp1chr and abs(sp1order - prev_sp1order) == 1 and abs(sp2order - prev_sp2order) == 1:
			prev_sp1chr, prev_sp1order, prev_sp2chr, prev_sp2order = sp1chr, sp1order, sp2chr, sp2order
			synteny_block += 1
		else:
			if not prev_sp1chr == '': 
				print("%s\t%s" % (prev_sp1chr, synteny_block))
			prev_sp1chr, prev_sp1order, prev_sp2chr, prev_sp2order = sp1chr, sp1order, sp2chr, sp2order
			synteny_block = 0