#!/usr/bin/env python3
import argparse

def parse_tsv(tsv_file):
	with open(tsv_file, 'r') as tsv:
		count_dict = {}
		prev_sp1chr = ''
		for line in tsv:
			columns = line.rstrip("\n").split("\t")
			sp1chr, sp1num, sp2chr, sp2num = columns[2], int(columns[5]), columns[8], int(columns[11])
			if not prev_sp1chr == '':
				if sp1chr == prev_sp1chr and sp2chr == prev_sp2chr: # MAJOR ASSUMPTION HERE
					if abs(sp2num - prev_sp2num) == 1:
						try:
							count_dict[sp1chr][0] += 1
						except KeyError:
							count_dict[sp1chr] = [1, 0]
					else:
						try:
							count_dict[sp1chr][1] += 1
						except KeyError:
							count_dict[sp1chr] = [0, 1]
			prev_sp1chr, prev_sp1num, prev_sp2chr, prev_sp2num = sp1chr, sp1num, sp2chr, sp2num
			syn_count, break_count = 0, 0
		for chr, counts in count_dict.items():
			syn_count += counts[0]
			break_count += counts[1]
			print("%s\t%s\t%s\t%s" % (chr, counts[0], counts[1], round((counts[0]/(counts[0]+counts[1])*100), 2)))
		print("%s\t%s\t%s\t%s" % ("Total", syn_count, break_count, round((syn_count/(syn_count+break_count))*100, 2)))

if __name__ == "__main__":
	SCRIPT = "calculate_synteny"
	parser = argparse.ArgumentParser()
	parser.add_argument("-t", "--tsv", type=str, help = "TSV file from print_tsv.py", required=True)
	args = parser.parse_args()
	tsv_file = args.tsv
	parse_tsv(tsv_file)
