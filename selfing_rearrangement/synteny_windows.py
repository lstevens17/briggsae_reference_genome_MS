#!/usr/bin/env python3
import argparse


def print_synteny_windows(tsv_file, window_size):
	with open(tsv_file, 'r') as tsv:
		window_stop = window_size
		prev_sp1_chr, prev_sp1_order, prev_sp2_chr, prev_sp2_order = '', 0, '', 0
		syn_count, break_count = 0, 0
		for line in tsv:
			cols = line.rstrip("\n").split("\t")
			sp1chr, sp1stop, sp1order, sp2chr, sp2stop, sp2order = cols[2], int(cols[4]), int(cols[5]), cols[8], int(cols[10]), int(cols[11])
			if prev_sp1_chr == '':
				prev_sp1_chr, prev_sp1_stop, prev_sp2_chr, prev_sp2_order = sp1chr, sp1stop, sp2chr, sp2order
			else:
				if sp1stop <= window_stop and sp1chr == prev_sp1_chr: # same chrom same window
					if abs(sp2order - prev_sp2_order) == 1:
						syn_count += 1
					else:
						break_count += 1
				elif sp1stop > window_stop and sp1chr == prev_sp1_chr: # new window
					try:
						print("%s\t%s\t%s\t%s\t%s\t%s" % (prev_sp1_chr, window_stop-window_size, window_stop, syn_count, break_count, (syn_count/(syn_count+break_count))))
					except ZeroDivisionError:
						if syn_count + break_count == 0:
							pass
						else:
							sys.exit("One of these numbers is Zero!")
					window_stop += window_size
					syn_count, break_count = 0, 0
				else: # new chrom
					try:
						print("%s\t%s\t%s\t%s\t%s\t%s" % (prev_sp1_chr, window_stop-window_size, prev_sp1_stop, syn_count, break_count, (syn_count/(syn_count+break_count))))
					except ZeroDivisionError:
						if syn_count + break_count == 0:
							pass
						else:
							sys.exit("One of these numbers is Zero!")
					window_stop = window_size
				prev_sp1_chr, prev_sp1_stop, prev_sp2_chr, prev_sp2_order = sp1chr, sp1stop, sp2chr, sp2order

if __name__ == "__main__":
	SCRIPT = "synteny_windows.py"
	parser = argparse.ArgumentParser()
	parser.add_argument("-t", "--tsv", type=str, help = "TSV file from print_tsv.py", required=True)
	parser.add_argument("-w", "--window", type=int, help = "Window size", required=True)
	args = parser.parse_args()
	tsv_file = args.tsv
	window_size = args.window
	print_synteny_windows(tsv_file, window_size)
