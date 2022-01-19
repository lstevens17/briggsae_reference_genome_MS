import sys

CELEG_chr_len_dict = {"I":15072434, "II":15279421, "III":13783801, "IV":17493829, "V":20924180, "X":17718942}
CBRIG_chr_len_dict = {"I":15540809, "II":16595099, "III":14810976, "IV":17080301, "V":19933398, "X":22220885}

with open(sys.argv[1], 'r') as tsv:
	for line in tsv:
		cols = line.rstrip("\n").split("\t")
		CELEG_chr, CBRIG_chr = cols[2], cols[8]
		CELEG_start, CELEG_stop, CBRIG_start, CBRIG_stop = int(cols[3]), int(cols[4]), int(cols[9]), int(cols[10])
		CELEG_mid = (CELEG_start + CELEG_stop)/2
		CBRIG_mid = (CBRIG_start + CBRIG_stop)/2
		CELEG_chr_len = CELEG_chr_len_dict[CELEG_chr]
		CBRIG_chr_len = CBRIG_chr_len_dict[CBRIG_chr]
		print(line.rstrip("'\n") + "\t" + str(CELEG_mid/CELEG_chr_len) + "\t" + str(CBRIG_mid/CBRIG_chr_len))
