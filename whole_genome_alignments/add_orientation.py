import sys

with open(sys.argv[1], 'r') as coords_file:
	for line in coords_file:
		cols = line.rstrip("\n").split("\t")
		if int(cols[2]) > int(cols[3]):
			print(line.rstrip("\n") + "\treverse")
		else:
			print(line.rstrip("\n") + "\tforward")
