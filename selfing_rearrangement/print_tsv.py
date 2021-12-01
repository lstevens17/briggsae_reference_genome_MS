#!/usr/bin/env python3
import argparse

def parse_orthogroups_file(orthogroups_file, sp1, sp2):
	orthogroups_dict, orthogroups_list = {}, []
	with open(orthogroups_file, 'r') as orthogroups:
		for line in orthogroups:
			seqlist = line.rstrip("\n").split(": ")[1].split(" ")
			for seq in seqlist:
				if seq.startswith(sp1):
					sp1_seq = seq
				elif seq.startswith(sp2):
					sp2_seq = seq
			orthogroups_dict[sp1_seq] = sp2_seq
			orthogroups_list += [sp1_seq, sp2_seq]
	return orthogroups_dict, orthogroups_list

def parse_gff_files(gff_file, prefix, orthogroups_list):
	gff_dict = {}
	gene_number = 0
	with open(gff_file, 'r') as gff:
		for line in gff:
			if not line.startswith("#"):
				columns = line.rstrip("\n").split("\t")
				chr, feature, start, stop, col9 = columns[0], columns[2], columns[3], columns[4], columns[8]
				if feature == "mRNA":
					transcriptID = col9.split(";")[0].split("=")[1].replace("Transcript:", "").replace("transcript:", "")
					if prefix == "CREMA":
						transcriptID = prefix + "." + transcriptID.replace("WGS:WUAV", "WGS_WUAV")
					elif prefix == "CELEG":
						transcriptID = prefix + "." + ".".join(transcriptID.split(".")[0:2])
					elif prefix == "CBRIG":
						#transcriptID = prefix + "." + transcriptID.split(".")[0]
						transcriptID = prefix + "." + transcriptID
					elif prefix == "CINOP" or prefix == "CTROP" or prefix == "CNIGO":
						transcriptID = prefix + "." + transcriptID
					if not "CDS:" in transcriptID and transcriptID in orthogroups_list:
						gff_dict[transcriptID] = [chr, start, stop, str(gene_number)]
						gene_number += 1
	return gff_dict
					

def print_tsv_file(orthogroups_dict, sp1gff_dict, sp2gff_dict, sp1, sp2):
	for sp1_seq, sp2_seq in orthogroups_dict.items():
		sp1_seq_list = sp1gff_dict[sp1_seq]
		sp2_seq_list = sp2gff_dict[sp2_seq]
		print("%s\t%s\t%s\t%s\t%s\t%s" % (sp1, sp1_seq, "\t".join(sp1_seq_list), sp2, sp2_seq, "\t".join(sp2_seq_list)))

if __name__ == "__main__":
	SCRIPT = "print_tsv.py"
	parser = argparse.ArgumentParser()
	parser.add_argument("-o", "--orthogroups", type=str, help = "Single-copy orthogroups from OrthoFinder", required=True)
	parser.add_argument("-1", "--sp1", type=str, help = "5-letter prefix of species 1 (e.g. CELEG)", required=True)
	parser.add_argument("-2", "--sp2", type=str, help = "5-letter prefix of species 2 (e.g. CBRIG)", required=True)
	parser.add_argument("-y", "--sp1gff", type=str, help = "GFF3 file for species 1", required=True)
	parser.add_argument("-z", "--sp2gff", type=str, help = "GFF3 file for species 2", required=True)
	args = parser.parse_args()
	orthogroups_file = args.orthogroups
	sp1, sp2 = args.sp1, args.sp2
	sp1gff_file, sp2gff_file = args.sp1gff, args.sp2gff
	orthogroups_dict, orthogroups_list = parse_orthogroups_file(orthogroups_file, sp1, sp2)
	sp1gff_dict = parse_gff_files(sp1gff_file, sp1, orthogroups_list)
	sp2gff_dict = parse_gff_files(sp2gff_file, sp2, orthogroups_list)
	print_tsv_file(orthogroups_dict, sp1gff_dict, sp2gff_dict, sp1, sp2)


	

