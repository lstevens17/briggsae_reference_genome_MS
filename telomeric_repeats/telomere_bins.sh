cat $1 | awk '/^>/{if (l!="") print l; printf "%s\t", $1; l=0; next}{l+=length($0)}END{print l}' | sed "s/>//" > ${1}.seqlen.tsv
seqkit locate --bed -M -G -p TTAGGC $1 | cut -f 1,2,3 | sort -k1,1 -k2,2n >${1}.teloRepeats.tsv
bedtools makewindows -g ${1}.seqlen.tsv -w 1000 | bedtools intersect -a stdin -b ${1}.teloRepeats.tsv -wa -wb | bedtools groupby -i stdin -g 1,2,3 -c 1 -o count | awk -F '\t' 'BEGIN{OFS=FS}{print $0, "telomeres"}' > telomere_${1}.1kw.tsv
