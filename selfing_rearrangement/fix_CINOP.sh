sed 's/Sp34_Chr1/I/' $1 > tmp && mv tmp $1
sed 's/Sp34_Chr2/II/' $1 > tmp && mv tmp $1
sed 's/Sp34_Chr3/III/' $1 > tmp && mv tmp $1
sed 's/Sp34_Chr4/IV/' $1 > tmp && mv tmp $1
sed 's/Sp34_Chr5/V/' $1 > tmp && mv tmp $1
sed 's/Sp34_ChrX/X/' $1 > tmp && mv tmp $1
