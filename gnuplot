set xr [0:50]   
set yr [0:50]

set size square   

flePnts = 'pnts.dat'
fleEdges = 'edges.dat'

loadEdges = sprintf('< gawk '' \
    FNR==NR{x[$1]=$2;y[$1]=$3;next;} \
    {printf "%%f\t%%f\n%%f\t%%f\n\n", x[$1], y[$1], x[$2], y[$2];} \
'' %s %s', flePnts, fleEdges); 

loadWeights = sprintf('< gawk '' \
    FNR==NR{x[$1]=$2;y[$1]=$3;next;} \
    {printf "%%f\t%%f\t%%s\n", (x[$1]+x[$2])/2 + $4, (y[$1]+y[$2])/2 + $5, $3} \
'' %s %s', flePnts, fleEdges);

plot \
    loadEdges using 1:2 with lines lc rgb "black" lw 2 notitle, \
    flePnts using 2:3:(0.6) with circles fill solid lc rgb "black" notitle, \
    flePnts using 2:3:1 with labels tc rgb "white" font "Arial Bold" notitle, \
    loadWeights using 1:2:3 with labels tc rgb "red" center font "Arial Bold" notitle
