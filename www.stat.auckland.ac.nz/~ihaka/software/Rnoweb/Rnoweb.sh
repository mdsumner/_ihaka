for i in $*
do
    (echo "library(Rnoweb)"
     echo "noweb(\""$i"\")") | R --no-save --quiet --slave
done
