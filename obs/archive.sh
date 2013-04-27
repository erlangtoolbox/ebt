for f in out/dist/ebt-*.ez
do 
    cp $f out/obs/`basename $f .ez`.zip
done
