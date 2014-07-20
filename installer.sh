rm -r dist
cabal install
cp dist/build/qs/qs ~/bin/
cp dist/build/qs/qs ${PATH%%:*}
echo 'Installed !!!!'
