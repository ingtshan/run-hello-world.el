#!/usr/bin/env sh

filename='hello-world.txt'
mkdir -p tmp
mkdir -p tmp/hello-world-list
for x in {a..z}; do echo "'(" >tmp/hello-world-list/$x.el; done
languageStart=false
while read line; do
    if [ "$line" = "<!--Languages start-->" ]; then
        languageStart=true
    elif [ "$line" = "<!--Languages end-->" ]; then
        languageStart=false
    fi

    if $languageStart; then
        eval $(echo "$line" | sed -rn 's@^.*\[(.*)\]\((.*)\).*$@name="\1";file="\2"@p')

        if [[ ${file%/*} = [a-z] ]]; then
            echo "(\"$name\" . \"$file\")" | tee -a tmp/hello-world-list/${file%/*}.el
        fi
    fi
done <$filename
for x in {a..z}; do echo ")" >>tmp/hello-world-list/$x.el; done
