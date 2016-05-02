#!/bin/bash

git remote update
git checkout master
git pull
git checkout develop
git pull

appfile=`ls src/*.app.src`

app=`echo $appfile | awk -F"[/.]" '{print $2;}'`

old=`grep vsn $appfile | awk -F"\"" '{print $2}'`

major=`echo $old | awk -F"\." '{print $1}'`
minor=`echo $old | awk -F"\." '{print $2}'`
patch=`echo $old | awk -F"[\.-]" '{print $3}'`

snapshot=`echo $old | awk -F"[\.-]" '{print $4}'`

# if [[ -z $snapshot ]] || [[ $snapshot != "SNAPSHOT" ]]; then
#     echo "develop isn't set to a SNAPSHOT version in $appfile. Aborting...";
#     exit 1;
# fi

if [ ! -z $1 ]; then
    if [ $1 == 'minor' ]; then
        minor=$minor+1
        patch=0
    elif [ $1 == 'major' ]; then
        major=$major+1
        minor=0
        patch=0
    fi
else
    patch=$path+1
fi

new=$major.$minor.$patch

echo "bumping $app from $old to $new"

git flow release start $new

files="$appfile relx.config Makefile"
for i in $files; do
    echo "patching $i";
    sed -i s/$old/$new/ $i;
    git add $i;
done
git commit -m"bump version $new"

git flow release finish $new
#git checkout master
#git push
#git push --tags

# git checkout develop
# git pull

# let patch=$patch+1
# snapshot="$major.$minor.$patch-SNAPSHOT"

# echo "bumping $app to snapshot $snapshot"

# files="$appfile relx.config Makefile"
# for i in $files; do
#     echo "patching $i";
#     sed -i s/$new/$snapshot/ $i;
#     git add $i;
# done
# git commit -m"bump version $snapshot"
#git push
