pushd references
git checkout master
git pull
popd
git add references
git commit -m "Update references"
git push