cd dist
bnfc --haskell -d   -m ../grammar/PythonScript.cf  &&  make
cd ..

echo "TESTING..."

for testcase in $2/*.ps 
do
    $1 < ${testcase} > /dev/null
    if [[ "$?" -eq 1 ]] 
    then
        echo "FAIL  $testcase"
    fi
done

echo "TESTING DONE"
