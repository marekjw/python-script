cd src
bnfc --haskell -d   -m ../grammar/PythonScript.cf  &&  make
cd ..

echo "TESTING..."

for testcase in $1/*.ps 
do
    ./src/PythonScript/Test < ${testcase} > /dev/null
    if [[ "$?" -eq 1 ]] 
    then
        echo "FAIL  $testcase"
    fi
done

echo "TESTING DONE"
