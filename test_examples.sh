for i in examples/*/*.c
do
    if [[ $i == *"cmps"* ]]; then
        # ignore cmps for now, not yet implemented
        continue
    fi
    
    gcc -m32 $i             #compile with gcc
    ./a.out                 #run it
    expected=$?             #get exit code
    ./nqcc $i              #compile with nqcc
    base="${i%.*}"
    $base                  #run the thing we assembled
    actual=$?                #get exit code
    echo -n "$i:    "
    if [ "$expected" -ne "$actual" ]
    then
        echo "FAIL"
    else
        echo "OK"
    fi
    rm $base
done

#cleanup
rm a.out
