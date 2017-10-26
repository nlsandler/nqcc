for i in *.c
do
    gcc -m32 $i             #generate program with gcc
    ./a.out                 #run it
    expected=$?             #get exit code
    ../nqcc.byte $i         #generate assembly with nqcc
    assembly="${i%.*}"
    assembly=$assembly".s"  #get name of assembly file (for foo.c we want foo.s)
    gcc -m32 $assembly       #assemble it
    ./a.out                  #run the thing we assembled
    actual=$?                #get exit code
    echo -n "$i:    "
    if [ "$expected" -ne "$actual" ]
    then
        echo "FAIL"
    else
        echo "OK"
    fi
done

#cleanup
rm *.s
rm a.out
