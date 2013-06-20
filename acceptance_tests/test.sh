for i in 1 2 3 4 5 6 7 8 9 10
     do
        echo "executing test $i"
        ../edi Texto.txt < e$i.txt > mi_salida$i.txt
        diff mi_salida$i.txt salida$i.txt
     done
