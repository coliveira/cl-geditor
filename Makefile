all:
	echo 'targets: run, clean'

run:
	echo '(load "geditor") (main)' | sbcl

clean:
	rm -f *.fasl *.exe
