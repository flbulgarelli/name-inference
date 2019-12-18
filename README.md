# name-inference

```
name-inference -h
Usage: name-inference (-g|--givens FILE) (-f|--families FILE) [-F|--file FILE]
                      [-X|--output-format tagged|csv|padded]
                      [-t|--transliterate] [-u|--unknown-as-family]
  Classify and flip personal names

Available options:
  -g,--givens FILE         givens filename
  -f,--families FILE       families filename
  -F,--file FILE           families filename
  -X,--output-format tagged|csv|padded
                           output format. `tagged` by default
  -t,--transliterate       transliterate names
  -u,--unknown-as-family   Treat unknown names as family names
  -h,--help                Show this help text
```


## Examples


```bash
$ echo "Franco Bulgarelli" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli

$ echo "Franco Bulgarelli Manfroni" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli Manfroni

$ echo "Feldfeber Kivelski Ivana" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Ivana,Feldfeber Kivelski

$ echo "Julian Berbel Alt" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "BERBEL ALT julian" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "Finzi Nadia Giselle" | name-inference --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Nadia Giselle,Finzi
```


