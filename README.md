# onomastic

Make inferences about personal names

```
onomastic -h
Usage: onomastic (-g|--givens FILE) (-f|--families FILE) [-F|--file FILE]
                 [-X|--output-format tagged|csv|padded] [-t|--transliterate]
                 [-u|--unknown-as-family] [-b|--break-full-names]
  Classify and flip personal names

Available options:
  -g,--givens FILE         givens filename
  -f,--families FILE       families filename
  -F,--file FILE           families filename
  -X,--output-format tagged|csv|padded
                           output format. `tagged` by default
  -t,--transliterate       transliterate names
  -u,--unknown-as-family   Treat unknown names as family names
  -b,--break-full-names    Force split of ambiguous full names
  -h,--help                Show this help text
```


## Examples


```bash
$ echo "Franco Bulgarelli" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli

$ echo "Franco Bulgarelli Manfroni" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli Manfroni

$ echo "Feldfeber Kivelski Ivana" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Ivana,Feldfeber Kivelski

$ echo "Julian Berbel Alt" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "BERBEL ALT julian" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "Finzi Nadia Giselle" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Nadia Giselle,Finzi
```


