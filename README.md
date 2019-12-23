# onomastic
> Deterministic classifier for personal names


```
$ onomastic --help
Usage: onomastic (-g|--givens FILE) (-f|--families FILE) [-F|--file FILE]
                 [-o|--output-format FORMAT] [-B|--bonus BONUS]
                 [-t|--transliterate] [-u|--unknown-as-family]
                 [-b|--break-full-names]
  Deterministic classifier for personal names

Available options:
  -g,--givens FILE         Givens filename
  -f,--families FILE       Families filename
  -F,--file FILE           Input filename
  -o,--output-format FORMAT
                           Output format {tagged|csv|padded}. Default is
                           'tagged'
  -B,--bonus BONUS         Try to maximize length of a name group. Options are
                           {no|given|family}. Default is 'no'
  -t,--transliterate       Transliterate names before classifying them
  -u,--unknown-as-family   Treat unknown names as family names
  -b,--break-full-names    Force split of ambiguous full names
  -h,--help                Show this help text

```

## TL;DR

`onomastic` is an algorithm for classifying personal names deterministically, using given and family names lists. `onomastic` tries to minimize misclassifications, and does not make inferences about ambiguous personal names unless forced to do so.

Sample usage:

```bash
$ echo "Bulgarelli Manfroni Franco Leonardo" | onomastic --givens test/data/givens.txt  --families test/data/families.txt --bonus given  -utb
GivenAndFamily:Franco Leonardo,Bulgarelli Manfroni
```

It offers both a haskell and [python](github.com/flbulgarelli/onomaspy) implementation. It can be used as a library or command-line command.

## The problem

Human names - aka _personal names_ - are usually composed of many individual names - first name, middle name, last name, for example - which can be grouped into two main sections: givens names and family names.

There is no single way of properly writing a personal name - the proper order f individual names may even vary from culture to culture. For example, two common formats are display-order and sort-order:

Thanks to those common and easily recognizable formats, it is simple to analyse a string that represents a name and infer which parts correspond to the given names and family names. For example:

There are a lot of good packages that effectively perform this task using parsers or regular expressions:

* [namae](https://github.com/berkmancenter/namae)
* [name-parser](https://github.com/theiconic/name-parser)
* [nameparser](https://pypi.org/project/nameparser/)

However, people do not always follow those conventions when they manually enter personal names. It is common to deal with lists like the following:


```bash
Ástor Pantaleón Piazzolla # first-name middle-name surname
Andreu Francis            # surname first-name
Troilo Aníbal Carmelo     # surname first-name middle-name
Goyeneche, Roberto        # surname, first-name
Merello, Laura Ana        # surname, first-name middle-name
Julia, Trzenko            # first-name, surname
```

In such situations, format-based algorithms will not solve our problem. You need something that actually undestands about individual names. Although you could use a machine learning algorithm - [see this article](https://towardsdatascience.com/name-classification-with-naive-bayes-7c5e1415788a) - improper classification of personal names can be a sensible thing. Also, getting a big list of real names can turn into troubles.

Because of this using a deterministic algorithm that only requires datasets of given and family names - and not just personal names - is a better approach.


## The solution

`onomastic` classifies personal names using given and families list, which can be obtained from different sources depending your country or location. `onomastic` is designed to classify names only when they are not ambiguous, but this restriction can be relaxed using different flags.

## CLI Usage

`onomastic` can be also used as a command-line program, which processes a personal full name per line from standard input, and prints each result to standard output:

```bash
$ onomastic --givens test/data/givens.txt  --families test/data/families.txt --bonus given -utb  <<EOF
Ástor Pantaleón Piazzolla
Andreu Francis
Troilo Aníbal Carmelo
Goyeneche, Roberto
Merello, Laura Ana
Julia, Trzenko
EOF

GivenAndFamily:Ástor Pantaleón,Piazzolla
GivenAndFamily:Francis,Andreu
GivenAndFamily:Aníbal Carmelo,Troilo
GivenAndFamily:Roberto,Goyeneche
GivenAndFamily:Laura Ana,Merello
GivenAndFamily:Julia,Trzenko
```


### Available options

#### `-g, --givens`

`onomastic` needs a list of given names, one name per line. The givens file content should look like the following:

```
Alvesio
Alvia
Alvida
Alvin
Alvina
Alvis
Alvita
Alys
Alysa
Alyson
Alyssa
Alzena
Amabel
Amabelia
Amabelio
Amable
Amada
Amadeo
```

**This is a requied option**.


#### `-f,--families`

`onomastic` needs a list of family names - aka surnames -, one name per line. The families file content should look like the following:

```
Acasio
Accino
Accorinti
Acebo
Aceituno
Acero
Aceval
Acevedo
Aceñero
Acha
Achacata
Achaval
Achemon
Achilli
Achucarro
```


**This is a requied option**.

#### `-F,--file`

Instead of reading personal names from standard input, they may be read from a file with a personal name per line. Example:

```bash
$ onomastic --givens test/data/givens.txt  --families test/data/families.txt  -F names.txt
GivenAndFamily:Ástor Pantaleón,Piazzolla
GivenAndFamily:Francis,Andreu
GivenAndFamily:Aníbal Carmelo,Troilo
GivenAndFamily:Goyeneche,Roberto
GivenAndFamily:Ana,Merello Laura
GivenAndFamily:Julia,Trzenko
```

#### `-o,--output-format`

Output format {tagged|csv|padded}. Default is 'tagged'

#### `-B,--bonus BONUS`

Try to maximize length of a name group. Options are {no|given|family}. Default is 'no'

#### `-t,--transliterate`

Transliterate names before classifying them
#### `-u,--unknown-as-family`

Treat unknown names as family names

#### `-b,--break-full-names`

Force split of ambiguous full names

### More examples


```bash
$ echo "Franco Bulgarelli" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli

$ echo "Franco Bulgarelli Manfroni" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Franco,Bulgarelli Manfroni

# some ambiguous splits will be prevented by default
$ echo "Bulgarelli Manfroni Franco Leonardo" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
FullName:Bulgarelli Manfroni Franco Leonardo

# however you can force them using the -b flag
$ echo "Bulgarelli Manfroni Franco Leonardo" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -utb
GivenAndFamily:Leonardo,Bulgarelli Manfroni Franco

$ echo "Feldfeber Kivelski Ivana" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Ivana,Feldfeber Kivelski

$ echo "Julian Berbel Alt" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "BERBEL ALT julian" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Julian,Berbel Alt

$ echo "Finzi Nadia Giselle" | onomastic --givens test/data/givens.txt  --families test/data/families.txt  -ut
GivenAndFamily:Nadia Giselle,Finzi

$ echo "Bulgarelli Manfroni Franco Leonardo" | onomastic --givens test/data/givens.txt  --families test/data/families.txt --bonus family  -utb
GivenAndFamily:Leonardo,Bulgarelli Manfroni Franco

$ echo "Bulgarelli Manfroni Franco Leonardo" | onomastic --givens test/data/givens.txt  --families test/data/families.txt --bonus given  -utb
GivenAndFamily:Franco Leonardo,Bulgarelli Manfroni
```

##  Caveats and future work

`onomastic` is far from perfect. It does currently not deal with:

* titles, initials and nicknames
* gender
* compound names like `Juan Cruz` or `María de los Angeles`
