# moodleClozeHelpers

## Easy creation of Moodle Cloze quizzes using R

The *moodleClozeHelpers* package is an add-on for the *exams* package to simplify and decrease the error-proneness of 
the process of creating "Embedded Answers" aka "Cloze" quizzes which can be imported to the Moodle Learning Management System.

A fictitious example rmarkdown quiz file can be retrieved using  
`system.file('examples',package='moodleClozeHelpers','cloze_quiz_example.Rmd')`

The main user functions of the `moodleClozeHelpers` package are:

- `new_clozeL()`: Creates a new container for the subquestions.
- `add_num()`, `add_string()`, `add_schoice()`, `add_mchoice()`: Adds a question asking for a number, a string, a single choice and multiple choices between multiple answer options, respectively.
- `print_answerlist()`: This list is required by Moodle whenever MCQs are involved. Best to leave this command in the Rmd file where it is.
- `print_metainfo()`: This extracts from each subquestion the type of question, points, answer options etc. Best to leave this command in the Rmd file where it is.

The subquestions are all collected in a list in a `clozeL` object and the order is important. Corresponding to that order, the boxes for entering the answers are positioned or referenced in the quiz document using `\#\#ANSWERX\#\#` where the X is replaced with the order number of that question in the list.

Basic standard rmarkdown can be used for formatting the text.

The last two commands `print_answerlist()` and `print_metainfo()` need to end the document in an `r` chunk.

## Example

As `moodleClozeHelpers` supports some Moodle cloze functionality which is not supported by the current CRAN `exams` package version 2.3-4, the two functions `exams2moodle()` and `read_metainfo()` are overwritten with slightly patched versions. This might break with future versions of the exams package. 

Most importantly, this allows in cloze questions for alternative answer options and corresponding point fractions in numeric and string subquestions in Moodle as well as for multiple choice questions with a set of correct answers instead of only one.

To convert the example rmarkdown quiz to a Moodle XML file:

1. Open the example quiz .Rmd file using
    `file.edit(system.file('examples',package='moodleClozeHelpers','cloze_quiz_example.Rmd'))`
2. Save it to your working directory.
3. Execute the following lines to convert the "cloze_quiz_example.Rmd" file to the Moodle XML file "cloze_quiz_example.xml" in your working directory.
    ```{r setup, echo=T, eval=F}
    library('exams')
    library('moodleClozeHelpers')
    # provisionally patch the exams package's functions
    source(system.file('examples',package='moodleClozeHelpers','read_metainfo_mod.R'))
    source(system.file('examples',package='moodleClozeHelpers','exams2moodle_mod.R'))
    assignInNamespace("exams2moodle",exams2moodle,ns="exams")
    assignInNamespace("read_metainfo",read_metainfo,ns="exams")
    # set a random seed to control reproducibiliy of the quizzes
    set.seed(6550)
    # convert Rmd to xml
    exams2moodle('cloze_quiz_example.Rmd', n=1, name='cloze_quiz_example', verbose=F)
    ```
4. The file "cloze_quiz_example.xml" can now be directly imported from within Moodle as Moodle XML format.

