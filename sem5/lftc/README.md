# Language Definition

### Alphabet

* upper (`A-Z`) and lower case letters (`a-z`) of the English alphabet
* underline character (`_`)
* decimal digits (`0-9`)

### Lexic

* special symbols, including:
  * operators `+ - * / = < <= == >=`
  * separators `[ ] { } : ; \s`
  * reserved words: `include using void int float string bool return for while if else`

* identifiers
  * a sequence of letters and digits, such that the first charater is a letter; the rule is:
    ```
    identifier = letter | letter{letter}{digit}
    letter = lowercase_letter | uppercase_letter
    lowercase_letter = "a" | "b" | ... | "z"
    uppercase_letter = "A" | "B" | ... | "Z"
    digit = "0" | "1" | ... | "9"
    ```
* constants
  * integers
    ```
    integer = +number | -number | number
    number = digit{number}
    ```
  * real values
    ```
    real_value = -number | number
    number = integer.real_part
    real_part = sequence_of_zeros number
    sequence_of_zeros = {0}sequence_of_zeros
    ```
  * character
    ```
    character = 'letter' | 'digit'
    ```
  * bool
    ```
    bool = false | true
    ```
  * string
    ```
    string = "string_literal"
    string_literal = char{string_literal}
    char = letter | digit
    ```

### Syntax:

* sintactic rules
  ```C++
  program = include_statement define_main statement finish_program

  include_statement = "#include<iostream>"
  define_main = "int main() {"
  finish_program = "return 0; }"

  single_statement = declaration_statement | assignment_statement
                   | conditional_statement | loop_statement | read_statement
                   | write_statement

  statement = single_statement | single_statement statement

  expression = arithmetic_expression | float_expression | char_expression | bool_expression

  integer_expression = integer_constant | identifier
                     | "(" arithmetic_expression ")"
                     | arithmetic_expression arithmetic_operator arithmetic_expression

  float_expression = float_constant
                   | identifier
                   | "(" float_expression ")"
                   | float_expression arithmetic_operator float_expression

  arithmetic_operator = "+" | "-" | "*" | "/"

  char_expression = char_constant | identifier

  bool_expression = bool_constant
                  | identifier
                  | "(" bool_expression ")"
                  | bool_expression relational_operator bool_expression

  relational_operator = "<" | ">" | "<=" | "==" | ">=" | "!="

  declaration_statement = primitive_declaration | array_declaration ";"

  primitive_type = "int" | "float" | "char" | "bool"

  primitive_declaration = primitive_type identifier
  array_declaration = primitive_type identifier "[" arithmetic_expression "]"

  assignment_statement = identifier "=" expression ";"

  conditional_statement = "if (" expression ") {" statement "}"
                          "else {" statement "}"

  loop_statement = "while (" expression ") {" statement "}"

  write_statement = "std::cout << " expression " << std::endl;"

  read_statement = "std::cin >> " identifier ";"
  ```

## Sample programs


Compute the circumference (perimeter) and area of circle with a given radius.

```C++
  1 #include<iostream>
  2
  3 int main() {
  4   float RADIUS;
  5   std::cin >> RADIUS;
  6
  7   float PI;
  8   PI = 3.14159;
  9
 10   float PERIMETER;
 11   PERIMETER = 2 * PI * RADIUS;
 12   std::cout << PERIMETER << std::endl;
 13
 14   float AREA;
 15   AREA = PI * RADIUS * RADIUS;
 16   std::cout << AREA << std::endl;
 17
 18   return 0;
 19 }
```

Determine the greatest common divisor of two natural numbers.

```C++
  1 #include<iostream>
  2
  3 int main() {
  4   int A;
  5   std::cin >> A;
  6
  7   int B;
  8   std::cin >> B;
  9
 10   while (A != B) {
 11     if (A > B) {
 12       A = A - B;
 13     } else {
 14       B = B - A;
 15     }
 16   }
 17
 18   std::cout << A << std::endl;
 19   return 0;
 20 }
```

Compute the sum of `N` real numbers.

```C++
  1 #include <iostream>
  2
  3 int main() {
  4   int N;
  5   std::cin >> N;
  6
  7   float SUM;
  8   SUM = 0;
  9 
 10   int I;
 11   I = 0;
 12 
 13   while (I < N) {
 14     float NUM;
 15     std::cin >> NUM;
 16     SUM = SUM + NUM;
 17     I = I + 1;
 18   }
 19 
 20   std::cout << SUM << std::endl;
 21   return 0;
 22 }
```

