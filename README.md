![linen logo](./linen_logo.png)

# linen - a stack-based terminal spreadsheet application

linen is a spreadsheet application designed for the terminal-dwelling users that want a lightweight application
to manage stuff you'd usually do in a spreadsheet program, but on the terminal.

Currently it is in a very early stage and should not really be relied upon. A list of features that I'd like to add one day can be found at the bottom of this README.

## Usage

Simply call linen from a shell: `$ linen`. This creates a new 10 by 10 sheet. 

Alternatively, to load a saved file, simply pass that as a second file as an argument. The default linen sheet file extension is `.lin`.

## Working principle

linen cells can contain a literal number, a literal string, or a formula. Numbers will have priority in parsing. A cell containing `29.3` will thus be registered as a number. Note that the decimal point has to be a full stop `.`; if instead a comma is used, the parsing as a number fails and the input is treated as a string instead. If a cell's contents start with an equals sign `=`, the content will be parsed as a formula.

Unlike traditional spreadsheet programs like Excel or LibreOffice Calc, linen formulas are parsed using a stack-based model. Referencing a cell or entering a literal pushes its value onto the calculation stack. Operators then pop the necessary values from the stack, and push the result back on to the stack. A cell's value is the topmost value. A few examples:

- `=2 3 +` will push the literal `2` to the stack, then push `3`, then the `+` will pop those two values and push their sum, `5`.
- `=3 4 5 + +` will push `3`, `4` and `5` onto the stack, sum the latter two to `9` and sum that and `3` to `12`. If the expression instead read `=3 4 5 +`, the `9` would be the topmost value and it would be returned instead.
- `=B1 B2 B3 * /` would take the values of `B2` and `B3`, multiply them together, then divide `B1` by that result.

Formulas are tokenised sensibly, i.e. cell names get priority. A formula like `=H2 6-B15+`, while ugly, is equivalent to `=H2 6 - B15 +`.

## Operations

### Basic maths

|operator|description|
|---|---|
|`+`|Pops two values and pushes their sum.|
|`-`|Pops two values and pushes their difference. Note the order of the values: `=6 2 -` will return `4`.|
|`*`|Pops two values and pushes their product.|
|`/`|Pops two values and pushes their quotient. Note the order of the values: `=6 2 /` will return `3`. Division by zero results in an error.|
|`%`|Pops two values and pushes the remainder of the second when dividing by the first; `=3 7 %` will return the remainder after dividing `7` by `3`, which is `1`.|
|`^`|Pops two values and pushes the second raised to the first power. `=2 6 ^` returns `36`.|


## Keybindings

|key|function|
|---|---|
|arrow keys|moves the cursor around|
|u|updates the entire window|
|s|saves the file. If the file was opened from an existing file, overwrites. If linen was opened without loading a file, this will save the current sheet to `sheet.lin`. This is temporary behaviour.|
|p|increases the precision of the current cell|
|o|decreases the precision of the current cell|
|h|decreases the width of the current column|
|l|increases the width of the current column|
|j|decreases the height of the current row|
|k|increases the height of the current row|
|=|starts editing the current cell as a formula|
|Enter|starts editing the current cell, or accepts the current value|
|Escape|discards the changed to the current cell, or exits the program|
|q|exits the program|

## Future features

- [x] functional references, with circular reference detection
- [ ] saving and loading files
- [ ] proper scrolling
- [ ] adding and removing columns and rows
- [ ] wrapping text in cells
- [ ] autocolouring cell references while editing formulas
- [ ] a way to autofill columns and rows based on the current cell's value
- [ ] syntax for cell ranges, and functions that can operate on them:
    - [ ] average, median and mode
    - [ ] max and min
- [ ] whole load of extra functions