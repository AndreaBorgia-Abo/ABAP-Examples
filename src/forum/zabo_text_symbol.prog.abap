*&---------------------------------------------------------------------*
*& Report ZABO_TEXT_SYMBOL
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Source: https://answers.sap.com/questions/13541448/text-symbols-pro-and-cons-of-text-nnn-vs-textnnn.html
*&---------------------------------------------------------------------*
REPORT zabo_text_symbol.

WRITE / 'translatable text with trailing number'(001).

* Intentionally re-used same number!
* It can be fixed as follows:
* Goto, Text elements
* Uilities, Adjust symbols (ctrl-shift-f7)
* Edit
* mark the final version, uncheck the other
* click replace
* click save
* confirm replace in program
* Now: the wrong string below should match the one above and symbol 001.
WRITE / 'different text with trailing number'(001).

WRITE / TEXT-002.

WRITE / TEXT-003.
