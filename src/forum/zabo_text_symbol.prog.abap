*&---------------------------------------------------------------------*
*& Report ZABO_TEXT_SYMBOL
*&---------------------------------------------------------------------*
*& Author: Andrea Borgia
*& Source: https://answers.sap.com/questions/13541448/text-symbols-pro-and-cons-of-text-nnn-vs-textnnn.html
*&---------------------------------------------------------------------*
REPORT zabo_text_symbol.

WRITE / 'translatable text with trailing number'(001).

* Intentionally re-used same number!
WRITE / 'different text with trailing number'(001).

WRITE / TEXT-002.

WRITE / TEXT-003.
