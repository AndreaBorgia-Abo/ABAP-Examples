*&---------------------------------------------------------------------*
*& Report ZABO_PRIME_REDUCE
*&---------------------------------------------------------------------*
*& Author: Sandra Rossi
*& Source: https://answers.sap.com/answers/13697145/view.html
*&---------------------------------------------------------------------*
REPORT ZABO_PRIME_REDUCE.

DATA(lv_prime) = REDUCE string(
    INIT text1 = `Prime Numbers:`
         mod0_count = 0
    FOR i = 2 THEN i + 1 WHILE i <= 100
    FOR j = 2 THEN j + 1 WHILE j < i
    NEXT mod0_count = COND #( WHEN j = 2 AND i MOD j = 0 THEN 1
                              WHEN j = 2 THEN 0
                              WHEN i MOD j = 0 THEN mod0_count + 1
                              ELSE mod0_count )
         text1      = COND #( WHEN j = i - 1 AND mod0_count = 0
                              THEN text1 && | { i }| && |,|
                              ELSE text1  ) ).
