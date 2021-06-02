*&---------------------------------------------------------------------*
*& Report zabo_lastchar
*&---------------------------------------------------------------------*
*& Author: Frederik Hudak
*& Source: https://blogs.sap.com/2020/01/18/how-to-retrieve-the-last-character-of-a-string-in-abap-definitive-edition/
*& See also:
*&  https://blogs.sap.com/2013/05/08/abap-profiling-in-eclipse/
*&---------------------------------------------------------------------*
REPORT zabo_lastchar.


class lcl_string_util definition create private.

  public section.

    " feel free to substitute your favorite single character abstraction
    types: t_last_character type c length 1.

    class-methods:
      get_using_substring
        importing str           type string
        returning value(result) type t_last_character.

    class-methods:
      get_using_off_len
        importing str           type string
        returning value(result) type t_last_character.

    class-methods get_using_shift
      importing value(str)    type string
      returning value(result) type t_last_character.

    class-methods get_using_builtin_reverse
      importing str           type string
      returning value(result) type t_last_character.

    class-methods get_using_find_regex
      importing str           type string
      returning value(result) type t_last_character.

    class-methods get_using_regex
      importing str           type string
      returning value(result) type t_last_character.


endclass.

class lcl_string_util implementation.

  method get_using_substring.
    result = substring( off = strlen( str ) - 1 len = 1 val = str ).
  endmethod.

  method get_using_off_len.
    data(index_before_last) = strlen( str ) - 1.
    result = str+index_before_last(1).
  endmethod.

  method get_using_shift.
    shift str right by 1 places circular.
    result = str(1).
  endmethod.

  method get_using_builtin_reverse.
    result = substring(
      val = reverse( str )
      len = 1 ).
  endmethod.

  method get_using_find_regex.
    find regex '.$' in str results data(res).
    result = str+res-offset(res-length).
  endmethod.

  method get_using_regex.
    result = match( val = str regex = '.' occ = -1 ).
  endmethod.


endclass.


class lcl_profiler definition create private.
  public section.

    class-data: guid type string.
    class-data: really_long_string type string.
    class-data: n type i.

    class-methods:
      off_len,
      substring,
      shift,
      builtin_reverse,
      find_regex,
      regex.

endclass.



class lcl_profiler implementation.

  method off_len.
    do n times.
      lcl_string_util=>get_using_off_len( guid ).
      lcl_string_util=>get_using_off_len( really_long_string ).
    enddo.
  endmethod.

  method substring.
    do n times.
      lcl_string_util=>get_using_substring( guid ).
      lcl_string_util=>get_using_substring( really_long_string ).
    enddo.
  endmethod.

  method shift.
    do n times.
      lcl_string_util=>get_using_shift( guid ).
      lcl_string_util=>get_using_shift( really_long_string ).
    enddo.
  endmethod.

  method builtin_reverse.
    do n times.
      lcl_string_util=>get_using_builtin_reverse( guid ).
      lcl_string_util=>get_using_builtin_reverse( really_long_string ).
    enddo.
  endmethod.

  method find_regex.
    do n times.
      lcl_string_util=>get_using_find_regex( guid ).
      lcl_string_util=>get_using_find_regex( really_long_string ).
    enddo.
  endmethod.

  method regex.
    do n times.
      lcl_string_util=>get_using_regex( guid ).
      lcl_string_util=>get_using_regex( really_long_string ).
    enddo.
  endmethod.


endclass.


start-of-selection.

  lcl_profiler=>n = 10000.

  lcl_profiler=>guid = `CAC01893-9097-1EEA-8F89-E062D3164D49`.
  lcl_profiler=>really_long_string = ``.

  do 5000 times.
    lcl_profiler=>really_long_string = lcl_profiler=>really_long_string && lcl_profiler=>guid.
  enddo.

  lcl_profiler=>off_len( ).
  lcl_profiler=>substring( ).
  lcl_profiler=>shift( ).
  lcl_profiler=>builtin_reverse( ).
  lcl_profiler=>find_regex( ).
  lcl_profiler=>regex( ).
