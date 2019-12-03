*&---------------------------------------------------------------------*
*& Report ztho_advent_day_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztho_advent_day_01.

CLASS lcl_fuel_counter DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES t_masses TYPE TABLE OF i WITH EMPTY KEY.

    DATA mt_masses TYPE t_masses.

    METHODS constructor
      IMPORTING it_masses TYPE t_masses.
    METHODS run
      RETURNING
        VALUE(rv_fuel_sum) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_fuel_counter IMPLEMENTATION.
  METHOD constructor.
    mt_masses = it_masses.
  ENDMETHOD.

  METHOD run.
    rv_fuel_sum = REDUCE #( INIT x = 0 FOR wa IN mt_masses
              NEXT x = x + ( round( val = ( wa / 3 ) dec = 0 mode = cl_abap_math=>round_floor ) - 2 ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_fuel_counter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mt_masses TYPE lcl_fuel_counter=>t_masses.
    METHODS:
      test_01 FOR TESTING RAISING cx_static_check,
      test_02 FOR TESTING RAISING cx_static_check,
      test_03 FOR TESTING RAISING cx_static_check,
      test_04 FOR TESTING RAISING cx_static_check,
      test_05 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_fuel_counter IMPLEMENTATION.

  METHOD test_01.
    mt_masses = VALUE #( ( 12 ) ).
    DATA(lo_cut) = NEW lcl_fuel_counter( it_masses = mt_masses ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_cut->run( ) ).
  ENDMETHOD.
  METHOD test_02.
    mt_masses = VALUE #( ( 14 ) ).
    DATA(lo_cut) = NEW lcl_fuel_counter( it_masses = mt_masses ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_03.
    mt_masses = VALUE #( ( 1969 ) ).
    DATA(lo_cut) = NEW lcl_fuel_counter( it_masses = mt_masses ).
    cl_abap_unit_assert=>assert_equals( exp = 654 act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_04.
    mt_masses = VALUE #( ( 12 ) ( 14 ) ).
    DATA(lo_cut) = NEW lcl_fuel_counter( it_masses = mt_masses ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = lo_cut->run( ) ).
  ENDMETHOD.

  METHOD test_05.
    mt_masses = VALUE #( ( 12 ) ( 14 ) ( 1969 ) ).
    DATA(lo_cut) = NEW lcl_fuel_counter( it_masses = mt_masses ).
    cl_abap_unit_assert=>assert_equals( exp = 658 act = lo_cut->run( ) ).
  ENDMETHOD.

ENDCLASS.

DATA mass TYPE i.
SELECT-OPTIONS s_mass FOR mass.

START-OF-SELECTION.

  DATA lt_masses TYPE TABLE OF i WITH EMPTY KEY.

  lt_masses = VALUE #( FOR ls_mass IN s_mass ( ls_mass-low ) ).

  DATA(fuel_counter) = NEW lcl_fuel_counter( it_masses = lt_masses ).

  WRITE: fuel_counter->run( ).
