class /MBTOOLS/CL_ICON_BROWSER definition
  public
  final
  create public .

************************************************************************
* MBT Icon Browser
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
public section.
  type-pools ICON .

  types:
    ty_class_range TYPE RANGE OF icon-i_class .
  types:
    ty_group_range TYPE RANGE OF icon-i_group .
  types:
    ty_icon_dir_range  TYPE RANGE OF icon-id .
  types:
    ty_name_range  TYPE RANGE OF icon-name .
  types:
    ty_text_range  TYPE RANGE OF icont-shorttext .
  types:
    BEGIN OF ty_icon_dir,
        id        TYPE icon_d,
        name      TYPE iconname,
        oleng     TYPE iconlength,
        button    TYPE icon_b,
        status    TYPE icon_s,
        message   TYPE icon_m,
        function  TYPE icon_f,
        textfield TYPE icon_t,
        internal  TYPE icon_int,
        locked    TYPE icon_l,
        i_class   TYPE icon_class,
        i_group   TYPE icon_group,
        i_member  TYPE icon_mem,
        shorttext TYPE icont-shorttext,
        quickinfo TYPE icont-quickinfo,
      END OF ty_icon_dir .

  methods INITIALIZE
    importing
      !IR_CLASSES type TY_CLASS_RANGE
      !IR_GROUPS type TY_GROUP_RANGE
      !IR_ICONS type TY_ICON_DIR_RANGE
      !IR_NAMES type TY_NAME_RANGE
      !IR_TEXTS type TY_TEXT_RANGE
      !IV_NAME type ABAP_BOOL
      !IV_ID type ABAP_BOOL
      !IV_TEXT type ABAP_BOOL
      !IV_DISP_N type ABAP_BOOL
      !IV_DISP_I type ABAP_BOOL
      !IV_DISP_P type ABAP_BOOL
    returning
      value(RV_RESULT) type I .
  methods PBO .
  methods PAI
    changing
      !CV_OK_CODE type SY-UCOMM .
  methods SCREEN .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_tree TYPE REF TO /mbtools/cl_tree .
    DATA:
      mt_icon_dir TYPE TABLE OF ty_icon_dir .
    DATA mr_classes TYPE ty_class_range .
    DATA mr_groups TYPE ty_group_range .
    DATA mr_icons TYPE ty_icon_dir_range .
    DATA mr_names TYPE ty_name_range .
    DATA mr_texts TYPE ty_text_range .
    DATA mv_name TYPE abap_bool .
    DATA mv_id TYPE abap_bool .
    DATA mv_text TYPE abap_bool .
    DATA mv_disp_n TYPE abap_bool .
    DATA mv_disp_i TYPE abap_bool .
    DATA mv_disp_p TYPE abap_bool .

    METHODS process_selection .
    METHODS process_main .
    METHODS process_class
      IMPORTING
        !is_class TYPE icon_cl
        !iv_level TYPE i .
    METHODS process_group
      IMPORTING
        !is_group TYPE icon_gr
        !iv_level TYPE i .
    METHODS process_icon
      IMPORTING
        !is_icon  TYPE ty_icon_dir
        !iv_level TYPE i .
    METHODS process_icon_properties
      IMPORTING
        !is_icon  TYPE ty_icon_dir
        !iv_level TYPE i .
ENDCLASS.



CLASS /MBTOOLS/CL_ICON_BROWSER IMPLEMENTATION.


  METHOD initialize.

    CREATE OBJECT mo_tree.

    mr_classes = ir_classes.
    mr_groups  = ir_groups.
    mr_icons   = ir_icons.
    mr_names   = ir_names.
    mr_texts   = ir_texts.
    mv_name    = iv_name.
    mv_id      = iv_id.
    mv_text    = iv_text.
    mv_disp_n  = iv_disp_n.
    mv_disp_i  = iv_disp_i.
    mv_disp_p  = iv_disp_p.

    process_selection( ).

    IF NOT mt_icon_dir IS INITIAL.
      process_main( ).
    ENDIF.

    rv_result = lines( mt_icon_dir ).

  ENDMETHOD.


  METHOD pai.

    mo_tree->pai( iv_ok_code = cv_ok_code ).

    CLEAR cv_ok_code.

  ENDMETHOD.


  METHOD pbo.

    SET PF-STATUS 'MAIN' OF PROGRAM sy-cprog.
    SET TITLEBAR  'MAIN' OF PROGRAM sy-cprog.

    mo_tree->display( ).

  ENDMETHOD.


  METHOD process_class.

    DATA:
      lo_level TYPE REF TO /mbtools/cl_tree_level,
      ls_group TYPE icon_gr.

    READ TABLE mt_icon_dir TRANSPORTING NO FIELDS
      WITH KEY i_class = is_class-id.
    CHECK sy-subrc = 0.

    CREATE OBJECT lo_level
      EXPORTING
        io_tree  = mo_tree
        iv_level = iv_level.

    mo_tree->add_detail(
      iv_icon  = icon_folder
      iv_title = 'Icon Class'
      iv_text  = is_class-name
      iv_value = is_class-id
      iv_level = lo_level->level ).

    lo_level->next( ).

    SELECT * FROM icon_gr INTO ls_group
      WHERE langu = sy-langu AND class = is_class-id
      ORDER BY PRIMARY KEY.

      process_group(
        is_group = ls_group
        iv_level = lo_level->level ).

    ENDSELECT.

    lo_level->back( ).

  ENDMETHOD.


  METHOD process_group.

    DATA:
      lo_level TYPE REF TO /mbtools/cl_tree_level,
      ls_icon  TYPE ty_icon_dir.

    READ TABLE mt_icon_dir TRANSPORTING NO FIELDS
      WITH KEY i_class = is_group-class i_group = is_group-id.
    CHECK sy-subrc = 0.

    CREATE OBJECT lo_level
      EXPORTING
        io_tree  = mo_tree
        iv_level = iv_level.

    mo_tree->add_detail(
      iv_icon  = icon_folder
      iv_title = 'Icon Group'
      iv_text  = is_group-name
      iv_value = is_group-id
      iv_level = lo_level->level ).

    lo_level->next( ).

    LOOP AT mt_icon_dir INTO ls_icon
      WHERE i_class = is_group-class AND i_group = is_group-id.

      process_icon(
        is_icon  = ls_icon
        iv_level = lo_level->level ).

    ENDLOOP.

    lo_level->back( ).

  ENDMETHOD.


  METHOD process_icon.

    DATA:
      lo_level TYPE REF TO /mbtools/cl_tree_level.

    CREATE OBJECT lo_level
      EXPORTING
        io_tree  = mo_tree
        iv_level = iv_level.

    IF mv_disp_n = abap_true.
      lo_level->value = is_icon-name.
    ELSEIF mv_disp_i = abap_true.
      lo_level->value = is_icon-id.
    ELSE.
      lo_level->value = ''.
    ENDIF.

    mo_tree->add_detail(
      iv_icon  = is_icon-id
      iv_title = 'Icon'
      iv_text  = is_icon-shorttext
      iv_value = lo_level->value
      iv_level = lo_level->level ).

    lo_level->next( ).

    process_icon_properties(
      is_icon  = is_icon
      iv_level = lo_level->level ).

    lo_level->back( ).

  ENDMETHOD.


  METHOD process_icon_properties.

    DATA:
      lo_level TYPE REF TO /mbtools/cl_tree_level.

    CHECK mv_disp_p = abap_true.

    CREATE OBJECT lo_level
      EXPORTING
        io_tree  = mo_tree
        iv_level = iv_level.

    mo_tree->add_detail(
      iv_title = 'Icon Name'
      iv_text  = is_icon-name
      iv_value = is_icon-name
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Icon ID'
      iv_text  = is_icon-id+1(2)
      iv_value = is_icon-id
      iv_level = lo_level->level ).

    IF is_icon-locked = abap_true.
      mo_tree->add_detail(
        iv_icon  = icon_locked
        iv_title = 'Locked'
        iv_level = lo_level->level ).
    ENDIF.

    lo_level->text = is_icon-internal.
    REPLACE ALL OCCURRENCES OF '@' IN lo_level->text WITH ''.

    mo_tree->add_detail(
      iv_title = 'Icon Internal Format'
      iv_text  = lo_level->text
      iv_value = is_icon-internal
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Quick Info'
      iv_text  = is_icon-quickinfo
      iv_value = is_icon-quickinfo
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Output Length'
      iv_text  = is_icon-oleng
      iv_value = is_icon-oleng
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Suitable for Pushbutton'
      iv_text  = is_icon-button
      iv_value = is_icon-button
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Suitable for Status Display'
      iv_text  = is_icon-status
      iv_value = is_icon-status
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Suitable for Message'
      iv_text  = is_icon-message
      iv_value = is_icon-message
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Suitable for Function Key'
      iv_text  = is_icon-function
      iv_value = is_icon-function
      iv_level = lo_level->level ).

    mo_tree->add_detail(
      iv_title = 'Suitable for Key Word'
      iv_text  = is_icon-textfield
      iv_value = is_icon-textfield
      iv_level = lo_level->level ).

  ENDMETHOD.


  METHOD process_main.

    DATA:
      ls_icon_cl TYPE icon_cl.

    " Add top node
    mo_tree->add_top_node(
      iv_icon  = icon_folder
      iv_title = 'SAP GUI Icons' ).

    " Process
    SELECT * FROM icon_cl INTO ls_icon_cl
      WHERE id IN mr_classes AND langu = sy-langu
      ORDER BY PRIMARY KEY.

      process_class(
        is_class = ls_icon_cl
        iv_level  = 1 ).

    ENDSELECT.

    " Expand complete tree
    mo_tree->expand_all( ).

  ENDMETHOD.


  METHOD process_selection.

    DATA:
      ls_icon_dir TYPE ty_icon_dir,
      ls_icon     TYPE icon,
      ls_icont    TYPE icont.

    CLEAR mt_icon_dir.

    SELECT * FROM icon INTO ls_icon
      WHERE i_class IN mr_classes
        AND i_group IN mr_groups
        AND id      IN mr_icons
        AND name    IN mr_names
      ORDER BY i_class i_group i_member.

      CLEAR ls_icon_dir.
      MOVE-CORRESPONDING ls_icon TO ls_icon_dir.

      SELECT SINGLE * FROM icont INTO ls_icont
        WHERE langu = sy-langu AND id = ls_icon-id.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_icont TO ls_icon_dir.
        IF NOT ls_icont-shorttext IN mr_texts.
          CONTINUE.
        ENDIF.
      ELSEIF NOT mr_texts IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND ls_icon_dir TO mt_icon_dir.

    ENDSELECT.

    IF mv_name = abap_true.
      SORT mt_icon_dir BY i_class i_group name.
    ELSEIF mv_id = abap_true.
      SORT mt_icon_dir BY i_class i_group id.
    ELSEIF mv_text = abap_true.
      SORT mt_icon_dir BY i_class i_group shorttext.
    ENDIF.

  ENDMETHOD.


  METHOD screen.

*   Place holder...

  ENDMETHOD.
ENDCLASS.
