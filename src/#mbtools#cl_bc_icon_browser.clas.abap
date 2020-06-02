************************************************************************
* /MBTOOLS/CL_BC_ICON_BROWSER
* MBT Icon Browser
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_bc_icon_browser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apack_manifest .
    INTERFACES /mbtools/if_manifest .

    TYPES:
      ty_class_range TYPE RANGE OF icon-i_class .
    TYPES:
      ty_group_range TYPE RANGE OF icon-i_group .
    TYPES:
      ty_icon_range  TYPE RANGE OF icon-id .
    TYPES:
      ty_name_range  TYPE RANGE OF icon-name .
    TYPES:
      ty_text_range  TYPE RANGE OF icont-shorttext .

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NO_TEXT.
    CONSTANTS c_title TYPE string VALUE 'MBT Icon Browser' ##NO_TEXT.
    CONSTANTS c_description TYPE string VALUE 'A Simple Tool to Query and Display Icons Available in SAP GUI' ##NO_TEXT.
    CONSTANTS c_download_id TYPE i VALUE 4413 ##NO_TEXT.

    METHODS constructor .
    METHODS initialize
      IMPORTING
        !i_classes TYPE ty_class_range
        !i_groups  TYPE ty_group_range
        !i_icons   TYPE ty_icon_range
        !i_names   TYPE ty_name_range
        !i_texts   TYPE ty_text_range
        !i_name    TYPE abap_bool
        !i_id      TYPE abap_bool
        !i_text    TYPE abap_bool
        !i_disp_n  TYPE abap_bool
        !i_disp_i  TYPE abap_bool
        !i_disp_p  TYPE abap_bool .
    METHODS pbo .
    METHODS pai
      IMPORTING
        !i_ok_code TYPE sy-ucomm .
    METHODS screen .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES apack_manifest
      FOR if_apack_manifest~descriptor .
    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    TYPES:
      BEGIN OF ty_icon.
        INCLUDE TYPE icon.
      TYPES:
        shorttext TYPE icont-shorttext,
        quickinfo TYPE icont-quickinfo,
      END OF ty_icon .

    DATA mr_tool TYPE REF TO /mbtools/cl_tools.
    DATA mr_tree TYPE REF TO /mbtools/cl_tree .
    DATA:
      mt_icon TYPE TABLE OF ty_icon .
    DATA m_classes TYPE ty_class_range .
    DATA m_groups TYPE ty_group_range .
    DATA m_icons TYPE ty_icon_range .
    DATA m_names TYPE ty_name_range .
    DATA m_texts TYPE ty_text_range .
    DATA m_name TYPE abap_bool .
    DATA m_id TYPE abap_bool .
    DATA m_text TYPE abap_bool .
    DATA m_disp_n TYPE abap_bool .
    DATA m_disp_i TYPE abap_bool .
    DATA m_disp_p TYPE abap_bool .

    METHODS process_selection .
    METHODS process_main .
    METHODS process_class
      IMPORTING
        !is_class TYPE icon_cl
        !i_level  TYPE i .
    METHODS process_group
      IMPORTING
        !is_group TYPE icon_gr
        !i_level  TYPE i .
    METHODS process_icon
      IMPORTING
        !is_icon TYPE ty_icon
        !i_level TYPE i .
    METHODS process_icon_properties
      IMPORTING
        !is_icon TYPE ty_icon
        !i_level TYPE i .
ENDCLASS.



CLASS /MBTOOLS/CL_BC_ICON_BROWSER IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mr_tool EXPORTING i_tool = me.

    apack_manifest = mr_tool->apack_manifest.
    mbt_manifest   = mr_tool->mbt_manifest.
  ENDMETHOD.


  METHOD initialize.

    CREATE OBJECT mr_tree.

    m_classes = i_classes.
    m_groups  = i_groups.
    m_icons   = i_icons.
    m_names   = i_names.
    m_texts   = i_texts.
    m_name    = i_name.
    m_id      = i_id.
    m_text    = i_text.
    m_disp_n  = i_disp_n.
    m_disp_i  = i_disp_i.
    m_disp_p  = i_disp_p.

    process_selection( ).

    process_main( ).

  ENDMETHOD.


  METHOD pai.
    mr_tree->pai( i_ok_code = i_ok_code ).
  ENDMETHOD.


  METHOD pbo.
    mr_tree->display( ).
  ENDMETHOD.


  METHOD process_class.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level,
      ls_group TYPE icon_gr.

    READ TABLE mt_icon TRANSPORTING NO FIELDS
      WITH KEY i_class = is_class-id.
    CHECK sy-subrc = 0.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    mr_tree->add_detail(
      i_icon  = icon_folder
      i_title = 'Icon Class'
      i_text  = is_class-name
      i_value = is_class-id
      i_level = lr_level->level ).

    lr_level->next( ).

    SELECT * FROM icon_gr INTO ls_group
      WHERE langu = sy-langu AND class = is_class-id
      ORDER BY PRIMARY KEY.

      process_group(
        is_group = ls_group
        i_level = lr_level->level ).

    ENDSELECT.

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_group.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level,
      ls_icon  TYPE ty_icon.

    READ TABLE mt_icon TRANSPORTING NO FIELDS
      WITH KEY i_class = is_group-class i_group = is_group-id.
    CHECK sy-subrc = 0.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    mr_tree->add_detail(
      i_icon  = icon_folder
      i_title = 'Icon Group'
      i_text  = is_group-name
      i_value = is_group-id
      i_level = lr_level->level ).

    lr_level->next( ).

    LOOP AT mt_icon INTO ls_icon
      WHERE i_class = is_group-class AND i_group = is_group-id.

      process_icon(
        is_icon = ls_icon
        i_level = lr_level->level ).

    ENDLOOP.

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_icon.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    IF m_disp_n = abap_true.
      lr_level->value = is_icon-name.
    ELSEIF m_disp_i = abap_true.
      lr_level->value = is_icon-id.
    ELSE.
      lr_level->value = ''.
    ENDIF.

    mr_tree->add_detail(
      i_icon  = is_icon-id
      i_title = 'Icon'
      i_text  = is_icon-shorttext
      i_value = lr_level->value
      i_level = lr_level->level ).

    lr_level->next( ).

    process_icon_properties(
      is_icon = is_icon
      i_level = lr_level->level ).

    lr_level->back( ).

  ENDMETHOD.


  METHOD process_icon_properties.

    DATA:
      lr_level TYPE REF TO /mbtools/cl_tree_level.

    CHECK m_disp_p = abap_true.

    CREATE OBJECT lr_level EXPORTING ir_app = mr_tree.

    mr_tree->add_detail(
      i_title = 'Icon Name'
      i_text  = is_icon-name
      i_value = is_icon-name
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Icon ID'
      i_text  = is_icon-id+1(2)
      i_value = is_icon-id
      i_level = lr_level->level ).

    IF is_icon-locked = abap_true.
      mr_tree->add_detail(
        i_icon  = icon_locked
        i_title = 'Locked'
        i_level = lr_level->level ).
    ENDIF.

    lr_level->text = is_icon-internal.
    REPLACE ALL OCCURRENCES OF '@' IN lr_level->text WITH ''.

    mr_tree->add_detail(
      i_title = 'Icon Internal Format'
      i_text  = lr_level->text
      i_value = is_icon-internal
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Quick Info'
      i_text  = is_icon-quickinfo
      i_value = is_icon-quickinfo
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Output Length'
      i_text  = is_icon-oleng
      i_value = is_icon-oleng
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Suitable for Pushbutton'
      i_text  = is_icon-button
      i_value = is_icon-button
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Suitable for Status Display'
      i_text  = is_icon-status
      i_value = is_icon-status
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Suitable for Message'
      i_text  = is_icon-message
      i_value = is_icon-message
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Suitable for Function Key'
      i_text  = is_icon-function
      i_value = is_icon-function
      i_level = lr_level->level ).

    mr_tree->add_detail(
      i_title = 'Suitable for Key Word'
      i_text  = is_icon-textfield
      i_value = is_icon-textfield
      i_level = lr_level->level ).

  ENDMETHOD.


  METHOD process_main.

    DATA:
      ls_icon_cl TYPE icon_cl.

    " Add top node
    CALL METHOD mr_tree->add_top_node
      EXPORTING
        i_icon  = icon_folder
        i_title = 'SAP GUI Icons'.

    " Process
    SELECT * FROM icon_cl INTO ls_icon_cl
      WHERE id IN m_classes AND langu = sy-langu
      ORDER BY PRIMARY KEY.

      process_class(
        is_class = ls_icon_cl
        i_level  = 0 ).

    ENDSELECT.

    " Expand complete tree
    mr_tree->expand_all( ).

  ENDMETHOD.


  METHOD process_selection.

    DATA:
      ls_icon_sel TYPE ty_icon,
      ls_icon     TYPE icon,
      ls_icont    TYPE icont.

    CLEAR mt_icon.

    SELECT * FROM icon INTO ls_icon
      WHERE i_class IN m_classes
        AND i_group IN m_groups
        AND id      IN m_icons
        AND name    IN m_names
      ORDER BY i_class i_group i_member.

      CLEAR ls_icon_sel.
      MOVE-CORRESPONDING ls_icon TO ls_icon_sel.

      SELECT SINGLE * FROM icont INTO ls_icont
        WHERE langu = sy-langu AND id = ls_icon-id.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_icont TO ls_icon_sel.
        IF NOT ls_icont-shorttext IN m_texts.
          CONTINUE.
        ENDIF.
      ELSEIF NOT m_texts IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND ls_icon_sel TO mt_icon.

    ENDSELECT.

    IF m_name = abap_true.
      SORT mt_icon BY i_class i_group name.
    ELSEIF m_id = abap_true.
      SORT mt_icon BY i_class i_group id.
    ELSEIF m_text = abap_true.
      SORT mt_icon BY i_class i_group shorttext.
    ENDIF.

  ENDMETHOD.


  METHOD screen.

*   Place holder...

*    DATA l_show TYPE abap_bool.
*
*    LOOP AT SCREEN.
*      l_show = abap_true.
*
*      IF l_show = abap_true.
*        screen-active = '1'.
*        screen-input = '1'.
*      ELSE.
*        screen-active = '0'.
*        screen-input = '0'.
*      ENDIF.
*
*      MODIFY SCREEN.
*    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
