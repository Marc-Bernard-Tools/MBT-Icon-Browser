************************************************************************
* /MBTOOLS/BC_ICON_BROWSER
* MBT Icon Browser
*
* This tool lists all SAP GUI icons in a tree control
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

REPORT /mbtools/bc_icon_browser.

TABLES:
  sscrfields, icon, icont, icon_cl, icon_gr.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) scr_t200,
      COMMENT /1(77) scr_t201,
      SKIP.
SELECT-OPTIONS:
  s_class FOR icon-i_class,
  s_group FOR icon-i_group,
  s_icon  FOR icon-id,
  s_name  FOR icon-name.
SELECTION-SCREEN:
    END OF BLOCK b200,
    BEGIN OF BLOCK b210 WITH FRAME.
PARAMETERS:
  p_name RADIOBUTTON GROUP g2,
  p_text RADIOBUTTON GROUP g2,
  p_id   RADIOBUTTON GROUP g2.
SELECTION-SCREEN:
    END OF BLOCK b210,
    BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_disp_n RADIOBUTTON GROUP g3,
  p_disp_i RADIOBUTTON GROUP g3.
SELECTION-SCREEN:
    END OF BLOCK b220,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) scr_t900,
      COMMENT 60(25) scr_t902,
      COMMENT /1(77) scr_t901,
      SKIP,
      COMMENT /1(77) scr_t903,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) b_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) b_lice USER-COMMAND lice,
      SKIP,
      PUSHBUTTON /1(55) b_repo USER-COMMAND repo,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header WITH FRAME,
    COMMENT /1(77) scr_t000,
    COMMENT /1(77) scr_t001,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 20 LINES,
    TAB (40) scr_tab2 USER-COMMAND scr_push2
      DEFAULT SCREEN 0200,
    TAB (40) scr_tab9 USER-COMMAND scr_push9
      DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.

DATA:
  gr_tree TYPE REF TO /mbtools/cl_tree.

*&---------------------------------------------------------------------*

INCLUDE /mbtools/bc_tree_t01.    " Control Definitions

INCLUDE /mbtools/bc_tree_t02.    " Control PBO and PAI

*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
FORM modify_screen.

  DATA l_show TYPE abap_bool.

  LOOP AT SCREEN.
    l_show = abap_true.

    IF l_show = abap_true.
      screen-active = '1'.
      screen-input = '1'.
    ELSE.
      screen-active = '0'.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    "modify_screen

*&---------------------------------------------------------------------*
*&      Form  process_class
*&---------------------------------------------------------------------*
FORM process_class USING
  VALUE(is_class) TYPE icon_cl
  VALUE(i_level)  TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon  = icon_folder
      i_title = 'Icon Class'
      i_text  = is_class-name
      i_value = is_class-id
      i_level = lr_level->level.

  lr_level->next( ).

  SELECT * FROM icon_gr
    WHERE langu = sy-langu AND class = is_class-id AND id IN s_group
    ORDER BY PRIMARY KEY.

    PERFORM process_group USING icon_gr lr_level->level.

  ENDSELECT.

  lr_level->back( ).

ENDFORM.                    "process_class

*&---------------------------------------------------------------------*
*&      Form  process_group
*&---------------------------------------------------------------------*
FORM process_group USING
  VALUE(is_group) TYPE icon_gr
  VALUE(i_level)  TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level,
    ls_icon  TYPE icon,
    ls_icont TYPE icont,
    ls_iconv TYPE iconv,
    lt_iconv TYPE TABLE OF iconv.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon  = icon_folder
      i_title = 'Icon Group'
      i_text  = icon_gr-name
      i_value = icon_gr-id
      i_level = lr_level->level.

  lr_level->next( ).

  SELECT * FROM icon INTO ls_icon
    WHERE i_class = is_group-class AND i_group = is_group-id AND id IN s_icon.

    MOVE-CORRESPONDING ls_icon TO ls_iconv.

    SELECT SINGLE * FROM icont INTO ls_icont
      WHERE langu = sy-langu AND id = ls_icon-id.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_icont TO ls_iconv.
    ENDIF.
    APPEND ls_iconv TO lt_iconv.

  ENDSELECT.

  IF p_name = abap_true.
    SORT lt_iconv BY name.
  ELSEIF p_id = abap_true.
    SORT lt_iconv BY id.
  ELSE.
    SORT lt_iconv BY shorttext.
  ENDIF.

  LOOP AT lt_iconv INTO ls_iconv.

    PERFORM process_icon USING ls_iconv lr_level->level.

  ENDLOOP.

  lr_level->back( ).

ENDFORM.                    "process_group

*&---------------------------------------------------------------------*
*&      Form  process_icon
*&---------------------------------------------------------------------*
FORM process_icon USING
  VALUE(is_iconv)  TYPE iconv
  VALUE(i_level)   TYPE i.

  DATA:
    lr_level TYPE REF TO /mbtools/cl_tree_level.

  CREATE OBJECT lr_level EXPORTING ir_app = gr_tree.

  IF p_disp_n = abap_true.
    lr_level->value = is_iconv-name.
  ELSE.
    lr_level->value = is_iconv-id.
  ENDIF.

  CALL METHOD gr_tree->add_detail
    EXPORTING
      i_icon  = is_iconv-id
      i_title = 'Icon'
      i_text  = is_iconv-shorttext
      i_value = lr_level->value
      i_level = lr_level->level.

ENDFORM.                    "process_icon

*&---------------------------------------------------------------------*

INITIALIZATION.

* Header
  scr_t000 =
  'Welcome to the world of abapGit! With a few clicks, this program'.
  scr_t001 =
  'will install, setup, and keep abapGit up-to-date in your system.'.

*-----------------------------------------------------------------------

* Start
  scr_tab2 = /mbtools/cl_screen=>header(
    iv_icon = icon_initial
    iv_text = 'Initial Installation' ).

  scr_t200 =
  'We have some questions about your installation.'.
  scr_t201 =
  'Are you ready?'.

*-----------------------------------------------------------------------

* About
  scr_tab9 = /mbtools/cl_screen=>header(
    iv_icon = icon_system_help
    iv_text = 'About' ).

  scr_t900 = /mbtools/cl_bc_icon_browser=>c_title.
  scr_t901 = /mbtools/cl_bc_icon_browser=>c_description.
  scr_t902 = | Version { /mbtools/cl_bc_icon_browser=>c_version } |.
  scr_t903 = /mbtools/cl_screen=>gv_copyright.

  b_docu = /mbtools/cl_screen=>icon(
    iv_icon = icon_system_extended_help
    iv_text = /mbtools/cl_screen=>gv_documentation ).
  b_lice = /mbtools/cl_screen=>icon(
    iv_icon = icon_legal_reg
    iv_text = /mbtools/cl_screen=>gv_terms ).
  b_repo = /mbtools/cl_screen=>icon(
    iv_icon = icon_url
    iv_text = /mbtools/cl_tools=>c_title ).

*-----------------------------------------------------------------------

* Adjust screen to release
  PERFORM modify_screen.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  PERFORM modify_screen.

  CHECK sy-dynnr <> '1000'.

  CASE sscrfields.

*   About
    WHEN 'DOCU'.
      /mbtools/cl_utilities=>call_browser( 'https://docs.abapgit.org/' ).

    WHEN 'REPO'.
      /mbtools/cl_utilities=>call_browser( 'https://www.abapgit.org/' ).

  ENDCASE.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  PERFORM modify_screen.

*&---------------------------------------------------------------------*

START-OF-SELECTION.

  CREATE OBJECT gr_tree.

* Add top node
  CALL METHOD gr_tree->add_top_node
    EXPORTING
      i_icon  = icon_folder
      i_title = 'SAP GUI Icons'.

* Process sub nodes
  SELECT * FROM icon_cl
    WHERE id IN s_class AND langu = sy-langu.

    PERFORM process_class USING icon_cl 0.

  ENDSELECT.

* Expand complete tree
  gr_tree->expand_all( ).

* Output as ALV tree control
  CALL SCREEN 100.
