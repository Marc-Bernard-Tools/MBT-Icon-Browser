REPORT /mbtools/icon_browser.
************************************************************************
* MBT Icon Browser
*
* This tool displays all SAP GUI icons in a tree control
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

TABLES:
  sscrfields, icon, icont.

INCLUDE /mbtools/icon_browser_cl.
INCLUDE /mbtools/icon_browser_gr.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) sc_t200,
      COMMENT /1(77) sc_t201,
  END OF BLOCK b200,
  BEGIN OF BLOCK b210 WITH FRAME.
SELECT-OPTIONS:
  s_class FOR icon-i_class,
  s_group FOR icon-i_group,
  s_icon  FOR icon-id,
  s_name  FOR icon-name,
  s_text  FOR icont-shorttext.
SELECTION-SCREEN:
  END OF BLOCK b210,
  BEGIN OF BLOCK b220 WITH FRAME.
PARAMETERS:
  p_name RADIOBUTTON GROUP g2 DEFAULT 'X',
  p_text RADIOBUTTON GROUP g2,
  p_id   RADIOBUTTON GROUP g2,
  p_orig RADIOBUTTON GROUP g2 ##NEEDED.
SELECTION-SCREEN:
  END OF BLOCK b220,
  BEGIN OF BLOCK b230 WITH FRAME.
PARAMETERS:
  p_disp_n RADIOBUTTON GROUP g3 DEFAULT 'X',
  p_disp_i RADIOBUTTON GROUP g3,
  p_disp_p RADIOBUTTON GROUP g3.
SELECTION-SCREEN:
    END OF BLOCK b230,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

* About
SELECTION-SCREEN:
  BEGIN OF SCREEN 900 AS SUBSCREEN,
    BEGIN OF BLOCK b900 WITH FRAME,
      COMMENT /1(50) sc_t900,
      COMMENT 60(25) sc_t901,
      SKIP,
      COMMENT /1(77) sc_t902,
    END OF BLOCK b900,
    BEGIN OF BLOCK b910 WITH FRAME,
      PUSHBUTTON /1(55) sc_docu USER-COMMAND docu,
      SKIP,
      PUSHBUTTON /1(55) sc_tool USER-COMMAND tool,
      SKIP,
      PUSHBUTTON /1(55) sc_lice USER-COMMAND lice,
      SKIP,
      PUSHBUTTON /1(55) sc_home USER-COMMAND home,
    END OF BLOCK b910,
  END OF SCREEN 900.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK sc_header,
    SKIP,
    SKIP,
    COMMENT /3(77) sc_t001 FOR FIELD s_class,
    SKIP,
  END OF BLOCK sc_header,
  BEGIN OF TABBED BLOCK sc_tab FOR 22 LINES,
    TAB (40) sc_tab2 USER-COMMAND sc_push2 DEFAULT SCREEN 200,
    TAB (40) sc_tab9 USER-COMMAND sc_push9 DEFAULT SCREEN 900,
  END OF BLOCK sc_tab.

*-----------------------------------------------------------------------

CONSTANTS
  c_title TYPE string VALUE /mbtools/cl_tool_bc_icon=>c_tool-title.

DATA:
  gv_ok_code TYPE sy-ucomm,
  gv_count   TYPE sy-tabix,
  go_tool    TYPE REF TO /mbtools/cl_tool,
  go_screen  TYPE REF TO /mbtools/cl_screen,
  go_app     TYPE REF TO /mbtools/cl_icon_browser.

*-----------------------------------------------------------------------

MODULE pbo_100 OUTPUT.

  go_screen->banner( abap_false ).

  go_app->pbo( ).

ENDMODULE.                 " PBO_0100  OUTPUT

MODULE pai_100 INPUT.

  go_app->pai( CHANGING cv_ok_code = gv_ok_code ).

ENDMODULE.                 " PAI_0100  INPUT

INITIALIZATION.

  IF /mbtools/cl_switches=>is_active( c_title ) = abap_false.
    MESSAGE e004(/mbtools/bc) WITH c_title.
    RETURN.
  ENDIF.

  CREATE OBJECT go_app.

  go_tool   = /mbtools/cl_tool_manager=>factory( c_title ).
  go_screen = /mbtools/cl_screen=>factory( c_title ).

  go_screen->init(
    IMPORTING
      ev_text      = sc_t001
      ev_about     = sc_tab9
      ev_title     = sc_t900
      ev_version   = sc_t901
      ev_copyright = sc_t902
      ev_docu      = sc_docu
      ev_tool      = sc_tool
      ev_home      = sc_home
      ev_lice      = sc_lice ).

  sc_tab2 = go_screen->header(
    iv_icon = icon_color
    iv_text = 'Icons' ).

  sc_t200 = 'Filter the selection of icons and set how you'.
  sc_t201 = 'want the results sorted and displayed'.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  go_app->screen( ).

  go_screen->ucomm( sscrfields-ucomm ).

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  go_screen->banner( ).

  go_app->screen( ).

*-----------------------------------------------------------------------

START-OF-SELECTION.

  DATA lv_msg TYPE string.

  LOG-POINT ID /mbtools/bc SUBKEY c_title FIELDS sy-datum sy-uzeit sy-uname.

  " Setup tree
  gv_count = go_app->initialize(
    ir_classes = s_class[]
    ir_groups  = s_group[]
    ir_icons   = s_icon[]
    ir_names   = s_name[]
    ir_texts   = s_text[]
    iv_name    = p_name
    iv_id      = p_id
    iv_text    = p_text
    iv_disp_n  = p_disp_n
    iv_disp_i  = p_disp_i
    iv_disp_p  = p_disp_p ).

  IF gv_count = 0.
    MESSAGE 'No icons found' TYPE 'S'.
  ELSE.
    lv_msg = |{ gv_count } icons found|.
    MESSAGE lv_msg TYPE 'S'.

    " Output as ALV tree control
    CALL SCREEN 100.
  ENDIF.
