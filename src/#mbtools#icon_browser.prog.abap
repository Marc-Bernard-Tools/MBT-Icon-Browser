************************************************************************
* /MBTOOLS/ICON_BROWSER
* MBT Icon Browser
*
* This tool lists all SAP GUI icons in a tree control
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
REPORT /mbtools/icon_browser.

TABLES:
  sscrfields, icon, icont.

*-----------------------------------------------------------------------

* Main
SELECTION-SCREEN:
  BEGIN OF SCREEN 200 AS SUBSCREEN,
    BEGIN OF BLOCK b200 WITH FRAME,
      COMMENT /1(77) scr_t200,
      COMMENT /1(77) scr_t201,
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
  p_name RADIOBUTTON GROUP g2,
  p_text RADIOBUTTON GROUP g2,
  p_id   RADIOBUTTON GROUP g2,
  p_orig RADIOBUTTON GROUP g2.
SELECTION-SCREEN:
  END OF BLOCK b220,
  BEGIN OF BLOCK b230 WITH FRAME.
PARAMETERS:
  p_disp_n RADIOBUTTON GROUP g3,
  p_disp_i RADIOBUTTON GROUP g3,
  p_disp_p RADIOBUTTON GROUP g3.
SELECTION-SCREEN:
    END OF BLOCK b230,
  END OF SCREEN 200.

*-----------------------------------------------------------------------

INCLUDE /mbtools/bc_screen_about_tab.

*-----------------------------------------------------------------------

* Header
SELECTION-SCREEN:
  BEGIN OF BLOCK scr_header,
    SKIP,
    SKIP,
    COMMENT /3(77) scr_t001 FOR FIELD s_class,
    SKIP,
  END OF BLOCK scr_header,
  BEGIN OF TABBED BLOCK scr_tab FOR 22 LINES,
    TAB (40) scr_tab2 USER-COMMAND scr_push2 DEFAULT SCREEN 0200,
    TAB (40) scr_tab9 USER-COMMAND scr_push9 DEFAULT SCREEN 0900,
  END OF BLOCK scr_tab.

*-----------------------------------------------------------------------

TYPES:
  BEGIN OF ty_icon.
    INCLUDE TYPE icon.
  TYPES:
    shorttext TYPE icont-shorttext,
    quickinfo TYPE icont-quickinfo,
  END OF ty_icon.

INCLUDE /mbtools/bc_screen_data.

DATA:
  go_app TYPE REF TO /mbtools/cl_bc_icon_browser.

*-----------------------------------------------------------------------

INCLUDE /mbtools/bc_screen_pbo_pai.

*-----------------------------------------------------------------------

INITIALIZATION.

  INCLUDE /mbtools/bc_screen_init.

  scr_tab2 = /mbtools/cl_screen=>header(
    iv_icon = icon_color
    iv_text = 'Icons' ).

  scr_t200 = 'Filter the selection of icons and set how you'.
  scr_t201 = 'want the results sorted and displayed'.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN.

  INCLUDE /mbtools/bc_screen_at_select.

*-----------------------------------------------------------------------

AT SELECTION-SCREEN OUTPUT.

  INCLUDE /mbtools/bc_screen_at_output.

*-----------------------------------------------------------------------

START-OF-SELECTION.

  LOG-POINT ID /mbtools/bc
    SUBKEY go_tool->get_title( )
    FIELDS sy-datum sy-uzeit sy-uname.

  " Setup tree
  go_app->initialize(
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

  " Output as ALV tree control
  CALL SCREEN 100.
