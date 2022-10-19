CLASS /mbtools/cl_tool_bc_icon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Icon Browser
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.3.0' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Icon Browser' ##NO_TEXT,
        description  TYPE string
        VALUE 'Query, display, and download icons available in SAP GUI' ##NO_TEXT,
        bundle_id    TYPE i VALUE 1,
        download_id  TYPE i VALUE 4413,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'ICON',
        mbt_shortcut TYPE string VALUE 'IB',
      END OF c_tool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /mbtools/cl_tool_bc_icon IMPLEMENTATION.


  METHOD /mbtools/if_tool~install.
    RETURN.
  ENDMETHOD.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/ICON_BROWSER' ).
  ENDMETHOD.


  METHOD /mbtools/if_tool~title.
    rv_title = c_tool-title.
  ENDMETHOD.


  METHOD /mbtools/if_tool~tool.
    MOVE-CORRESPONDING c_tool TO rs_tool.
  ENDMETHOD.


  METHOD /mbtools/if_tool~uninstall.
    RETURN.
  ENDMETHOD.
ENDCLASS.
