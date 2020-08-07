CLASS /mbtools/cl_tool_bc_icon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* MBT Icon Browser
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.
    INTERFACES /mbtools/if_manifest .

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Icon Browser' ##NO_TEXT,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 4413,
        description TYPE string
        VALUE 'Query, display, and download icons available in SAP GUI' ##NO_TEXT,
        has_launch  TYPE abap_bool VALUE abap_true,
      END OF c_tool.

    METHODS constructor .

    METHODS launch.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .

ENDCLASS.



CLASS /MBTOOLS/CL_TOOL_BC_ICON IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.


  METHOD launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/ICON_BROWSER' ).
  ENDMETHOD.
ENDCLASS.
