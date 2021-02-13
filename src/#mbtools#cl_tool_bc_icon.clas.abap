CLASS /mbtools/cl_tool_bc_icon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* MBT Icon Browser
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

  PUBLIC SECTION.

    INTERFACES /mbtools/if_tool.

    CONSTANTS:
      BEGIN OF c_tool,
        version      TYPE string VALUE '1.0.0' ##NO_TEXT,
        title        TYPE string VALUE 'MBT Icon Browser' ##NO_TEXT,
        bundle_id    TYPE i VALUE 0,
        download_id  TYPE i VALUE 4413,
        description  TYPE string
        VALUE 'Query, display, and download icons available in SAP GUI' ##NO_TEXT,
        has_launch   TYPE abap_bool VALUE abap_true,
        mbt_command  TYPE string VALUE 'ICON',
        mbt_shortcut TYPE string VALUE 'IB',
      END OF c_tool.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_tool TYPE REF TO /mbtools/cl_tools.

ENDCLASS.



CLASS /mbtools/cl_tool_bc_icon IMPLEMENTATION.


  METHOD /mbtools/if_tool~launch.
    /mbtools/cl_sap=>run_program( '/MBTOOLS/ICON_BROWSER' ).
  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    /mbtools/if_tool~ms_manifest = mo_tool->ms_manifest.
  ENDMETHOD.
ENDCLASS.
