************************************************************************
* /MBTOOLS/CL_TOOL_BC_ICON
* MBT Icon Browser
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
CLASS /mbtools/cl_tool_bc_icon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /mbtools/if_manifest .

    CONSTANTS:
      BEGIN OF c_tool,
        version     TYPE string VALUE '1.0.0' ##NO_TEXT,
        title       TYPE string VALUE 'MBT Icon Browser' ##NO_TEXT,
        bundle_id   TYPE i VALUE 0,
        download_id TYPE i VALUE 4413,
        description TYPE string
        VALUE 'A Simple Tool to Query, Display, and Download Icons Available in SAP GUI' ##NO_TEXT,
      END OF c_tool.

    METHODS constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES mbt_manifest
      FOR /mbtools/if_manifest~descriptor .

    DATA mo_tool TYPE REF TO /mbtools/cl_tools .
ENDCLASS.



CLASS /MBTOOLS/CL_TOOL_BC_ICON IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_tool EXPORTING io_tool = me.
    mbt_manifest = mo_tool->mbt_manifest.
  ENDMETHOD.
ENDCLASS.
