class ZCL_MDG_BANK_DATA_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_RULE_SERVICE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MDG_BANK_DATA_CHECK IMPLEMENTATION.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_FINAL.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_START.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_FINAL.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_START.
  endmethod.


  METHOD if_ex_usmd_rule_service~check_entity.

    DATA: lv_crequest_type TYPE usmd_crequest_type,
          lv_crequest_wfs  TYPE usmd_crequest_wfs,
          lv_crequest_id   TYPE usmd_crequest.

    CALL METHOD cl_usmd_app_context=>get_context
      RECEIVING
        eo_context = DATA(lo_context).
    IF lo_context IS NOT INITIAL.               " Application Context
      lo_context->get_attributes(
          IMPORTING
          ev_crequest_id    = lv_crequest_id                 " Change Request
          ev_crequest_type  = lv_crequest_type                 " Type of Change Request
          ev_crequest_step  = lv_crequest_wfs                 " Workflow Step Number
      ).

      io_model->read_entity_data_all(
        EXPORTING
          i_fieldname      = 'BNKA'                " Financial MDM: Field Name
          if_active        = abap_false                " Financial MDM: General Indicator
          i_crequest       = id_crequest                " Change Request
*    it_sel           =                  " Sorted Table: Selection Condition (Range per Field)
*    it_entity_filter =                  " Ent.Types for Which Data Is Expected; Default: All Ent.Types
        IMPORTING
*    et_message       =                  " Messages
          et_data_entity   =  DATA(lt_data)              " Data for Entity Types
      ).



    ENDIF.
  ENDMETHOD.


  method IF_EX_USMD_RULE_SERVICE~CHECK_ENTITY_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~DERIVE_ENTITY.
  endmethod.
ENDCLASS.
