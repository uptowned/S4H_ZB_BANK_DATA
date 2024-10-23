class ZCL_BANK_DATA_ACCESS_CLAS definition
  public
  final
  create public .

public section.

  interfaces IF_USMD_PP_ACCESS .
  interfaces IF_USMD_PP_HANA_SEARCH .
  interfaces IF_USMD_PP_BLOCKLIST .

  types:
    gty_t_bnka TYPE TABLE OF zbb_s_zb_pp_bnka .

  data GV_BNKA_KEY type BANKK .
  data GV_CREQUEST_ID type USMD_CREQUEST .
  data GT_BNKA type GTY_T_BNKA .
  data GO_SO_GOV_API type ref to IF_USMD_CONV_SOM_GOV_API .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BANK_DATA_ACCESS_CLAS IMPLEMENTATION.


  method IF_USMD_PP_ACCESS~ADJUST_SELECTION_ATTR.
  endmethod.


  method IF_USMD_PP_ACCESS~CHECK_AUTHORITY.
  endmethod.


  method IF_USMD_PP_ACCESS~CHECK_AUTHORITY_MASS.
  endmethod.


  method IF_USMD_PP_ACCESS~CHECK_DATA.
  endmethod.


  method IF_USMD_PP_ACCESS~CHECK_EXISTENCE_MASS.
  endmethod.


  METHOD if_usmd_pp_access~dequeue.

*    DATA lt_usmd_gov_api_ts_ent_tabl TYPE usmd_gov_api_ts_ent_tabl.
*    DATA ls_usmd_gov_api_s_ent_tabl TYPE usmd_gov_api_s_ent_tabl.
*    DATA lr_data TYPE REF TO data.
*
*    TRY.
**        gr_gov_api->save( i_mode = if_usmd_ui_services=>gc_save_mode_draft_no_check ).
*        go_so_gov_api->if_usmd_conv_som_gov_trans~save(
*        EXPORTING
*          if_include_check_in_save_only = abap_false
*          ).
*
**        gr_gov_api->dequeue_entity( EXPORTING iv_crequest_id = gv_crequest_id
**                                              iv_entity_name = if_mdg_bs_mat_gen_c=>gc_entity_material
**                                              it_data        = gt_bnka ).
*
*        CLEAR ls_usmd_gov_api_s_ent_tabl.
*        CLEAR lt_usmd_gov_api_ts_ent_tabl.
*
*        CREATE DATA lr_data TYPE zbb_s_zb_pp_bnka.
*        ASSIGN lr_data->* TO FIELD-SYMBOL(<lt_data>).
*        MOVE-CORRESPONDING it_value TO <lt_data>.
*
*        ls_usmd_gov_api_s_ent_tabl-entity = i_entity.
*        ls_usmd_gov_api_s_ent_tabl-tabl = lr_data.
*
*        APPEND ls_usmd_gov_api_s_ent_tabl TO lt_usmd_gov_api_ts_ent_tabl.
*
*        go_so_gov_api->if_usmd_conv_som_gov_entity~dequeue_entity( it_entity_keys = lt_usmd_gov_api_ts_ent_tabl ).
**CATCH cx_usmd_gov_api. " CX_USMD_CORE_DYNAMIC_CHECK
*
*
**        gr_gov_api->dequeue_crequest( EXPORTING iv_crequest_id = gv_crequest_id ).
*        go_so_gov_api->if_usmd_conv_som_gov_cr~dequeue_crequest( ).
**        gr_gov_api->if_usmd_gov_api_process~start_workflow( EXPORTING iv_crequest_id = gv_crequest_id ).
**        COMMIT WORK AND WAIT.
*
*      CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api INTO DATA(lo_err_msg).
**        gr_gov_api->refresh_buffers( ).
*        go_so_gov_api->if_usmd_conv_som_gov_trans~refresh_buffers(
*        EXPORTING
*          if_keep_environment = abap_true
*          ).
**        APPEND LINES OF zcl_vg_plm_helper=>set_bapi_messages( lr_gov_api->get_messages( ) ) TO et_message.
*        TRY.
*            IF gt_bnka IS NOT INITIAL.
**              gr_gov_api->dequeue_entity( EXPORTING iv_crequest_id = gv_crequest_id
**                                                    iv_entity_name = if_mdg_bs_mat_gen_c=>gc_entity_material
**                                                    it_data        = gt_bnka ).
*              go_so_gov_api->if_usmd_conv_som_gov_entity~dequeue_entity( it_entity_keys = lt_usmd_gov_api_ts_ent_tabl ).
**              CATCH cx_usmd_gov_api. " CX_USMD_CORE_DYNAMIC_CHECK
*
*            ENDIF.
*            IF go_so_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
**            IF gr_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
**              gr_gov_api->dequeue_crequest( iv_crequest_id = gv_crequest_id ).
*              go_so_gov_api->if_usmd_conv_som_gov_cr~dequeue_crequest( ).
*            ENDIF.
*          CATCH cx_usmd_gov_api.
**            APPEND LINES OF zcl_vg_plm_helper=>set_bapi_messages( lr_gov_api->get_messages( ) ) TO et_message.
*            RETURN.
*        ENDTRY.
*    ENDTRY.
  ENDMETHOD.


  method IF_USMD_PP_ACCESS~DERIVE_DATA.
  endmethod.


  method IF_USMD_PP_ACCESS~DERIVE_DATA_ON_KEY_CHANGE.
  endmethod.


  method IF_USMD_PP_ACCESS~DISCARD_READ_BUFFER.
  endmethod.


  METHOD if_usmd_pp_access~enqueue.

*    DATA: lo_app_context TYPE REF TO if_usmd_app_context.
*    DATA lt_usmd_gov_api_ts_ent_tabl TYPE usmd_gov_api_ts_ent_tabl.
*    DATA ls_usmd_gov_api_s_ent_tabl TYPE usmd_gov_api_s_ent_tabl.
*    DATA lr_data TYPE REF TO data.
*
*    lo_app_context = cl_usmd_app_context=>get_context( ).
*
*    lo_app_context->get_attributes(
*      IMPORTING
*        ev_crequest_id    = gv_crequest_id                  " Change Request
*    ).
*
*    TRY.
*        IF go_so_gov_api IS NOT BOUND.
**        IF gr_gov_api IS NOT BOUND.
*
**          gr_gov_api = cl_usmd_conv_som_gov_api=>get_instance( iv_model_name = 'ZB' ).
*          cl_usmd_conv_som_gov_api=>get_instance(
*            EXPORTING
*              iv_model_name = 'ZB'                           " Data Model
*              iv_classname  = 'CL_USMD_CONV_SOM_GOV_API' " Object Type Name
*            RECEIVING
*              ro_so_gov_api = go_so_gov_api                           " Governance API: Single Chg. Req. Convenience Governance API
*          ).
*
*
*        ENDIF.
*      CATCH cx_usmd_conv_som_gov_api.       " CX_USMD_GOV_API
*      CATCH cx_usmd_app_context_cons_error. " Exception: Consistency Error in Design of Appl. Context
*      CATCH cx_usmd_gov_api.                " General Processing Error GOV_API
*
*    ENDTRY.
*
*
*    TRY.
*
**        gr_gov_api->refresh_buffers( ).
******************        go_so_gov_api->if_usmd_conv_som_gov_trans~refresh_buffers(
******************        EXPORTING
******************          if_keep_environment = abap_true
******************          ).
**        DATA(lv_new_crequest_id) = lr_gov_api->create_crequest( EXPORTING iv_crequest_type = iv_ctype
**                                                                          iv_description   = lv_description ).
**        gr_gov_api->enqueue_crequest( EXPORTING iv_crequest_id = gv_crequest_id ).
*        go_so_gov_api->if_usmd_conv_som_gov_cr~enqueue_crequest( iv_lock_mode = i_lock_type ).
*
**        gr_gov_api->refresh_buffers( ).
******************        go_so_gov_api->if_usmd_conv_som_gov_trans~refresh_buffers(
******************        EXPORTING
******************          if_keep_environment = abap_true
******************          ).
**        APPEND LINES OF zcl_vg_plm_helper=>set_bapi_messages( lr_gov_api->get_messages( ) ) TO et_message.
**        IF gr_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
*        IF go_so_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
**          gr_gov_api->dequeue_crequest( iv_crequest_id = gv_crequest_id ).
*          go_so_gov_api->if_usmd_conv_som_gov_cr~dequeue_crequest( ).
*
*        ENDIF.
*      CATCH cx_usmd_gov_api_core_error. " CX_USMD_CORE_DYNAMIC_CHECK
*      CATCH cx_usmd_gov_api.            " General Processing Error GOV_API
*
*
*        RETURN.
*    ENDTRY.
*
*    TRY.
**        "Enqueue BANK Entity
**        gr_gov_api->create_data_reference( EXPORTING iv_entity_name = 'BNKA'
**                                                     iv_struct      = gr_gov_api->gc_struct_key
**                                           IMPORTING er_structure   = DATA(lr_bnka_key_str)
**                                                     er_table       = DATA(lr_bnka_key_tab) ).
**        ASSIGN lr_bnka_key_str->* TO FIELD-SYMBOL(<ls_bnka_key>).
**        FIELD-SYMBOLS <lt_bnka_key> TYPE ANY TABLE.
**        ASSIGN lr_bnka_key_tab->* TO <lt_bnka_key>.
**        ASSIGN COMPONENT 'BNKA' OF STRUCTURE <ls_bnka_key> TO FIELD-SYMBOL(<lv_value>).
**        IF sy-subrc EQ 0.
**          IF gv_bnka_key IS NOT INITIAL.
**            <lv_value> = gv_bnka_key.
**          ELSE.
**            RETURN.
**          ENDIF.
**        ENDIF.
**        INSERT <ls_bnka_key> INTO TABLE <lt_bnka_key>.
*
*
*        CLEAR gt_bnka .
*        MOVE-CORRESPONDING it_value TO gt_bnka.
*
*        CLEAR ls_usmd_gov_api_s_ent_tabl.
*        CLEAR lt_usmd_gov_api_ts_ent_tabl.
*
*        CREATE DATA lr_data TYPE TABLE OF ZBB_S_ZB_KF_BNKA.
*        ASSIGN lr_data->* TO FIELD-SYMBOL(<lt_data>).
*        MOVE-CORRESPONDING it_value TO <lt_data>.
*
*        ls_usmd_gov_api_s_ent_tabl-entity = i_entity.
*        ls_usmd_gov_api_s_ent_tabl-tabl = lr_data.
*
*        APPEND ls_usmd_gov_api_s_ent_tabl TO lt_usmd_gov_api_ts_ent_tabl.
*
*
**        gr_gov_api->enqueue_entity( EXPORTING iv_crequest_id = gv_crequest_id
**                                                iv_entity_name = 'BNKA'
**                                                it_data        = gt_bnka
***                                                it_data        = <lt_bnka_key>
**                                                ).
*********************        go_so_gov_api->if_usmd_conv_som_gov_entity~enqueue_entity(
*********************          it_entity_keys = lt_usmd_gov_api_ts_ent_tabl                 " MDG: Table Type for Table of Entities Structure
*********************          iv_lock_mode   = i_lock_type              " Lock Mode
*********************        ).
*
*      CATCH cx_usmd_gov_api_entity_lock cx_usmd_gov_api cx_usmd_cr_root_no_cr_typ_4_cr  INTO DATA(lo_err_msg).
**        gr_gov_api->refresh_buffers( ).
*        go_so_gov_api->if_usmd_conv_som_gov_trans~refresh_buffers(
*        EXPORTING
*          if_keep_environment = abap_true
*          ).
**        APPEND LINES OF zcl_vg_plm_helper=>set_bapi_messages( lr_gov_api->get_messages( ) ) TO et_message.
**        IF gr_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
*        IF go_so_gov_api IS BOUND AND gv_crequest_id IS NOT INITIAL.
**          gr_gov_api->dequeue_crequest( iv_crequest_id = gv_crequest_id ).
*          go_so_gov_api->if_usmd_conv_som_gov_cr~dequeue_crequest( ).
*        ENDIF.
*        RETURN.
*    ENDTRY.
*    TRY.
**        lo_gov_api->save( ).
******************        go_so_gov_api->if_usmd_conv_som_gov_trans~save(
******************        EXPORTING
******************          if_include_check_in_save_only = abap_false
******************          ).
*        "Save is done in draft mode by default so it is possible to
*        "save the change request even if change request data or
*        "entity data is not consistent.
*      CATCH cx_usmd_gov_api_core_error.
*        EXIT.
*        "Adequate Exception handling
*    ENDTRY.

  ENDMETHOD.


  method IF_USMD_PP_ACCESS~GET_CHANGE_DOCUMENT.
  endmethod.


  method IF_USMD_PP_ACCESS~GET_ENTITY_PROPERTIES.
  endmethod.


  method IF_USMD_PP_ACCESS~GET_FIELD_PROPERTIES.
  endmethod.


  method IF_USMD_PP_ACCESS~GET_KEY_HANDLING.
  endmethod.


  method IF_USMD_PP_ACCESS~GET_MAPPING_CD.
  endmethod.


  method IF_USMD_PP_ACCESS~GET_QUERY_PROPERTIES.
  endmethod.


  METHOD if_usmd_pp_access~query.
    DATA:ls_sel        TYPE usmd_s_sel,
         lrt_sel_banks TYPE RANGE OF banks,
         lrt_sel_bnka  TYPE RANGE OF bankk,
         lrs_sel_banks LIKE LINE OF lrt_sel_banks,
         lrs_sel_bnka  LIKE LINE OF lrt_sel_bnka,
         ls_pp_bnka    TYPE zbb_s_zb_pp_bnka,
         lr_data       TYPE REF TO data,
         lt_bnka       TYPE STANDARD TABLE OF bnka.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    CLEAR:   et_data, et_message, ef_not_supported.

    ASSERT i_entity EQ 'BNKA' .

    LOOP AT it_sel INTO ls_sel WHERE fieldname EQ 'BANKS'.
      MOVE-CORRESPONDING ls_sel TO lrs_sel_banks.
      APPEND lrs_sel_banks TO lrt_sel_banks.
    ENDLOOP.

    LOOP AT it_sel INTO ls_sel WHERE fieldname EQ 'BNKA'.
      MOVE-CORRESPONDING ls_sel TO lrs_sel_bnka.
      APPEND lrs_sel_bnka TO lrt_sel_bnka.
    ENDLOOP.

    IF lrt_sel_banks IS NOT INITIAL OR
       lrt_sel_bnka IS NOT INITIAL.
* Select data
      SELECT * FROM bnka UP TO i_num_entries ROWS INTO TABLE lt_bnka
        WHERE banks     IN lrt_sel_banks AND
              bankl     IN lrt_sel_bnka.
*      RETURN.
    ELSE.
      SELECT * FROM bnka UP TO i_num_entries ROWS INTO TABLE lt_bnka.
    ENDIF.

    CREATE DATA lr_data LIKE LINE OF et_data.
    ASSIGN lr_data->* TO <ls_data>.

    LOOP AT lt_bnka INTO DATA(ls_bnka).
      MOVE-CORRESPONDING ls_bnka TO ls_pp_bnka.             "#EC ENHOK
      ls_pp_bnka-bnka  = ls_bnka-bankl.
      ls_pp_bnka-bnklz = ls_bnka-bnklz.
      ls_pp_bnka-ort01 = ls_bnka-ort01.
      ls_pp_bnka-stras = ls_bnka-stras.

      MOVE-CORRESPONDING ls_pp_bnka TO <ls_data>.
      INSERT <ls_data> INTO TABLE et_data.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_usmd_pp_access~read_value.

    DATA:
      lt_bnka    TYPE STANDARD TABLE OF bnka,
      lr_data    TYPE REF TO data,
      ls_sel     TYPE usmd_s_sel,
      lrt_bnk_dt TYPE RANGE OF bankl,
      lrs_bnk_dt LIKE LINE OF lrt_bnk_dt.

    FIELD-SYMBOLS:
      <lv_bnk>  TYPE any,
      <ls_bnka> LIKE LINE OF lt_bnka,
      <ls_data> TYPE any.

* Check input
    ASSERT i_entity EQ 'BNKA'.

* Initialize output
    CLEAR:
      et_data,
      et_message.

* Build range for select
    LOOP AT it_sel INTO ls_sel WHERE fieldname EQ i_entity.
      MOVE-CORRESPONDING ls_sel TO lrs_bnk_dt.
      APPEND lrs_bnk_dt TO lrt_bnk_dt.
    ENDLOOP.

* Create local data structure
    CREATE DATA lr_data LIKE LINE OF et_data.
    ASSIGN lr_data->* TO <ls_data>.
    ASSIGN COMPONENT 'BNKA' OF STRUCTURE <ls_data> TO <lv_bnk>.
    gv_bnka_key = <lv_bnk>.

* get data
    SELECT * FROM bnka INTO TABLE lt_bnka
      WHERE bankl IN lrt_bnk_dt.

* Convert data from DB format into MDG format
    LOOP AT lt_bnka ASSIGNING <ls_bnka>.
      MOVE-CORRESPONDING <ls_bnka> TO <ls_data>.
      IF <lv_bnk> IS ASSIGNED.
        <lv_bnk> = <ls_bnka>-bankl.
      ENDIF.
      INSERT <ls_data> INTO TABLE et_data.
    ENDLOOP.


  ENDMETHOD.


  METHOD if_usmd_pp_access~save.

    DATA:
      lt_entity            TYPE usmd_t_entity,
      lv_entity            TYPE usmd_entity,
      ls_bnka              TYPE bnka,
      ls_message           TYPE usmd_s_message,
      lt_message           TYPE usmd_t_message,
      lv_new               TYPE boolean,
      ls_action            TYPE usmd_actioncode,
      ls_bapi1011_addressx TYPE  bapi1011_addressx,
      ls_bapi1011_detail   TYPE  bapi1011_detail,
      ls_bapi1011_detailx  TYPE  bapi1011_detailx,
      lrt_data_inserted    TYPE REF TO data,
      lrt_data_updated     TYPE REF TO data,
      lrt_data_deleted     TYPE REF TO data.

    DATA ls_bank_address TYPE  bapi1011_address.

    DATA ls_return TYPE  bapiret2.
    DATA lv_bankcountry  TYPE  bapi1011_key-bank_ctry.
    DATA lv_bankkey     TYPE  bapi1011_key-bank_key.
    DATA ls_bank_address1 TYPE  bapiaddr1.

    FIELD-SYMBOLS:
      <lt_insert> TYPE ANY TABLE,
      <lt_update> TYPE ANY TABLE,
      <lt_delete> TYPE ANY TABLE,
      <lv_bnka>   TYPE bankk,
      <lv_banks>  TYPE banks.

    CLEAR:
      et_message,
      et_tmp_key_map,
      lv_new,
      ls_bapi1011_addressx,
      ls_bapi1011_detail,
      ls_bapi1011_detailx,
      ls_message,
      lt_message.

* process data for entity type
    io_delta->get_entity_types(
      IMPORTING
        et_entity = lt_entity ).

    LOOP AT lt_entity INTO lv_entity.
      io_delta->read_data(
        EXPORTING
          i_entity      = lv_entity                             " entity
          i_struct      = if_usmd_model_ext=>gc_struct_key_attr " keys and attributes
        IMPORTING
          er_t_data_ins = lrt_data_inserted                     " inserted data
          er_t_data_upd = lrt_data_updated                      " updated data
          er_t_data_del = lrt_data_deleted ).                   " deleted data

      CASE lv_entity.
        WHEN 'BNKA'.

          IF lrt_data_inserted IS NOT INITIAL.
            lv_new = abap_true.
            ASSIGN lrt_data_inserted->* TO <lt_insert>.
            LOOP AT <lt_insert> ASSIGNING FIELD-SYMBOL(<lrs_data_inserted>).
              ASSIGN COMPONENT 'BNKA' OF STRUCTURE <lrs_data_inserted> TO <lv_bnka>.
              ASSIGN COMPONENT 'BANKS' OF STRUCTURE <lrs_data_inserted> TO <lv_banks>.

              " insert means new, it shouldn't be in db
              SELECT SINGLE * FROM bnka
                WHERE bankl = @<lv_bnka> AND
                      banks = @<lv_banks>
                INTO @ls_bnka.

              IF sy-subrc EQ 0.
                " HATA
                ls_message-msgid = 'ZMDG_MSG_CLSS_AOZ'.
                ls_message-msgno = 002.
                ls_message-msgty = 'E'.
                ls_message-msgv1 = <lv_bnka>.
                ls_message-msgv2 = <lv_banks>.
                APPEND ls_message TO et_message.
                CLEAR ls_bnka.
                RETURN.
              ELSE.
                MOVE-CORRESPONDING <lrs_data_inserted> TO ls_bnka.
                ls_bnka-mandt = sy-mandt.
                ls_bnka-bankl = <lv_bnka>.
                ls_bnka-ernam = sy-uname.
                ls_bnka-erdat = sy-datum.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lrt_data_updated IS NOT INITIAL.
            ASSIGN lrt_data_updated->* TO <lt_update>.
            LOOP AT <lt_update> ASSIGNING FIELD-SYMBOL(<lrs_data_updated>).
              ASSIGN COMPONENT 'BNKA' OF STRUCTURE <lrs_data_updated> TO <lv_bnka>.
              MOVE-CORRESPONDING <lrs_data_updated> TO ls_bnka.
              ls_bnka-mandt = sy-mandt.
              ls_bnka-bankl = <lv_bnka>.
            ENDLOOP.
          ENDIF.
          IF ls_bnka IS NOT INITIAL.

            ls_bank_address-bank_name   = ls_bnka-banka.
            ls_bank_address-region      = ls_bnka-provz.
            ls_bank_address-street      = ls_bnka-stras.
            ls_bank_address-city        = ls_bnka-ort01.
            ls_bank_address-swift_code  = ls_bnka-swift.
            ls_bank_address-bank_group  = ls_bnka-bgrup.
            ls_bank_address-pobk_curac  = ls_bnka-xpgro.
            ls_bank_address-bank_no     = ls_bnka-bnklz.
            ls_bank_address-post_bank   = ls_bnka-pskto.
            ls_bank_address-bank_branch = ls_bnka-brnch.
            ls_bank_address-addr_no     = ls_bnka-adrnr.
            ls_bapi1011_detail-bank_delete = ls_bnka-loevm.

            CASE lv_new.
              WHEN abap_true.
                MOVE-CORRESPONDING ls_bank_address TO ls_bank_address1.
                MOVE-CORRESPONDING ls_bapi1011_detail TO ls_bapi1011_detailx.
                ls_bank_address1-country = ls_bnka-banks.

                CALL FUNCTION 'BAPI_BANK_CREATE'
                  EXPORTING
                    bank_ctry     = ls_bnka-banks
                    bank_key      = ls_bnka-bankl
                    bank_address  = ls_bank_address
*                   BANK_METHOD   =
*                   BANK_FORMATTING =
                    bank_address1 = ls_bank_address1
*                   I_XUPDATE     = 'X'
*                   I_CHECK_BEFORE_SAVE                =
*                   BANK_IBAN_RULE                     =
*                   BANK_B2B_SUPPORTED                 =
*                   BANK_COR1_SUPPORTED                =
*                   BANK_R_TRANSACTION_SUPPORTED       =
*                   BANK_INTERNAL_BANK                 =
*                   I_NO_OVERWRITE                     =
                  IMPORTING
                    return        = ls_return
                    bankcountry   = lv_bankcountry
                    bankkey       = lv_bankkey.

*                IF ls_bapi1011_detailx-bank_delete = 'X' AND ls_bapi1011_detail-bank_delete = 'X'.
*                  CALL FUNCTION 'BAPI_BANK_CREATE'
*                    EXPORTING
*                      bank_ctry     = ls_bnka-banks
*                      bank_key      = ls_bnka-bankl
*                      bank_address  = ls_bank_address
*                     BANK_METHOD   = ls_bapi1011_detail
*                     BANK_FORMATTING = ls_bapi1011_detailx
*                      bank_address1 = ls_bank_address1
**                     I_XUPDATE     = 'X'
**                     I_CHECK_BEFORE_SAVE                =
**                     BANK_IBAN_RULE                     =
**                     BANK_B2B_SUPPORTED                 =
**                     BANK_COR1_SUPPORTED                =
**                     BANK_R_TRANSACTION_SUPPORTED       =
**                     BANK_INTERNAL_BANK                 =
**                     I_NO_OVERWRITE                     =
*                    IMPORTING
*                      return        = ls_return
*                      bankcountry   = lv_bankcountry
*                      bankkey       = lv_bankkey.
*
*
*                ENDIF.

              WHEN abap_false.
                MOVE-CORRESPONDING ls_bapi1011_detail TO ls_bapi1011_detailx.
                IF <lt_update> IS ASSIGNED AND
                    <lt_update> IS NOT INITIAL.

                  LOOP AT <lt_update> ASSIGNING FIELD-SYMBOL(<ls_update>).
                    ASSIGN COMPONENT 'USMDX_S_UPDATE' OF STRUCTURE <ls_update> TO FIELD-SYMBOL(<ls_usmdx_update>).

                    IF <ls_usmdx_update> IS ASSIGNED.
                      ASSIGN COMPONENT 'BANKA' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_banka_up>).
                      ASSIGN COMPONENT 'BRNCH' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_brnch_up>).
                      ASSIGN COMPONENT 'ORT01' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_ort01_up>).
                      ASSIGN COMPONENT 'STRAS' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_stras_up>).
                      ASSIGN COMPONENT 'SWIFT' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_swift_up>).
                      ASSIGN COMPONENT 'LOEVM' OF STRUCTURE <ls_usmdx_update> TO FIELD-SYMBOL(<lv_loevm_up>).

                      IF <lv_banka_up> IS ASSIGNED AND
                         <lv_brnch_up> IS ASSIGNED AND
                         <lv_ort01_up> IS ASSIGNED AND
                         <lv_stras_up> IS ASSIGNED AND
                         <lv_swift_up> IS ASSIGNED AND
                         <lv_loevm_up> IS ASSIGNED .
                        ls_bapi1011_addressx-bank_name  = <lv_banka_up>.
                        ls_bapi1011_addressx-bank_branch = <lv_brnch_up>.
                        ls_bapi1011_addressx-city = <lv_ort01_up>.
                        ls_bapi1011_addressx-street = <lv_stras_up>.
                        ls_bapi1011_addressx-swift_code = <lv_swift_up>.
                        ls_bapi1011_detailx-bank_delete = <lv_loevm_up>.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.

*                  IF ls_bapi1011_detailx-bank_delete = 'X' AND ls_bapi1011_detail-bank_delete = 'X'.
*
*                    CALL FUNCTION 'BAPI_BANK_CHANGE'
*                      EXPORTING
*                        bankcountry   = ls_bnka-banks
*                        bankkey       = ls_bnka-bankl
*                        bank_address  = ls_bank_address
*                        bank_addressx = ls_bapi1011_addressx
*                        bank_detail   = ls_bapi1011_detail
*                        bank_detailx  = ls_bapi1011_detailx
**                       bank_address1 = ls_bank_address1
**                       BANK_ADDRESS1X            =
**                       I_CHECK_BEFORE_SAVE    =
*                      IMPORTING
*                        return        = ls_return.

*                  ELSE.
                    CALL FUNCTION 'BAPI_BANK_CHANGE'
                      EXPORTING
                        bankcountry   = ls_bnka-banks
                        bankkey       = ls_bnka-bankl
                        bank_address  = ls_bank_address
                        bank_addressx = ls_bapi1011_addressx
                        bank_detail   = ls_bapi1011_detail
                        bank_detailx  = ls_bapi1011_detailx
*                       bank_address1 = ls_bank_address1
*                       BANK_ADDRESS1X            =
*                       I_CHECK_BEFORE_SAVE    =
                      IMPORTING
                        return        = ls_return.

*                  ENDIF.
                ENDIF.

              WHEN OTHERS.
                RETURN.
            ENDCASE.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  method IF_USMD_PP_BLOCKLIST~GET_BLOCKLIST_FOR_READ.
  endmethod.


  method IF_USMD_PP_BLOCKLIST~GET_BLOCKLIST_FOR_WRITE.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~ADAPT_RESULT_LIST.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~ADAPT_SEL_FIELDS.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~ADAPT_WHERE_CLAUSE.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~GET_MAPPING_INFO.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~GET_REUSE_VIEW_CONTENT.
  endmethod.


  method IF_USMD_PP_HANA_SEARCH~MERGE_REUSE_AUTHORIZATION.
  endmethod.
ENDCLASS.
