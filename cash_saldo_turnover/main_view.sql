create or replace view xxpi_osv_v as
select a."OSV_ID",
       a.file_id,
       a.end_balance_null,
       a.beg_balance_null,
       a."BANK_ACC_ID",
       a."BANK_DATE",
       a."IS_1C",
       a."IS_1C_REVERSED",
       a."BEG_BALANCE",
       a."F_BEG_DATE",
       a."F_END_DATE",
       a."F_BEG_BALANCE",
       a."F_BEG_FILE_ID",
       a."F_BEG_CREATION_DATE",
       a."F_END_BALANCE",
       a."F_END_FILE_ID",
       a."F_END_CREATION_DATE",
       a."F_SUM_IN",
       a."F_SUM_OUT",
       a."F_FILE_ID",
       a."F_CREATION_DATE",
       a."PREV_END_BALANCE",
       a."NEXT_BEG_BALANCE",
       a."END_BALANCE",
       a."UDEBIT",
       a."UCREDIT",
       a."UDEBIT_NULL",
       a."UCREDIT_NULL",
       a."CREATED_BY",
       a."CREATION_DATE",
       a."UPDATED_BY",
       a."UPDATE_DATE",
       a."BEG_DATE",
       a."END_DATE",
       a."BK_FILE_TAG",
       a.bk_day_errors,
       a."FF_CREATION_DATE",
       b.beg_balance beg_balance_alt,
       DECODE(b.udebit_null, NULL, NULL, b.beg_balance) beg_balance_null_alt,
       b.udebit_null udebit_null_alt,
       b.ucredit_null ucredit_null_alt,
       b.end_balance end_balance_alt,
       DECODE(b.udebit_null, NULL, NULL, b.end_balance) end_balance_null_alt
       , a.is_locked
       , a.pre_locked
       , a.post_locked
--b.beg_balance beg_balance_1c,
--b.udebit_null udebit_null_1c,
--b.ucredit_null ucredit_null_1c,
--b.end_balance end_balance_1c
  from (select a."OSV_ID",
               a."BANK_ACC_ID",
               a."BANK_DATE",
               a."IS_1C",
               a."IS_1C_REVERSED",
               a."BEG_BALANCE",
               a.beg_balance_null,
               a."F_BEG_DATE",
               a."F_END_DATE",
               a."F_BEG_BALANCE",
               a."F_BEG_FILE_ID",
               a."F_BEG_CREATION_DATE",
               a."F_END_BALANCE",
               a."F_END_FILE_ID",
               a."F_END_CREATION_DATE",
               a."F_SUM_IN",
               a."F_SUM_OUT",
               a."F_FILE_ID",
               a."F_CREATION_DATE",
               a."PREV_END_BALANCE",
               a."NEXT_BEG_BALANCE",
               a."END_BALANCE",
               a.end_balance_null,
               a."UDEBIT",
               a."UCREDIT",
               a."UDEBIT_NULL",
               a."UCREDIT_NULL",
               a."CREATED_BY",
               a."CREATION_DATE",
               a."UPDATED_BY",
               a."UPDATE_DATE",
               a."BEG_DATE",
               a."END_DATE",
               --DECODE(a.f_file_id, NULL, NULL, a."UDEBIT") udebit_string, -- åñëè íåò ÁÂ íà äàííûé äåíü, îáðîòû ïîê-åì êàê NULL
               --DECODE(a.f_file_id, NULL, NULL, a."UCREDIT") ucredit_string,
               DECODE(f.file_id,
                      NULL,
                      -- 05.07.2012 AMOMZYAKOV - îøèáêè ïî ñóùåñòâóþùåé âûïèñêå -  â îòäåëüíîé êîëîíêå!
                      -- DECODE(a.file_id, NULL, 'Îòñóòñòâóåò', a.file_id || ':  ' || ' Â âûïèñêå íåò ñàëüäî íà ýòîò äåíü.'),
                      -- DECODE(a.file_id, NULL, 'Îòñóòñòâóåò', a.file_id || ':  ' || to_char(f.beg_date, 'dd.mm.rr') || ' - ' || to_char(f.end_date, 'dd.mm.rr')),
                      DECODE(a.file_id,
                             NULL,
                             'Îòñóòñòâóåò',
                             a.file_id || ':  ' ||
                             (select to_char(beg_date, 'dd.mm.rr')
                                from xxpi_bk_imp_file
                               where file_id = a.file_id) || ' - ' ||
                             (select to_char(end_date, 'dd.mm.rr')
                                from xxpi_bk_imp_file
                               where file_id = a.file_id)),
                      f.file_id || ':  ' || to_char(f.beg_date, 'dd.mm.rr') ||
                      ' - ' || to_char(f.end_date, 'dd.mm.rr')) bk_file_tag, -- ìåòêà ôàéëà ÁÂ
               DECODE(f.file_id,
                      NULL,
                      -- 05.07.2012 AMOMZYAKOV - îøèáêè ïî ñóùåñòâóþùåé âûïèñêå -  â îòäåëüíîé êîëîíêå!
                      -- DECODE(a.file_id, NULL, 'Îòñóòñòâóåò', a.file_id || ':  ' || ' Â âûïèñêå íåò ñàëüäî íà ýòîò äåíü.'),
                      DECODE(a.file_id,
                             NULL,
                             'Íåò äàííûõ âûïèñêè! ',
                             'Â âûïèñêå íåò ñàëüäî íà ýòîò äåíü. '),
                      NULL) ||
               xxpi_bk_imp.check_bk_dates(a.bank_date,
                                          a.f_end_date,
                                          f.creation_date) BK_DAY_ERRORS, -- ìåòêà ôàéëà ÁÂ
               f.creation_date ff_creation_date,
               a.file_id -- ïîëå òàáëèöû xxpi_osv, çàïîëíÿåòñÿ âî âðåìÿ ðàñ÷¸òà îáîðîòêè - äëÿ àíàëèçà àêòóàëüíîñòè îáîðîòêè
               -- AMOMZYAKOV 09.04.2015
               , DECODE(lk.bank_date, null, 'N', 'Y') is_locked -- äåíü ïîïàë â ïåðèîä áëîêèðîâêè
               , DECODE(prev_lk.bank_date, null, 'N', 'Y') pre_locked  -- äåíü ïðåäøåñòâóåò ïåðèîäó áëîêèðîâêè (äëÿ èíòåðïðåòàöèè ðàñõ-ÿ âõ è èñõ ñàëüäî)
               , DECODE(lk_next.bank_date, null, 'N', 'Y') post_locked -- äåíü ñëåäóåò çà ïåðèîäîì áëîêèðîâêè (äëÿ èíòåðïðåòàöèè ðàñõ-ÿ âõ è èñõ ñàëüäî)
               -- end 09.04.2015
          from
               xxpi_osvutl_v a,
               xxpi_bk_imp_file f,
               -- AMOMZYAKOV 09.04.2015
               (select distinct o.bank_date, o.bank_acc_id
                       from xxpi_osv o,
                            xxpi_acc_locks l
                       where o.bank_acc_id = l.bank_account_id
                             and o.bank_date >= nvl(l.lock_from_date, o.bank_date)
                             and o.bank_date <= nvl(l.lock_to_date, o.bank_date)
                             and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                             and l.deleted_flg = 'N'
                             and o.is_1c = 'N'
               ) lk, -- äíè ñ ïðèçíàêîì áëîêèðîâêè ïî ñ÷åòàì
               (select distinct o.bank_date, o.bank_acc_id
                       from xxpi_osv o,
                            xxpi_acc_locks l
                       where o.bank_acc_id = l.bank_account_id
                             and o.bank_date = nvl(l.lock_from_date, o.bank_date) - 1
                             and l.deleted_flg = 'N'
                             and o.is_1c = 'N'
               ) prev_lk, -- äíè, ïðåäøåñòâóþùèå ïåðèîäó áëîêèðîâêè
               (select distinct o.bank_date, o.bank_acc_id
                       from xxpi_osv o,
                            xxpi_acc_locks l
                       where o.bank_acc_id = l.bank_account_id
                             and o.bank_date = nvl(l.lock_to_date, o.bank_date) + 1
                             and l.deleted_flg = 'N'
                             and o.is_1c = 'N'
               ) lk_next-- äíè, ñëåäóþùèå çà ïåðèîäîì áëîêèðîâêè
               -- end 09.04.2015
         where 1 = 1
           and (f_creation_date is null or
               f_creation_date =
               (select max(b.creation_date)
                   from xxpi_bk_imp_accf b
                  where 1 = 1
                    -- AMOMZYAKOV and b.create_cd like '%F%'
                    and b.is_1c = a.is_1c
                    and b.bank_acc_id = a.bank_acc_ID
                    and b.beg_date <= a.bank_date
                    and b.end_date >= a.bank_date
                    ))
           and (f_beg_creation_date IS null or
               f_beg_creation_date =
               (select max(b.creation_date)
                   from xxpi_bk_imp_accf b
                  where 1 = 1
                    and b.bank_acc_id = a.bank_acc_ID
                    and b.beg_date = a.bank_date
                    and b.is_1c = a.is_1c
                     and b.create_cd like '%F%'
                    ))
           and (f_end_creation_date IS null or
               f_end_creation_date =
               (select max(b.creation_date)
                   from xxpi_bk_imp_accf b
                  where 1 = 1
                    and b.bank_acc_id = a.bank_acc_ID
                    and b.end_date = a.bank_date
                    and b.is_1c = a.is_1c
                     and b.create_cd like '%F%'
                    ))

           and f.file_id(+) = a.f_file_id
           -- AMOMZYAKOV 09.04.2015
           and a.bank_acc_id = lk.bank_acc_id (+)
           and a.bank_date = lk.bank_date (+)
           and a.bank_acc_id = prev_lk.bank_acc_id (+)
           and a.bank_date = prev_lk.bank_date (+)
           and a.bank_acc_id = lk_next.bank_acc_id (+)
           and a.bank_date = lk_next.bank_date (+)
           -- end 09.04.2015
        ) a,
       xxpi_osv b -- àëüòåðíàòèâíûé èñòî÷íèê
 where 1 = 1
   and b.bank_date(+) = a.bank_date
   and b.bank_acc_id(+) = a.BANK_ACC_ID
   and b.is_1c(+) = a.is_1c_reversed

 order by a.is_1c, a.bank_acc_id, a.bank_date
