create or replace view xxpi_osvutl_v as
select
       o.osv_id,
       o.file_id,
       o.bank_acc_id,
       --o.file_id,
       o.bank_date,
       o.is_1c,
       o.is_1c_reversed,
       o.beg_balance,
       decode(ucredit_null, NULL, NULL, o.beg_balance) beg_balance_null,
       bkf.beg_date f_beg_date, -- íà÷àëî ïåðèîäà ïî âûïèñêå, "âêëþ÷àþùåé" äåíü
       bkf.end_date f_end_date, -- êîíåö ïåðèîäà ïî âûïèñêå, "âêëþ÷àþùåé" äåíü
       beg_bkf.beg_balance f_beg_balance, -- âõ ñàëüäî ïî âûïèñêå, âûáðàííîé íà áàíê. äåíü ïî beg_date
       beg_bkf.file_id f_beg_file_id,
       beg_bkf.creation_date f_beg_creation_date,
       beg_bkf.lend_date f_beg_lend_date,
       end_bkf.end_balance f_end_balance, -- èñõ ñàëüäî ïî âûïèñêå, âûáðàííîé íà áàíê äåíü ïî end_date
       end_bkf.file_id f_end_file_id,
       end_bkf.creation_date f_end_creation_date,
       end_bkf.lend_date f_end_lend_date,
       bkf.sum_in f_sum_in, -- ïðèõîä ïî ÁÂ
       bkf.sum_out f_sum_out, -- ðàñõîä ïî ÁÂ
       bkf.file_id f_file_id,
       bkf.creation_date f_creation_date,
       bkf.lend_date f_lend_date,
       o1.prev_end_balance, -- èñõ ñàëüäî ïðåäûä äíÿ, åñëè èìååòñÿ
       o2.next_beg_balance, -- âõ ñàëüäî ñëåäóþùåãî äíÿ, åñëè èìååòñÿ
       o.end_balance,
       decode(ucredit_null, NULL, NULL, o.end_balance) end_balance_null,
       o.udebit, -- îáîðîòû ïî äåáåòó - ñîõðàí¸ííûå îáîðîòû ïî udoc
       o.ucredit, -- îáîðîòû ïî êðåäèòó - ñîõðàí¸ííûå îáîðîòû ïî udoc
       o.udebit_null, -- åñëè íåò äàííûõ âûïèñêè íà äåíü - òî NULL
       o.ucredit_null, --åñëè íåò äàííûõ âûïèñêè íà äåíü - òî NULL
--       q.debit_, -- îáîðîòû ïî äåáåòó ïî udoc (òåêóùåå ðàñ÷¸òíîå çíà÷åíèå)
--       q.credit_, -- îáîðîòû ïî êðåäèòó ïî udoc (òåêóùåå ðàñ÷¸òíîå çíà÷åíèå)
       o.created_by,
       o.creation_date,
       o.updated_by,
       o.update_date,
       o.beg_date, -- íà÷àëî ðàñ÷ ïåðèîäà
       o.end_date  -- êîíåö ðàñ÷ ïåðèîäà
       from
            xxpi_osv o,
            -- AMOMZYAKOV 05/06/2012 (select end_balance prev_end_balance , bank_acc_id, bank_date, is_1c
            (select DECODE(file_id, NULL, to_number(NULL), end_balance) prev_end_balance , bank_acc_id, bank_date, is_1c
                    from xxpi_osv
            ) o1, -- äëÿ èñõ ñàëüäî ïðåäûä äíÿ
            -- AMOMZYAKOV 05/06/2012 (select beg_balance next_beg_balance , bank_acc_id, bank_date, is_1c
            (select DECODE(file_id, NULL, to_number(NULL), beg_balance) next_beg_balance , bank_acc_id, bank_date, is_1c
                    from xxpi_osv
            ) o2, -- äëÿ âõ ñàëüäî ñëåä äíÿ
            (select a.beg_date, a.end_date, a.beg_balance, a.end_balance, a.sum_in, a.sum_out,
                    a.file_id, a.is_1c, a.bank_acc_id, a.creation_date, f.lend_date -- ïî äàòå íà÷àëà ïåðèîäà
                   from xxpi_bk_imp_file f,
                        xxpi_bk_imp_accf a,
                        ap_bank_branches bb,
                        ap_bank_accounts_all aa
                        where 1=1
                              and bb.bank_branch_id = aa.bank_branch_id
                              and aa.bank_account_id = a.bank_acc_id
                              and (((bb.attribute13 IS NULL
                                     or (bb.attribute13 IS NOT NULL and bb.attribute13<>'Y')
                                    ) and a.create_cd like '%F%'
                                   )
                                    or (bb.attribute13='Y' and a.beg_date = a.end_date)
                                  )
                              and f.file_id = a.file_id

             MINUS
             select a.beg_date, a.end_date, a.beg_balance, a.end_balance, a.sum_in, a.sum_out,
                    a.file_id, a.is_1c, a.bank_acc_id, a.creation_date, f.lend_date -- ïî äàòå íà÷àëà ïåðèîäà
                   from xxpi_bk_imp_file f,
                        xxpi_bk_imp_accf a,
                        ap_bank_branches bb,
                        ap_bank_accounts_all aa
                        where 1=1
                              and bb.bank_branch_id = aa.bank_branch_id
                              and aa.bank_account_id = a.bank_acc_id
                              and a.beg_date = a.end_date
                              and --or
                                  (a.create_cd like '%D%' and (bb.attribute13 IS NULL
                                                           or (bb.attribute13 IS NOT NULL and bb.attribute13<>'Y') )
                                                                     and exists (
                                                                         select 1 from xxpi_bk_imp_file f1, xxpi_bk_imp_accf a1
                                                                                where f1.file_id = a1.file_id
                                                                                      and a1.bank_acc_id = a.bank_acc_id
                                                                                      and a1.beg_date = a.beg_date
                                                                                      and a1.end_date = a.end_date
                                                                                      and a1.create_cd = 'F'
                                                                                      and a1.is_1c = a.is_1c
                                                                         )
                                  )
                                  --)
                              and f.file_id = a.file_id
            ) bkf, -- äëÿ äàííûõ ïî ïîñëåäíåé âûïèñêå, îõâàòûâàþùåé äàòó áàíêà
            (select a.beg_date, a.end_date, a.beg_balance, a.end_balance, a.sum_in, a.sum_out,
                    a.file_id, a.is_1c, a.bank_acc_id, a.creation_date, f.lend_date -- ïî äàòå íà÷àëà ïåðèîäà
                   from xxpi_bk_imp_file f,
                        xxpi_bk_imp_accf a
                        where 1=1
                              and a.create_cd like '%F%'
                              and trunc(nvl(a.creation_date, f.lend_date)) > a.beg_date -- îòáðàêîâêà ïî äàòå ñîçäàíèÿ
                              and a.file_id = f.file_id
            ) beg_bkf, -- äëÿ âõ ñàëüäî ïî âûïèñêå íà äàòó
            (select a.beg_date, a.end_date, a.beg_balance, a.end_balance, a.sum_in, a.sum_out,
                    a.file_id, a.is_1c, a.bank_acc_id, a.creation_date, f.lend_date -- ïî äàòå íà÷àëà ïåðèîäà
                   from xxpi_bk_imp_file f,
                        xxpi_bk_imp_accf a
                        where 1=1
                              and a.create_cd like '%F%'
                              and trunc(nvl(a.creation_date, f.lend_date)) > a.end_date -- îòáðàêîâêà ïî äàòå ñîçäàíèÿ
                              and a.file_id = f.file_id
            ) end_bkf -- äëÿ èñõ ñàëüäî ïî âûïèñêå íà äàòó
            where 1=1
                  and o1.bank_date(+) = o.bank_date - 1
                  and o1.bank_acc_id(+) = o.bank_acc_id
                  and o1.is_1c(+) = o.is_1c
                  and o2.bank_date(+) = o.bank_date + 1
                  and o2.bank_acc_id(+) = o.bank_acc_id
                  and o2.is_1c(+) = o.is_1c
                              and bkf.bank_acc_id(+) = o.bank_acc_id
                              and bkf.is_1c(+) = o.is_1c
                              and bkf.beg_date(+) <= o.bank_date
                              and bkf.end_date(+) >= o.bank_date
                              and beg_bkf.bank_acc_id(+) = o.bank_acc_id
                              and beg_bkf.is_1c(+) = o.is_1c
                              and beg_bkf.beg_date(+) = o.bank_date
                              and end_bkf.bank_acc_id(+) = o.bank_acc_id
                              and end_bkf.is_1c(+) = o.is_1c
                              and end_bkf.end_date(+) = o.bank_date
                  --and o.bank_acc_id = q.bank_acc_id(+)
                  --and o.bank_date = q.bank_date(+)
                  --and o.is_1c = q.is_1c(+)
--            order by is_1c, bank_acc_id, bank_date
