create or replace package xxpi_bk_err_stats_pkg as
  -- Создано: 16.05.2012 AMOMZAYKOV
  -- Изменения:
  -- 25.05.2012 AMOMZYAKOV добавил корректировку периода сводки с учётом периода действия счёта:
  -- добавлена процедура update_period, ее вызов добавлен в prepare_err_info_by_acc
  -- 10.07.2012 - добавлена ошибка "Дата создания файла равна или предшествует дате исх. сальдо в"
  -- в acc_errors_info
  -- 01.08.2013 - исправлено условие для случаев с "дырками" в оборотно-сальдовой инфе выписок (acc_errors_info)
  -- 02.08.2013 - изменено сообщение "Обнаружены неверно заданные периоды выписки" на
  -- "Нет данных по оборотам и сальдо выписки"
  -- 14/02/2014 - добавлен отчёт об изначально невыверенных при загрузке БК платежах (bk_initially_not_matched_rep)
  -- 21.04.2015 - добавлена процедура подготовки напоминаний о загрузке данных из 1С (send_1c_reminder)
  -- 08.07.2015 - добавлена процедура send_bk_reminder  -- Запускается из 2-х парал. программ
  --            - одна - для еженедельной загрузки - с параметром is_weekly='Y',
  --            - другая - для ежемесячной загрузки - с параметром is_weekly='N'

  n_user_id number; -- User_id для OEBS
  friday_num number(1);

  -- возвращает текст ошибок по оборотке для зад. счёта, источника, периода
  function acc_errors_info(pd_from_date in date,  -- дата начала периода
                           pd_to_date in date, -- дата окончания периода
                           pn_acc_id in number, -- id внутр. расч счёта (тип - INTERNAL !!!)
                           pv_is1C in varchar2,-- источник (1с - Y, бк- N)
                           p_break varchar2 default 'N'
                           ) return varchar2;
  -- то же с учетом периодов блокировки
  function acc_errors_info_unblocked(pd_from_date in date,  -- дата начала периода
                           pd_to_date in date, -- дата окончания периода
                           pn_acc_id in number, -- id внутр. расч счёта (тип - INTERNAL !!!)
                           pv_is1C in varchar2,-- источник (1с - Y, бк- N)
                           p_break varchar2 default 'N'
                           ) return varchar2;

  -- формирует либо обновляет запись об ошибках по заданному расч счёту, году и источнику в
  -- табле xxpi_bk_err_stats
  -- (тек. год считается с 01.01 по SYSDATE)
  procedure prepare_err_info_byacc(errbuf    out nocopy varchar2,
                                retcode   out nocopy number,
                                pn_yr in number, -- год не позднее текущего
                                pn_acc_id in number, -- id внутр. расч счёта (тип - INTERNAL !!!)
                                pv_is1C in varchar2 -- источник (1с - Y, бк- N)
                                );
  -- формирует либо обновляет записи по всем расч счетам
  -- по заданному периоду в годах
  -- (тек. год считается с 01.01 по SYSDATE)
  procedure prepare_err_info(errbuf    out nocopy varchar2,
                          retcode   out nocopy number,
                          pv_ul_1C_code in varchar2, -- код юр лица из 1С -- корректность кода проверяется в параллельной программе
                          pn_acc_id in varchar2, -- id расч счёта - м.б. NULL
                          pn_beg_yr in number, -- начальный год периода (включительно)
                          pn_end_yr in number, -- конечный год периода (включительно) - не позднее тек года!
                          pv_is1C in varchar2 -- источник (если из набора значений AP_OIE_YES_NO_MAND - 'Yes' или 'No')
                          );

  -----------------------------------------------------------------------------
  -- отчёт о состоянии загрузки данных по расчётным счетам
  -- по предварительно сформированной
  -----------------------------------------------------------------------------
  procedure err_report(errbuf     out nocopy varchar2,
                       retcode    out nocopy number);

  -----------------------------------------------------------------------------
  -- формирования сводки ошибок загрузки по внутренним счетам
  -- для запуска из второго этапа загрузки выписки!!!
  -----------------------------------------------------------------------------
  procedure prepare_err_info_bystatement(errbuf    out nocopy varchar2,
                                         retcode   out nocopy number,
                                         p_log_id in number,
                                         p_log_entry_num in out number,
                                         p_user_id in number,
                                         p_req_id in number,
                                         p_file_id in number
                          );

  function get_last_friday(p_date in date) return date;

  -----------------------------------------------------------------
  -- пакетная сводка по всем РС за тек год
  -----------------------------------------------------------------
  procedure prep_err_info_batch(errbuf     out nocopy varchar2,
                                retcode    out nocopy number);

  -----------------------------------------------------------------------------
  -- Очёт по первоначально невыверенным (во время загрузки БВ) строкам БВ
  -- Для СБ
  -----------------------------------------------------------------------------
  procedure bk_initially_not_matched_rep(errbuf out nocopy varchar2,
                                         retcode out nocopy number);

  ------------------------------------------------------------------------------
  -- доб 15.04.2014
  -- генерация сообщения об ошибках загрузки для Рс с заданным открытым периодом
  -- ежедневной загрузки и сохранение в табличге xxpi_acc_daily_ctrl
  ------------------------------------------------------------------------------
  procedure prepare_daily_err_info(errbuf out VARCHAR2, retcode out NUMBER, p_bank_account_id IN NUMBER);

    -----------------------------------------------------------------------------
  -- Формирование сводки по состоянию загрузки по всем РС с ежедневной загрузкой
  -- для запуска из параллельной программы
  -----------------------------------------------------------------------------
  procedure prepare_daily_info_batch(errbuf out nocopy VARCHAR2, retcode out nocopy NUMBER);
  procedure prepare_weekly_info_batch(errbuf out nocopy VARCHAR2, retcode out nocopy NUMBER);


  procedure run_prepare_daily_info_batch(req_id out NUMBER);

  procedure send_1c_reminder(errbuf     out nocopy varchar2,
                       retcode    out nocopy number) ;

  procedure send_bk_reminder(errbuf     out nocopy varchar2,
                       retcode    out nocopy number,
                       is_weekly varchar2 default 'N' -- признак еженедельной загрузки для соотв. РС
                       ) ;


end xxpi_bk_err_stats_pkg;
/
create or replace package body xxpi_bk_err_stats_pkg as
/* Изменения тушки*/
-- AMOMZYAKOV
-- 14/02/2014 - добавлен отчёт об изначально невыверенных при загрузке БК платежах (bk_initially_not_matched_rep)
-- добавлена run_concurrent_report - запускалка для параллельных xml-отчётов
-- 19/06/2014 - run_concurrent_report_attache для посылки отчёта в виде аттача
-- 26/03/2015 - пересчет сообщений еще и за предыдущий год в prep_err_info_batch

  procedure update_period(pn_acc_id IN NUMBER, pd_beg_date IN OUT DATE, pd_end_date IN OUT DATE);
  -----------------------------------------------------------------------------
  -- ФУНКЦИЯ формирования информации об ошибках по заданному счёту, источнику загрузки и диапазону дат
  -- возвращает текст ошибки
  -----------------------------------------------------------------------------
  function acc_errors_info(pd_from_date in date, pd_to_date in date, pn_acc_id in number, pv_is1C in varchar2,
                           p_break varchar2 default 'N')
           return varchar2 is
    vv_acc_name VARCHAR2(80);
    result VARCHAR2(2000);
    -- для правильных результатов надо отсечь время от параметров - дат!!!
    vd_from_date DATE := TRUNC(pd_from_date);
    vd_to_date DATE := TRUNC(pd_to_date);
  begin

    result := '';

    begin
      select bank_account_name into vv_acc_name
           from
               ap_bank_accounts_all
               where
                    bank_account_id = pn_acc_id
                    and account_type = 'INTERNAL';
    exception
      when NO_DATA_FOUND then
           result := 'Счёт не существует или не является внутренним.';
           RETURN result;
    end;

    for Rec in (
               select 'Нет расчёта оборотов и сальдо в ' || count(*) || ' днях.' s, count(*) cnt, 1 ordr
                      from (
                           select vd_from_date + level - 1 bank_date
                                  from dual
                                       connect by level <= vd_to_date - vd_from_date + 1
                           MINUS
                           select bank_date
                                  from xxpi_osv
                                       where bank_date >= vd_from_date
                                             and bank_date <= vd_to_date
                                             and is_1c = pv_is1C
                                             and bank_acc_id = pn_acc_id
                           ) q
               UNION
               SELECT 'Не загружены данные выписки в ' || count(*)||' днях.' s, count(*) cnt, 2 ordr
                      FROM xxpi_osv_v
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and (
                                     -- нет загруженных из выписке данных
                                     file_id IS NULL
                                     )
               UNION
-- AMOMZYAKOV 02.08.2013               SELECT 'Обнаружены неверно заданные периоды выписки в ' || count(*)||' днях.' s, count(*) cnt, 3 ordr
               SELECT 'Нет данных по оборотам и сальдо выписки в ' || count(*)||' днях.' s, count(*) cnt, 3 ordr
                      FROM xxpi_osv_v
                           -- AMOMZYAKOV 01.08.2013
                           ,
                           (select aa.bank_account_id, bb.attribute13
                                       from ap_bank_accounts_all aa,
                                            ap_bank_branches bb
                                            where bb.bank_branch_id = aa.bank_branch_id
                           ) aa
                           -- 01.08.2013
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and aa.bank_account_id(+) = bank_acc_id -- 01.08.2013
                                 and (
                                     -- кривые выписке!
                                     file_id IS NOT NULL
                                     and f_file_id IS NULL
                                     -- AMOMZYAKOV 01.08.2013
                                     and (aa.attribute13 IS NULL or
                                         (aa.attribute13 IS NOT NULL and aa.attribute13<>'Y') )
                                     -- 01.08.2013
                                     )
               UNION
               SELECT 'Расхождения входящих и исходящих cальдо - в' || count(*)||' днях.' s, count(*) cnt, 4 ordr
                      FROM xxpi_osv_v
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and (
                                     -- расхождения вх и исх сальдо
                                     (beg_balance_null is not null and
                                     prev_end_balance <> beg_balance_null) or
                                     (end_balance_null is not null and
                                     next_beg_balance <> end_balance_null)
                                     )
               UNION
               SELECT 'Расхождения расчётных сальдо с выписками - в' || count(*)||' днях.' s, count(*) cnt, 5 ordr
                      FROM xxpi_osv_v
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and (
                                     -- расхождения сальдо с выпискаме
                                     (beg_balance_null is not null and
                                     -- AMOMZYAKOV 05/06/2012: не включаем в сводку расх-я с сальдо перегруженных выписок
                                     --beg_balance_null <> nvl(f_beg_balance, beg_balance_null)) or
                                     beg_balance_null <> nvl(f_beg_balance, beg_balance_null) and f_file_id=f_beg_file_id ) or
                                     (end_balance_null is not null and
                                     -- AMOMZYAKOV 05/06/2012: не включаем в сводку расх-я с сальдо перегруженных выписок
                                     --end_balance_null <> nvl(f_end_balance, end_balance_null))
                                     end_balance_null <> nvl(f_end_balance, end_balance_null) and f_file_id=f_end_file_id)
                                     )
               order by ordr

               ) loop

       if Rec.cnt > 0 then
          if nvl(length(result), 0) = 0 then
             result := Rec.s;
          else
             if p_break = 'Y' then
                result :=  result || '; ' || CHR(10) || Rec.s;
             else
                result :=  result || '; ' || Rec.s;
             end if;
          end if;
       end if;
    end loop;

    if nvl(length(result), 0)=0 then
       result := 'O.K.';
    end if;

    return result;

  end acc_errors_info;

  -----------------------------------------------------------------------------
  -- процедура подготовки сводной информации об ошибках по
  -- заданному внутр. расч. счёту, году и источнику
  -----------------------------------------------------------------------------
  procedure prepare_err_info_byacc(errbuf    out nocopy varchar2,
                                retcode   out nocopy number,
                                pn_yr in number, -- год не позднее текущего, не раньше 1900
                                pn_acc_id in number, -- id внутр. расч счёта (тип - INTERNAL !!!)
                                pv_is1C in varchar2 -- источник (1с - Y, бк- N)
                                ) IS
    CURSOR internal_accid_cur IS -- для проверки типа переданного счёта
           select bank_account_id
                  from ap_bank_accounts_all
                       where bank_account_id = pn_acc_id
                             and org_id = 81
                             and account_type = 'INTERNAL';
    CURSOR err_info_cur IS -- проверка наличия строки со сводкой ошибок
           select bank_acc_id
                  from xxpi_bk_err_stats
                       where bank_acc_id = pn_acc_id
                             and year = pn_yr
                             and is_1c = pv_is1C;
    tmp NUMBER;
    vd1 date; -- начало периода
    vd2 date; -- конец периода
  begin
    -- проверка корректности года
    if pn_yr < 1900 then
       errbuf := 'Задан год, предшествующий 1900-му году. Обработка невозможна.';
       retcode := 2;
       RETURN;
    end if;

    if pn_yr > to_number(to_char(sysdate, 'yyyy')) then
       errbuf := 'Задан будущий год. Обработка невозможна.';
       retcode := 2;
       RETURN;
    end if;

    -- проверка корректности расч. счёта

    if pn_acc_id IS NULL then
       retcode := 2; -- ошибка с вылетом
       errbuf := errbuf || 'Не задан ID внутреннего р.с. Обработка невозможна.';
       RETURN;
    else
       -- проверка, что счёт существует и является внутренним
       -- TODO добавить номер счёта и валюту
       open internal_accid_cur;
       fetch internal_accid_cur into tmp;
       if internal_accid_cur%NOTFOUND then
          retcode := 2;
          errbuf := errbuf || 'Задан неверный ID внутреннего р.с. Обработка невозможна.';
          close internal_accid_cur;
          RETURN;
       end if;
          close internal_accid_cur;
    end if;

    -- проверка корректности источника
    if UPPER(pv_is1c) not in ('Y', 'N') then
       retcode := 2;
       errbuf := errbuf || 'Неверно задан источник выписки. Обработка невозможна.';
       RETURN;
    end if;

    -- вычисляем начальную и конечную дату периода
    select to_date('0101' || to_char(pn_yr), 'ddmmyyyy') into vd1 -- 1 января
    from dual;
    if pn_yr = to_number(to_char(sysdate, 'YYYY')) then
       -- vd2 := get_last_friday(sysdate); -- последняя пятница предшествующая тек дню
       vd2 := get_last_friday(sysdate) - 1; -- последний четверг, предшествующий тек дню
    else
      select to_date('0101' || to_char(pn_yr + 1), 'ddmmyyyy') - 1 into vd2 -- 31 декабря
      from dual;
    end if;
    -- доб. AMOMZYAKOV 25.05.2012 -- корректировка периода сводки с учётом периода действия счёта
    update_period(pn_acc_id, vd1, vd2);
    -- кон 25.05.2012
    --TEST
    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Период ' || to_char(vd1)|| ' - ' ||to_char(trunc(vd2))|| ' ПТН: '||get_last_friday(trunc(sysdate)));
    --DBMS_OUTPUT.put_line( to_char(vd1)|| ' - ' ||to_char(trunc(vd2))|| ' '||get_last_friday(trunc(sysdate))|| ' ' ||to_char(sysdate));
    --
    open err_info_cur;
    fetch err_info_cur into tmp;
    if err_info_cur%FOUND then
       update xxpi_bk_err_stats
              set errors = xxpi_bk_err_stats_pkg.acc_errors_info(vd1, --trunc(SYSDATE, 'YYYY'),
                                                                 vd2, --SYSDATE,
                                                                 pn_acc_id,
                                                                 pv_is1C,
                                                                 'N'
                                                                 ),
                  -- AMOMZYAKOV 06.04.2015
                  errors_no_blocked = DECODE(pv_is1C, 'N', xxpi_bk_err_stats_pkg.acc_errors_info_unblocked(vd1, --trunc(SYSDATE, 'YYYY'),
                                                                 vd2, --SYSDATE,
                                                                 pn_acc_id,
                                                                 pv_is1C,
                                                                 'N'
                                                                 ), NULL ),
                  -- end 06.04.2015
                  last_update_date = sysdate,
                  last_updated_by = n_user_id
              where bank_acc_id = pn_acc_id
                    and year = pn_yr
                    and is_1c = pv_is1c;
       commit;
    else
       insert into xxpi_bk_err_stats(bank_acc_id, year, is_1c, creation_date, created_by,  errors, errors_no_blocked)
       values (pn_acc_id, pn_yr, pv_is1c, sysdate, n_user_id, xxpi_bk_err_stats_pkg.acc_errors_info(vd1, --trunc(SYSDATE, 'YYYY'),
                                                                 vd2, --SYSDATE,
                                                                 pn_acc_id,
                                                                 pv_is1C,
                                                                 'N'
                                                                 ),
                                                              -- AMOMZYAKOV 06.04.2015
                                                              DECODE(pv_is1C, 'N', xxpi_bk_err_stats_pkg.acc_errors_info_unblocked(vd1, --trunc(SYSDATE, 'YYYY'),
                                                                 vd2, --SYSDATE,
                                                                 pn_acc_id,
                                                                 pv_is1C,
                                                                 'N'
                                                                 ), NULL )
                                                              -- end 06.04.2015
                                                                 );
       commit;

    end if;

    close err_info_cur;

    retcode := 0;

    -- формируем текст для записи в журнал конкаррента
    select 'Сформирована сводка на основе данных ' || DECODE(pv_is1c, 'Y', '1С', 'БК' ) || ' по Р/с ' || --baw.bank_account_name,
           baw.bank_account_num || ' в ' ||
           bb.bank_name || ' ' ||
           bb.bank_branch_name || ' БИК ' ||
           bb.eft_swift_code || ' ' ||
           DECODE(bb.attribute1, NULL, NULL, ' К/с ' || bb.attribute1)
           into errbuf
           from apps.ap_bank_accounts_all baw,
                apps.XXPI_UL_BUH_V        bh,
                apps.ap_bank_branches     bb,
                apps.XXPI_ACC_UL_V        we
                where 1=1
                      and baw.bank_account_id = pn_acc_id
                      and baw.bank_branch_id = bb.bank_branch_id
                      and baw.org_id = 81
                      and baw.account_type = 'INTERNAL'
                      and we.flex_value = baw.attribute1
                      and we.flex_value_id = bh.pi_ul_fv_id(+);

  exception
    when others then
         errbuf := errbuf || ' ' || SQLERRM;
         retcode := 2;
         rollback;

         if err_info_cur%ISOPEN then
            close err_info_cur;
         end if;

         if internal_accid_cur%ISOPEN then
            close internal_accid_cur;
         end if;

  end prepare_err_info_byacc;

  -----------------------------------------------------------------------------
  -- формирования сводки ошибок загрузки по внутренним счетам
  -- для запуска из второго этапа загрузки выписки!!!
  -----------------------------------------------------------------------------
  procedure prepare_err_info_bystatement(errbuf    out nocopy varchar2,
                                         retcode   out nocopy number,
                                         p_log_id in number,
                                         p_log_entry_num in out number,
                                         p_user_id in number,
                                         p_req_id in number,
                                         p_file_id in number
                          ) is
    vd_beg_date DATE;
    vd_end_date DATE;
    vv_is1c VARCHAR2(1);
    beg_year NUMBER(4);
    end_year NUMBER(4);
    log_msg VARCHAR2(2000);
    v_errbuf VARCHAR2(2000);
    v_retcode NUMBER;

  begin
    select beg_date, end_date,  is_1c
    into vd_beg_date, vd_end_date, vv_is1c
    from xxpi_bk_imp_file
    where file_id = p_file_id;

    beg_year := to_number(to_char(vd_beg_date, 'yyyy'));
    end_year := to_number(to_char(vd_end_date, 'yyyy'));

    for ACC in (select distinct bank_acc_id, acc_num
                       from xxpi_bk_imp_accf
                            where file_id = p_file_id
                ) loop
      for Y in beg_year..end_year loop
          prepare_err_info_byacc(errbuf => errbuf,
                                retcode => retcode,
                                pn_yr => Y,
                                pn_acc_id => ACC.bank_acc_id,
                                pv_is1C => vv_is1c );
          if p_log_entry_num IS NOT NULL and p_log_id IS NOT NULL then
             p_log_entry_num := p_log_entry_num + 1;
             log_msg   := 'Сводка о состоянии загрузки по расч. счёту ' ||
                           ACC.acc_num || ' за ' || to_char(Y) || ' год завершена.';
             xxpi_bk_imp_step.sp_ins_log(p_file_id,
                         p_log_id,
                         2,
                         'SP_IMP_STEP',
                         p_req_id,
                         p_log_entry_num,
                         null,
                         log_msg,
                         'F',
                         p_user_id);
              if retcode > 0 then
                log_msg   := 'n_err=' || retcode || ', s_msg=' || 'Р.с. ' || ACC.acc_num ||':'|| errbuf;
                p_log_entry_num := p_log_entry_num + 1;
                xxpi_bk_imp_step.sp_ins_log(p_file_id,
                           p_log_id,
                           2,
                           'SP_IMP_STEP',
                           p_req_id,
                           p_log_entry_num,
                           null,
                           log_msg,
                           'E',
                           p_user_id);
              end if;



          end if;
      end loop;
      -- AMOMZYAKOV 15/04/2014
      declare
         is_daily_ctrl_on VARCHAR2(1);
      begin
        select DECODE(p.ctrl_start_date, NULL, 'N', DECODE(p.ctrl_end_date, NULL, 'Y', 'N') )
        into is_daily_ctrl_on
        from xxpi_acc_daily_ctrl p
        where bank_account_id = ACC.bank_acc_id;

        if is_daily_ctrl_on = 'Y' then

          prepare_daily_err_info(errbuf => v_errbuf, retcode => v_retcode, p_bank_account_id => ACC.bank_acc_id);

          if p_log_entry_num IS NOT NULL and p_log_id IS NOT NULL then
             p_log_entry_num := p_log_entry_num + 1;
             log_msg   := 'Сводка о состоянии ежедневной загрузки по расч. счёту ' ||
                           ACC.acc_num ||' завершена.';
             xxpi_bk_imp_step.sp_ins_log(p_file_id,
                         p_log_id,
                         2,
                         'SP_IMP_STEP',
                         p_req_id,
                         p_log_entry_num,
                         null,
                         log_msg,
                         'F',
                         p_user_id);
              if retcode > 0 then
                log_msg   := 'n_err=' || retcode || ', s_msg=' || 'Р.с. ' || ACC.acc_num ||':'|| errbuf;
                p_log_entry_num := p_log_entry_num + 1;
                xxpi_bk_imp_step.sp_ins_log(p_file_id,
                           p_log_id,
                           2,
                           'SP_IMP_STEP',
                           p_req_id,
                           p_log_entry_num,
                           null,
                           log_msg,
                           'E',
                           p_user_id);
              end if;

          end if;
        end if;
      exception
        WHEN NO_DATA_FOUND then NULL;
      end;
      -- end 15/04/2014
    end loop;

    retcode := 0;
    errbuf := 'ok';
  exception
    When others then
         retcode := 2;
         errbuf := SQLERRM;
  end prepare_err_info_bystatement;

  -----------------------------------------------------------------------------
  -- формирования сводки ошибок загрузки по внутренним счетам
  -- сервисная! параллельная прога на график!
  -----------------------------------------------------------------------------
  procedure prepare_err_info(errbuf    out nocopy varchar2,
                          retcode   out nocopy number,
                          pv_ul_1C_code in varchar2, -- код юр лица из 1С -- корректность кода проверяется в параллельной программе
                          pn_acc_id in varchar2, -- id расч счёта - м.б. NULL
                          pn_beg_yr in number, -- начальный год периода (включительно)
                          pn_end_yr in number, -- конечный год периода (включительно) - не позднее тек года!
                          pv_is1C in varchar2 -- источник (если из набора значений AP_OIE_YES_NO_MAND - 'Yes' или 'No')
                          ) IS
    CURSOR err_info_cur(p_acc_id number, p_yr number, p_src varchar2) IS -- проверка наличия строки со сводкой ошибок
           select bank_acc_id
                  from xxpi_bk_err_stats
                       where bank_acc_id = p_acc_id
                             and year = p_yr
                             and is_1c = p_src;
    tmp NUMBER;
     v_errbuf varchar2(2000);
     v_retcode number(1);
     vn_cnt number; -- количество обработанных строк типа счёт-год
     vn_cnt_acc number; -- количество обработанных счетов
     vn_cnt_err number; -- количество ошибок
     vn_request_id number;
     vv_is1c varchar2(1);
     vn_test_recs number;
  begin
     vn_test_recs := 5;

     vn_cnt := 0;
     vn_cnt_err := 0;
     vn_cnt_acc := 0;
     vn_request_id := FND_GLOBAL.CONC_REQUEST_ID;

     if vn_request_id > -1 then
        FND_FILE.put_line(FND_FILE.LOG, 'Текущий запрос: ' || to_char(vn_request_id));
     end if;

    if pn_beg_yr IS NULL or pn_end_yr IS NULL then
       retcode := 2;
       errbuf := 'Неверно задан период. Обработка невозможна.';
       RETURN;
    end if;

    if pn_beg_yr > pn_end_yr then
       retcode := 2;
       errbuf := 'Год начала периода больше года конца периода. Обработка невозможна.';
       RETURN;
    end if;

    if pv_is1c not in ('Y', 'N', 'Yes', 'No') then
       retcode := 2;
       errbuf := 'Неверно задан источник данных. Обработка невозможна.';
       RETURN;
    end if;

    vv_is1C := SUBSTR(pv_is1c, 1,1);

    -- если задан счёт - игнорируем юр лицо
    if pn_acc_id IS NOT NULL then
       -- резервируем записи в xxpi_bk_err_stats (для обтображения служ сообщения в формах)
       for y in pn_beg_yr..pn_end_yr LOOP
           open err_info_cur(pn_acc_id, y, 'Y');
           fetch err_info_cur into tmp;
           if err_info_cur%FOUND then
              update xxpi_bk_err_stats
                        set request_id = vn_request_id
                        where bank_acc_id = pn_acc_id
                              and year = y
                              and is_1c = 'Y';
              commit;
           else
              insert into xxpi_bk_err_stats(bank_acc_id, year, is_1c, request_id)
              values (pn_acc_id, y, 'Y', vn_request_id);
              commit;
           end if;
           close err_info_cur;
       end LOOP;

       vn_cnt_acc := vn_cnt_acc + 1;

       -- собственно делаем сводку
       for Y in pn_beg_yr..pn_end_yr LOOP
           prepare_err_info_byacc(errbuf => v_errbuf,
                                  retcode => v_retcode,
                                  pn_yr => Y,
                                  pn_acc_id => pn_acc_id,
                                  pv_is1C => vv_is1c);
           vn_cnt := vn_cnt + 1;
           if v_retcode = 2 then
              vn_cnt_err := vn_cnt_err + 1;
           end if;
           FND_FILE.put_line(FND_FILE.LOG, to_char(Y) || ' год: ' || v_errbuf);

       end LOOP;
    -- если не задан расч счёт и задано юр лицо, то перебираем все расч счета указанного юр лица
    elsif pv_ul_1C_code IS NOT NULL then
       -- резервируем записи в xxpi_bk_err_stats (для обтображения служ сообщения в формах)
       for Rec in (select
                     bank_account_id
                     from xxpi_internal_bank_accounts_v ba
                          where 1=1
                             and ba.attribute1 = pv_ul_1c_code
                             --and rownum <= vn_test_recs

                   ) LOOP
          vn_cnt_acc := vn_cnt_acc + 1;
          for y in pn_beg_yr..pn_end_yr LOOP
              open err_info_cur(Rec.bank_account_id, y, 'Y');
              fetch err_info_cur into tmp;
              if err_info_cur%FOUND then
                 update xxpi_bk_err_stats
                        set request_id = vn_request_id
                        where bank_acc_id = Rec.bank_account_id
                              and year = y
                              and is_1c = 'Y';
                 commit;
              else
                 insert into xxpi_bk_err_stats(bank_acc_id, year, is_1c, request_id)
                 values (Rec.bank_account_id, y, 'Y', vn_request_id);
                 commit;
              end if;
              close err_info_cur;
          end LOOP;
       end LOOP;
       -- собственно делаем сводку по расч счетам юр лица
       for Rec in (select
                     bank_account_id
                     from xxpi_internal_bank_accounts_v ba
                          where 1=1
                             and ba.attribute1 = pv_ul_1c_code
                             --and rownum <= vn_test_recs
                   ) LOOP

           -- перебираем годы заданного периода
           for Y in pn_beg_yr..pn_end_yr LOOP
               prepare_err_info_byacc(errbuf => v_errbuf,
                                  retcode => v_retcode,
                                  pn_yr => Y,
                                  pn_acc_id => Rec.bank_account_id,
                                  pv_is1C => vv_is1c);


               vn_cnt := vn_cnt + 1;
               if v_retcode = 2 then
                  vn_cnt_err := vn_cnt_err + 1;
               end if;
               FND_FILE.put_line(FND_FILE.LOG, to_char(Y) || ' год: ' || v_errbuf);

           end LOOP;
       end LOOP;
    end if;

    -- убираем request_id после завершения
    update xxpi_bk_err_stats
           set request_id = NULL
           where request_id = vn_request_id;
    commit;

    FND_FILE.put_line(FND_FILE.LOG, 'Обработано счетов: ' || to_char(vn_cnt_acc));
    FND_FILE.put_line(FND_FILE.LOG, 'Обработано строк: ' || to_char(vn_cnt));
    FND_FILE.put_line(FND_FILE.LOG, 'Ошибок обоработки: ' || to_char(vn_cnt_err));

    retcode := 0;
    errbuf := 'OK';

  exception
    when OTHERS then
         retcode := 2;
         errbuf := SQLERRM;

  end prepare_err_info;

  -----------------------------------------------------------------------------
  -- отчёт о состоянии загрузки данных по расчётным счетам
  -- по предварительно сформированной
  -----------------------------------------------------------------------------
    procedure err_report(errbuf     out nocopy varchar2,
                       retcode    out nocopy number) IS
    Ctx          DBMS_XMLGEN.ctxHandle; -- Var's to convert SQL output to XML
    l_xml_string VARCHAR2(32767);
    xml          CLOB;
    cnt          NUMBER;
    --//--
    l_clob_length  NUMBER;
    l_iterations   NUMBER;
    l_chunk_length NUMBER := 32767;
    vn_cur_year number(4);
    vv_cur_source varchar2(1) ;

  begin

    ctx := DBMS_XMLGEN.newContext(
                              'select ''Отчёт о состоянии загрузки'' title,
                                     DECODE(er.is_1c, ''N'', ''1С'', ''БК'') src,
                                     er.year,
                                     baw.bank_account_name,
                                     er.errors err_text,
                                     bh.buh_name,
                                     baw.bank_account_num || '' в '' ||
                                     bb.bank_name || '' '' ||
                                     bb.bank_branch_name ||
                                     ''БИК '' ||
                                     bb.eft_swift_code || '' '' ||
                                     DECODE(bb.attribute1, NULL, NULL, '' К/с '' || bb.attribute1) acc_full_info,
                                     to_char(NVL(er.last_update_date, er.creation_date), ''dd.mm.yyyy'') issue_date
                                     from apps.ap_bank_accounts_all baw,
                                          apps.XXPI_UL_BUH_V        bh,
                                          apps.ap_bank_branches     bb,
                                          apps.XXPI_ACC_UL_V        we,
                                          xxpi_bk_err_stats er--,
                                          --xxpi_bank_accounts_supervised bas
                                          where 1=1
                                                --and baw.bank_account_id = bas.bank_account_id
                                                --and bas.is_active = ''Y''
                                                and baw.bank_account_id = er.bank_acc_id
                                                and baw.bank_branch_id = bb.bank_branch_id
                                                and baw.org_id = 81
                                                and baw.account_type = ''INTERNAL''
                                                and we.flex_value = baw.attribute1
                                                and we.flex_value_id = bh.pi_ul_fv_id(+)
                                     order by 1, 2, 3'
                       );

    xml := DBMS_XMLGEN.getXML(Ctx);

    DBMS_XMLGEN.closeContext(Ctx);

    xml := replace(xml,
                   '<?xml version="1.0"?>',
                   '<?xml version = ''1.0'' encoding = ''iso-8859-5''?>');

    l_clob_length := dbms_lob.getlength(xml);

    l_iterations := CEIL(l_clob_length / l_chunk_length);

    FOR i IN 0 .. l_iterations - 1 LOOP

      l_xml_string := dbms_lob.substr(xml,
                                      l_chunk_length,
                                      i * l_chunk_length + 1);

      FND_FILE.PUT(FND_FILE.OUTPUT, l_xml_string);

    END LOOP;

    retcode := 0;
    errbuf := 'OK';

  exception
    when OTHERS then
         retcode := 2;
         errbuf := SQLERRM;
  end err_report;

  -----------------------------------------------------------------
  -- возвращает дату последней пятницы, предыдущей заданной дате
  -----------------------------------------------------------------
  function get_last_friday(p_date in date) return date is
    n number(1);
    n_fri number(1); -- для NLS по умолчанию в БД
  begin
    if n_user_id IS NOT NULL and n_user_id > 0 then
       n_fri := 5;-- если из-под oebs
    else
       n_fri := 6; -- если  в tool'e
    end if;
    n := to_number(to_char(p_date, 'D'));
    --FND_FILE.PUT_LINE(FND_FILE.LOG, 'Пор номер тек дня: ' || to_char(n));
    --DBMS_OUTPUT.put_line('Пор номер тек дня: ' || to_char(n));
    --DBMS_OUTPUT.put_line('Пор номер ПТН: ' || to_char(n_fri));
    if n > n_fri then
      return p_date - (n - n_fri);
    else
      return p_date - 7 + (n_fri - n);
    end if;
  end get_last_friday;

  -----------------------------------------------------------------
  -- пакетная сводка по всем РС за тек год
  -----------------------------------------------------------------
  procedure prep_err_info_batch(errbuf     out nocopy varchar2,
                       retcode    out nocopy number) is
    v_errbuf VARCHAR2(2000);
    v_retcode NUMBER(1);
    type src_type_list is table of VARCHAR2(1) index by PLS_INTEGER;
    va_src src_type_list;
    tmp varchar2(2000);
  begin
    va_src(1):='N'; va_src(2):='Y'; -- источнички
    for ACC in (SELECT
       BANK_ACCOUNT_ID,
       UL_PAY_GROUP,
       BANK_ACCOUNT_NAME,
       UL_NAME,
       BANK_NAME,
       BANK_ACCOUNT_NUM,
       IS_KL_BANK
       FROM XXPI_BK_IMP_BUH_V1
            WHERE 1 = 1
                  and bank_name not in ('Взаимозачеты', 'Вексели')
                  and day = trunc(SYSDATE)
                  and nvl(inactive_date, sysdate) >
                  trunc(SYSDATE) - 14
                ) loop
       for i in va_src.first..va_src.last loop
           if not (va_src(i)='N' AND ACC.IS_KL_BANK='N') then
              select 'Формируется сводка загрузки из '||(select DECODE(va_src(i), 'N', 'БК', '1С') from dual) ||
                     ' по р.с. ' || ACC.BANK_ACCOUNT_NAME ||' № ' || ACC.BANK_ACCOUNT_NUM
                          || ' за ' || to_char(sysdate, 'YYYY') ||'... ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss')
              into tmp
                   from dual;
              FND_FILE.PUT_LINE(FND_FILE.LOG, tmp);
              --DBMS_OUTPUT.put_line(tmp);

              prepare_err_info_byacc(errbuf => v_errbuf,
                              retcode => v_retcode,
                              pn_yr => to_number(to_char(sysdate, 'YYYY')), -- год не позднее текущего, не раньше 1900
                              pn_acc_id => ACC.bank_account_id, -- id внутр. расч счёта (тип - INTERNAL !!!)
                              pv_is1C => va_src(i));

              if v_retcode = 0 then
                 FND_FILE.PUT_LINE(FND_FILE.LOG, 'OK. ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
                 --DBMS_OUTPUT.put_line('OK ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
              else
                 FND_FILE.PUT_LINE(FND_FILE.LOG, 'ошибка: '|| v_errbuf ||' '|| to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
                 --DBMS_OUTPUT.put_line('ошибка: '|| v_errbuf ||' '|| to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
              end if;

              -- AMOMZYAKOV 24/03/2015 -- формирование сводки за прошлый год
              select 'Формируется сводка загрузки из '||(select DECODE(va_src(i), 'N', 'БК', '1C') from dual) ||
                     ' по р.с. ' || ACC.BANK_ACCOUNT_NAME ||' № ' || ACC.BANK_ACCOUNT_NUM
                          || ' за ' || to_char( to_number(to_char(sysdate, 'YYYY')) - 1) ||'... ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss')
              into tmp
                   from dual;
              FND_FILE.PUT_LINE(FND_FILE.LOG, tmp);
              --DBMS_OUTPUT.put_line(tmp);

              prepare_err_info_byacc(errbuf => v_errbuf,
                              retcode => v_retcode,
                              pn_yr => to_number(to_char(sysdate, 'YYYY')) - 1, -- за передыдущий год
                              pn_acc_id => ACC.bank_account_id, -- id внутреннего расч. счёта (тип - INTERNAL !!!)
                              pv_is1C => va_src(i));

              if v_retcode = 0 then
                 FND_FILE.PUT_LINE(FND_FILE.LOG, 'OK. ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
                 --DBMS_OUTPUT.put_line('OK ' || to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
              else
                 FND_FILE.PUT_LINE(FND_FILE.LOG, 'ошибка: '|| v_errbuf ||' '|| to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
                 --DBMS_OUTPUT.put_line('ioeaea: '|| v_errbuf ||' '|| to_char(sysdate, 'dd.mm.yyyy hh24:mi:ss'));
              end if;
              -- end 24/03/2015
           end if;
       end loop;
    end loop;
    retcode := 0;
    errbuf := 'prep_err_info_batch: ok';
  exception
    when others then
      retcode := 2; errbuf := substr(SQLERRM, 1, 2000);
  end prep_err_info_batch;

  -----------------------------------------------------------------------------
  -- проверка периода действия счёта и корректировка периода сводки
  -----------------------------------------------------------------------------
  procedure update_period(pn_acc_id IN NUMBER,
                          pd_beg_date IN OUT DATE,
                          pd_end_date IN OUT DATE) IS
    vd_open_date DATE;
    vd_close_date DATE;
  begin

    select to_date(ba.attribute6, 'YYYY/MM/DD hh24:mi:ss'), inactive_date
           into vd_open_date, vd_close_date
           from ap_bank_accounts_all ba
                where bank_account_id = pn_acc_id;

    if vd_open_date IS NOT NULL and vd_open_date > pd_beg_date then
       pd_beg_date := vd_open_date;
    end if;

    if vd_close_date IS NOT NULL and vd_close_date < pd_end_date then
       pd_end_date := vd_close_date;
    end if;

  end update_period;

  -----------------------------------------------------------------------------
  -- утилита запуска простых SQL-отчётов из параллельных прог
  -----------------------------------------------------------------------------
  procedure run_concurrent_report(errbuf     out nocopy varchar2,
                       retcode    out nocopy number,
                       p_query in VARCHAR2 -- полностью подготовленная к выполнению ( с необх. параметрами) строка запроса
                       ) IS
    Ctx          DBMS_XMLGEN.ctxHandle; -- Var's to convert SQL output to XML
    l_xml_string VARCHAR2(32767);
    xml          CLOB;
    cnt          NUMBER;
    --//--
    l_clob_length  NUMBER;
    l_iterations   NUMBER;
    l_chunk_length NUMBER := 32767;
    vn_cur_year number(4);
    vv_cur_source varchar2(1) ;

  begin

    ctx := DBMS_XMLGEN.newContext(p_query);
    xml := DBMS_XMLGEN.getXML(Ctx);
    DBMS_XMLGEN.closeContext(Ctx);
    xml := replace(xml,
                   '<?xml version="1.0"?>',
                   '<?xml version = ''1.0'' encoding = ''iso-8859-5''?>');
    l_clob_length := dbms_lob.getlength(xml);
    l_iterations := CEIL(l_clob_length / l_chunk_length);

    FOR i IN 0 .. l_iterations - 1 LOOP
      l_xml_string := dbms_lob.substr(xml,
                                      l_chunk_length,
                                      i * l_chunk_length + 1);
      FND_FILE.PUT(FND_FILE.OUTPUT, l_xml_string);
    END LOOP;

    retcode := 0;
    errbuf := 'OK';

  exception
    when OTHERS then
         retcode := 2;
         errbuf := SQLERRM;
  end run_concurrent_report;

  -----------------------------------------------------------------------------
  -- 16/06/2014 утилита запуска простых SQL-отчётов из параллельных прог -
  -- с ручным формированием сообщения и аттачем к нему
  -----------------------------------------------------------------------------
  procedure run_concurrent_report_attache(errbuf     out nocopy varchar2,
                       retcode    out nocopy number,
                       p_query in VARCHAR2 -- полностью подготовленная к выполнению ( с необх. параметрами) строка запроса
                       ) IS
    Ctx          DBMS_XMLGEN.ctxHandle; -- Var's to convert SQL output to XML
    l_xml_string VARCHAR2(32767);
    xml          CLOB;
    cnt          NUMBER;
    --//--
    l_clob_length  NUMBER;
    l_iterations   NUMBER;
    l_chunk_length NUMBER := 32767;
    vn_cur_year number(4);
    vv_cur_source varchar2(1) ;
    -- max длина мени файла в MAIL_PKG.ADD_ATTACHMENT - 30 символов !!!
    l_file_name varchar2(300) := 'unmatched_bk_docs_rep_.xml';
    l_file_name_fin varchar2(300) := 'unmch_paym_rep_' || to_char(SYSDATE, 'dd_mm_yyyy') || '.xml';

    v_file sys.utl_file.file_type;
    v_file_fin sys.utl_file.file_type; -- откорректированный выходной файл

  begin

    ctx := DBMS_XMLGEN.newContext(p_query);
    xml := DBMS_XMLGEN.getXML(Ctx);
    DBMS_XMLGEN.closeContext(Ctx);

    v_file:=utl_file.fopen(location => 'BK_IMPORT',filename => l_file_name,open_mode => 'w',max_linesize => 32767);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Открыт файл: '||l_file_name );

    utl_file.put_line(file => v_file,
                  buffer => '<?xml version="1.0" encoding="iso-8859-5" standalone="yes"?>');

    xml := replace(xml,
                   '<?xml version="1.0"?>',
                   '');

    --               '<?xml version = ''1.0'' encoding = ''iso-8859-5''?>');

    l_clob_length := dbms_lob.getlength(xml);
    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Длина файла- '||l_clob_length);
    l_iterations := CEIL(l_clob_length / l_chunk_length);
    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Итераций- '||l_iterations);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Переменные цикла инициализированы.' );

    FOR i IN 0 .. l_iterations - 1 LOOP
      FND_FILE.PUT_LINE(FND_FILE.LOG, 'шаг i='||i);
      l_xml_string := dbms_lob.substr(xml,
                                      l_chunk_length,
                                      i * l_chunk_length + 1);
      FND_FILE.PUT_LINE(FND_FILE.LOG, 'длина l_xml_string='||length(l_xml_string));

      FND_FILE.PUT(FND_FILE.OUTPUT, l_xml_string);

      utl_file.put_raw(v_file, utl_raw.cast_to_raw(l_xml_string) );

      sys.utl_file.fclose(v_file);
      v_file:=utl_file.fopen(location => 'BK_IMPORT',filename => l_file_name,open_mode => 'a', max_linesize => 32767);

    END LOOP;

    --utl_file.new_line(file => v_file);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Запись в файл завершена.' );

    sys.utl_file.fclose(v_file);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Файл закрыт.' );

    -- удаление лишних переносов строк внутри тегов и в данных
    DECLARE
       tmp RAW(1); -- байтег
       prev_tmp RAW(1); -- для идентификации открывающего тэга
       bytes_aft_cl_br PLS_INTEGER :=0; --количесво прочитанных байт после символа '>'
       in_tag BOOLEAN := FALSE;
       in_data BOOLEAN := FALSE;
       in_closing_tag BOOLEAN := FALSE;
    BEGIN
    v_file_fin := utl_file.fopen(location => 'BK_IMPORT', filename => l_file_name_fin, open_mode => 'w', max_linesize => 32767);
    v_file := utl_file.fopen(location => 'BK_IMPORT', filename => l_file_name, open_mode => 'r', max_linesize => 32767);
    utl_file.fclose(v_file_fin);
    v_file_fin := utl_file.fopen(location => 'BK_IMPORT',filename => l_file_name_fin,open_mode => 'a',max_linesize => 32767);
    loop
      UTL_FILE.GET_RAW (v_file, tmp, 1);

      if tmp=utl_raw.cast_to_raw('<') then
         in_tag := TRUE;
         in_data := FALSE;
      elsif tmp=utl_raw.cast_to_raw('>') then

         in_tag := FALSE;
         if in_closing_tag then
            in_data := FALSE;
         else
            in_data :=TRUE;
         end if;

      end if;

      -- если вошли в закрывающий тэг - мы внутри закрыв. тэга
      if prev_tmp=utl_raw.cast_to_raw('<') and tmp=utl_raw.cast_to_raw('/') then
         in_closing_tag := TRUE;
      end if;
      -- если вошли в открывающий тэг - мы внутри открыв. тэга
      if prev_tmp=utl_raw.cast_to_raw('<') and tmp<>utl_raw.cast_to_raw('/') then
         in_closing_tag := FALSE;
      end if;


      if in_tag then
         if tmp <> utl_raw.cast_to_raw(chr(10)) and tmp <> utl_raw.cast_to_raw(chr(13)) then
            UTL_FILE.PUT_RAW(v_file_fin, tmp);
         else
            FND_FILE.PUT_LINE(FND_FILE.LOG, 'Удалён перенос внутри тэга.' );
         end if;
      else
         -- после прочтения открывающего тэга тоже удаляем все переносы из данных
         if in_data then
            if tmp <> utl_raw.cast_to_raw(chr(10)) and tmp <> utl_raw.cast_to_raw(chr(13)) then
               UTL_FILE.PUT_RAW(v_file_fin, tmp);
            else
               FND_FILE.PUT_LINE(FND_FILE.LOG, 'Удалён перенос внутри данных.' );
            end if;
         else
            UTL_FILE.PUT_RAW(v_file_fin, tmp);
         end if;
      end if;

      prev_tmp := tmp;

    end loop;

    EXCEPTION
      WHEN NO_DATA_FOUND then
         utl_file.fclose(v_file_fin);
         utl_file.fclose(v_file);
    END;

    -- конец удаление лишних переносов строк внутри тегов

    MAIL_PKG.ADD_ATTACHMENT(dirname  => 'BK_IMPORT', filename => l_file_name_fin);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Вложение прикреплено.' );

    MAIL_PKG.SEND(mailto        =>  replace(replace('anikolaev@kuku.ru','>'),'<')
                 ,subject       => 'Отчёт о платежах, невыверенных при загрузке выписки из клиент-банка на '||to_char(sysdate,'dd.mm.yyyy hh24:mi:ss')
                 ,message       => 'Открыть вложение в MS Excel как XML-таблицу.'
                 ,mailfrom      => '<tvu@kuku.ru>'
                 ,mimetype      => 'text/html'
                 ,sendername    => 'OEBS'
                 ,recipientname => ''
                 ,copy_name     => ''
                 ,p_SPAM_NAMES  => '<anikolaev@kuku.ru>,<amomzyakov@kuku.ru>'
                 --,p_SPAM_NAMES  => '<amomzyakov@kuku.ru>,<tvu@kuku.ru>'
                 ,P_FRST_NM     => '<anikolaev@kuku.ru>');
                 --,P_FRST_NM     => '<amomzyakov@kuku.ru>');

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Сообщение отправлено.' );

    UTL_FILE.FREMOVE (location => 'BK_IMPORT',
                     filename => l_file_name);
    UTL_FILE.FREMOVE (location => 'BK_IMPORT',
                     filename => l_file_name_fin);

    FND_FILE.PUT_LINE(FND_FILE.LOG, 'Файлы удалены.' );

    retcode := 0;
    errbuf := 'OK';

  exception
    when OTHERS then
         retcode := 2;
         errbuf := SQLERRM;
  end run_concurrent_report_attache;

  -----------------------------------------------------------------------------
  -- Очёт по первоначально невыверенным (во время загрузки БВ) строкам БВ
  -- Для Овчинникова и его бойцов
  -----------------------------------------------------------------------------
  procedure bk_initially_not_matched_rep(errbuf out nocopy varchar2,
                                         retcode out nocopy number
                                         ) is
    v_query VARCHAR2(32000);
  begin

    v_query := '
            select to_char(doc_date, ''dd.mm.yyyy'') doc_date_,
                   doc_num doc_num,
                   replace(to_char(amount, ''9999999999999D00''), ''.'', '','') amount,
                   currency_code,
                   to_char(bank_date, ''dd.mm.yyyy'') bank_date,
                   doc_sequence_value,
                   from_name1,
                   to_name1,
                   description,
                   to_vendor_name,
                   from_bank_account_name,
                   '''''''' || from_acc_num from_acc_num,
                   to_bank_account_name,
                   '''''''' || to_acc_num to_acc_num,
                   from_bank1,
                   to_bank1,
                   '''''''' || from_bik from_bik,
                   '''''''' || from_ks from_ks,
                   from_bank2,
                   '''''''' || from_inn from_inn,
                   '''''''' || from_kpp from_kpp,
                   '''''''' || to_bik to_bik,
                   '''''''' || to_ks to_ks,
                   to_bank2,
                   '''''''' || to_inn to_inn,
                   '''''''' || to_kpp to_kpp,
                   transfer_priority,
                   vid_pl,
                   DECODE(is_budjet, ''Y'', ''Бюдж.'', '''') is_budjet,
                   mch_status,
                   initial_mch_status
            from XXPI_BK_INITIAL_NOMATCH_REP_V p
            where 1=1
            and p.initial_mch_status IS not NULL
            and (p.mch_status<>p.initial_mch_status or p.mch_status=''UNMATCHED'')
            and p.amount_IN_RUB > 10000
            order by doc_date
    ';

    run_concurrent_report_attache(errbuf, retcode, v_query);

  end bk_initially_not_matched_rep;

  ------------------------------------------------------------------------------
  -- доб 15.04.2014
  -- генерация сообщения об ошибках загрузки для Рс с заданным открытым периодом
  -- ежедневной загрузки и сохранение в табличге xxpi_acc_daily_ctrl
  ------------------------------------------------------------------------------
  procedure prepare_daily_err_info(errbuf out VARCHAR2, retcode out NUMBER, p_bank_account_id IN NUMBER)
  is
    is_daily_ctrl_on VARCHAR2(1);
    v_ctrl_start_date DATE;
    v_errors VARCHAR2(2000);
  begin
    select DECODE(p.ctrl_start_date, NULL, 'N', DECODE(p.ctrl_end_date, NULL, 'Y', 'N') ),
           DECODE(sliding_period_ctrl, 'Y', trunc(SYSDATE) - sliding_period + 1, ctrl_start_date)
    into is_daily_ctrl_on,
         v_ctrl_start_date
    from xxpi_acc_daily_ctrl_v p
    where bank_account_id = p_bank_account_id;

    v_errors := acc_errors_info(pd_from_date => v_ctrl_start_date - 1,  -- дата начала периода
                           pd_to_date => SYSDATE - 1, -- дата окончания периода
                           pn_acc_id => p_bank_account_id, -- id внутр. расч счёта (тип - INTERNAL !!!)
                           pv_is1C => 'N'-- источник (1с - Y, бк- N)
                           );

    if v_errors IS NOT NULL and v_errors not like '%O.K.%' and v_errors not like '%н.д.%' then
       v_errors := v_errors || ' ПЕРИОД ПРОВЕРКИ: с '|| to_char(v_ctrl_start_date-1, 'dd.mm.yyyy') || ' по '|| to_char(sysdate-1, 'dd.mm.yyyy');
    end if;

    if is_daily_ctrl_on = 'Y' then
       update xxpi_acc_daily_ctrl
       set errors = v_errors
       where bank_account_id = p_bank_account_id;
    end if;

    if SQL%FOUND then
       retcode := 0;
       errbuf :='';
    else
       retcode := 1;
       errbuf := 'РС id='|| p_bank_account_id ||'не существует или не отмечен для ежедневного контроля.';
    end if;

    commit;

  exception
    WHEN NO_DATA_FOUND then
         retcode := 0;
         errbuf := '';
    WHEN OTHERS then
         retcode := 2;
         errbuf := SQLERRM;
         rollback;
  end prepare_daily_err_info;

  ------------------------------------------------------------------------------
  -- Формирование диагностических сообщений о состоянии загрузки за неделю для РС с еженедельной
  -- загрузкой.
  ------------------------------------------------------------------------------

  procedure prepare_weekly_err_info(errbuf out VARCHAR2, retcode out NUMBER, p_bank_account_id IN NUMBER)
  is
    is_weekly_ctrl_on VARCHAR2(1);
    v_ctrl_start_date DATE;
    v_errors VARCHAR2(2000);
    curr_date DATE;
    date_last_thu DATE;
    n_thu NUMBER(1);
    n NUMBER(1);
  begin
    select p.bank_account_ctrl
           into is_weekly_ctrl_on
           from xxpi_acc_daily_ctrl_v p
           where bank_account_id = p_bank_account_id;

    -- вычислить период - предыдущая неделя до четверга включительно
    -- 1. тек. день недели
    curr_date := SYSDATE;

    -- определяем порядковый номер четверга
    if n_user_id IS NOT NULL and n_user_id > 0 then
       n_thu := 4;-- если из-под oebs
    else
       n_thu := 5; -- если  в tool'e
    end if;

    n := to_number(to_char(curr_date, 'D')); -- тек день недели

    if n > n_thu then
       date_last_thu := trunc(curr_date) - ( n - n_thu );
    else
       date_last_thu := trunc(curr_date) - 7 + ( n_thu - n);
    end if;

    v_errors := acc_errors_info(pd_from_date => date_last_thu - 6,  -- дата начала периода
                           pd_to_date => date_last_thu, -- дата окончания периода
                           pn_acc_id => p_bank_account_id, -- id внутр. расч счёта (тип - INTERNAL !!!)
                           pv_is1C => 'N'-- источник (1с - Y, бк- N)
                           );

    if v_errors IS NOT NULL and v_errors not like '%O.K.%' and v_errors not like '%н.д.%' then
       v_errors := v_errors || ' ПЕРИОД ПРОВЕРКИ: с '|| to_char(date_last_thu - 7, 'dd.mm.yyyy') || ' по '|| to_char(date_last_thu, 'dd.mm.yyyy');
    end if;

    if is_weekly_ctrl_on = 'Y' then
       update xxpi_acc_daily_ctrl
       set errors = v_errors
       where bank_account_id = p_bank_account_id;
    end if;

    if SQL%FOUND then
       retcode := 0;
       errbuf :='';
    else
       retcode := 1;
       errbuf := 'РС id='|| p_bank_account_id ||'не существует или не отмечен для ежедневного контроля.';
    end if;

    commit;

  exception
    WHEN NO_DATA_FOUND then
         retcode := 0;
         errbuf := '';
    WHEN OTHERS then
         retcode := 2;
         errbuf := SQLERRM;
         rollback;
  end prepare_weekly_err_info;

  -----------------------------------------------------------------------------
  -- Формирование сводки по состоянию загрузки по всем РС с ежедневной загрузкой
  -- для запуска из параллельной программы
  -----------------------------------------------------------------------------
  procedure prepare_daily_info_batch(errbuf out nocopy VARCHAR2, retcode out nocopy NUMBER)
  IS
     v_errbuf VARCHAR2(4000);
     v_retcode NUMBER;
  begin
    for rec in (select bank_account_id,
                       bank_branch_id,
                       ul_code,
                       ul_name,
                       bank_name,
                       bik,
                       bank_account_name,
                       bank_account_num,
                       ctrl_start_date,
                       ctrl_end_date
                from xxpi_acc_daily_ctrl_v
                where bank_account_ctrl = 'Y'
                ) loop
       prepare_daily_err_info(errbuf => v_errbuf,
                              retcode => v_retcode,
                              p_bank_account_id => rec.bank_account_id);
       FND_FILE.PUT_LINE(FND_FILE.LOG, 'Обработка РС ' || rec.bank_account_name || ' (№ '||rec.bank_account_num ||' БИК '||rec.bik||'): ' );
       if v_retcode > 0 then
          FND_FILE.put_line(FND_FILE.LOG, '.... Ошибка: ' || v_errbuf);
       else
          FND_FILE.put(FND_FILE.LOG, '.... O.K.');
          FND_FILE.new_line(FND_FILE.LOG);
       end if;
    end loop;
  end prepare_daily_info_batch;

  -----------------------------------------------------------------------------
  -- Формирование сводки по состоянию загрузки счетов в еженедельном режиме
  -- для запуска из паралледьной программы
  -----------------------------------------------------------------------------
  procedure prepare_weekly_info_batch(errbuf out nocopy VARCHAR2, retcode out nocopy NUMBER)
  IS
     v_errbuf VARCHAR2(4000);
     v_retcode NUMBER;
  begin
    for rec in (select bank_account_id,
                       bank_branch_id,
                       ul_code,
                       ul_name,
                       bank_name,
                       bik,
                       bank_account_name,
                       bank_account_num,
                       ctrl_start_date,
                       ctrl_end_date
                from xxpi_acc_daily_ctrl_v
                where bank_account_ctrl = 'Y'
                ) loop
       prepare_weekly_err_info(errbuf => v_errbuf,
                              retcode => v_retcode,
                              p_bank_account_id => rec.bank_account_id);
       FND_FILE.PUT_LINE(FND_FILE.LOG, 'Ошибка РС ' || rec.bank_account_name || ' (№ '||rec.bank_account_num ||' БИК '||rec.bik||'): ' );
       if v_retcode > 0 then
          FND_FILE.put_line(FND_FILE.LOG, '.... Ошибка: ' || v_errbuf);
       else
          FND_FILE.put(FND_FILE.LOG, '.... O.K.');
          FND_FILE.new_line(FND_FILE.LOG);
       end if;
    end loop;
  end prepare_weekly_info_batch;

  procedure run_prepare_daily_info_batch(req_id out NUMBER) is

  begin
    req_id := fnd_request.submit_request(application=>'SQLAP', program=>'XXPI_BK_DAILY_INFO_BATCH', sub_request => FALSE);

  end;

  procedure run_prepare_weekly_info_batch(req_id out NUMBER) is

  begin
    req_id := fnd_request.submit_request(application=>'SQLAP', program=>'XXPI_BK_WEEKLY_INFO_BATCH', sub_request => FALSE);

  end;

-- диагностика загрузок БВ с учетом периодов блокировки
function acc_errors_info_unblocked(pd_from_date in date, pd_to_date in date, pn_acc_id in number, pv_is1C in varchar2,
                           p_break varchar2 default 'N')
           return varchar2 is
    vv_acc_name VARCHAR2(80);
    result VARCHAR2(2000);
    -- для правильных результатов надо отсечь время от параметров - дат!!!
    vd_from_date DATE := TRUNC(pd_from_date);
    vd_to_date DATE := TRUNC(pd_to_date);
  begin

    result := '';

    begin
      select bank_account_name into vv_acc_name
           from
               ap_bank_accounts_all
               where
                    bank_account_id = pn_acc_id
                    and account_type = 'INTERNAL';
    exception
      when NO_DATA_FOUND then
           result := 'Счёт не существует или не является внутренним.';
           RETURN result;
    end;

    for Rec in (
               select 'Нет расчёта оборотов и сальдо в ' || count(*) || ' днях.' s, count(*) cnt, 1 ordr
                      from (
                           select vd_from_date + level - 1 bank_date
                                  from dual
                                       connect by level <= vd_to_date - vd_from_date + 1
                           MINUS
                           select bank_date
                                  from xxpi_osv
                                       where bank_date >= vd_from_date
                                             and bank_date <= vd_to_date
                                             and is_1c = pv_is1C
                                             and bank_acc_id = pn_acc_id
                           MINUS -- дни с билокировкой
                           (select distinct dd.bank_date  --, l.lock_from_date, l.lock_to_date
                                               from
                                                   (select vd_from_date + level - 1 bank_date
                                                           from dual
                                                           connect by level <= vd_to_date - vd_from_date + 1
                                                   ) dd,
                                                   (select *
                                                           from xxpi_acc_locks
                                                           where bank_account_id = pn_acc_id
                                                                 and deleted_flg = 'N') l
                                               where dd.bank_date >= nvl(l.lock_from_date, dd.bank_date)
                                                      and dd.bank_date <= nvl(l.lock_to_date, dd.bank_date)
                                                      and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                                                      and pv_is1C = 'N'
                           ) -- дни с блокировкой
                           ) q
               UNION
               SELECT 'Не загружены данные выписки в ' || count(*)||' днях.' s, count(*) cnt, 2 ordr
                      FROM (select bank_date from xxpi_osv_v
                                    WHERE 1 = 1
                                         and bank_date >= vd_from_date
                                         and bank_date <= vd_to_date
                                         AND bank_acc_id = pn_acc_id
                                         AND is_1c = pv_is1c
                                         and (
                                             -- нет загруженных из выписке данных
                                             file_id IS NULL
                                             )
                           MINUS -- дни с билокировкой
                           (select distinct dd.bank_date  --, l.lock_from_date, l.lock_to_date
                                               from
                                                   (select vd_from_date + level - 1 bank_date
                                                           from dual
                                                           connect by level <= vd_to_date - vd_from_date + 1
                                                   ) dd,
                                                   (select *
                                                           from xxpi_acc_locks
                                                           where bank_account_id = pn_acc_id
                                                                 and deleted_flg = 'N') l
                                               where dd.bank_date >= nvl(l.lock_from_date, dd.bank_date)
                                                      and dd.bank_date <= nvl(l.lock_to_date, dd.bank_date)
                                                      and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                                                      and pv_is1C = 'N'
                           ) -- дни с блокировкой
                           )
               UNION
               SELECT 'Нет данных по оборотам и сальдо выписки в ' || count(*)||' днях.' s, count(*) cnt, 3 ordr
                      FROM (select bank_date from xxpi_osv_v
                           -- AMOMZYAKOV 01.08.2013
                           ,
                           (select aa.bank_account_id, bb.attribute13
                                       from ap_bank_accounts_all aa,
                                            ap_bank_branches bb
                                            where bb.bank_branch_id = aa.bank_branch_id
                           ) aa
                           -- 01.08.2013
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and aa.bank_account_id(+) = bank_acc_id -- 01.08.2013
                                 and (
                                     -- кривые выписке!
                                     file_id IS NOT NULL
                                     and f_file_id IS NULL
                                     -- AMOMZYAKOV 01.08.2013
                                     and (aa.attribute13 IS NULL or
                                         (aa.attribute13 IS NOT NULL and aa.attribute13<>'Y') )
                                     -- 01.08.2013
                                     )
                           MINUS -- заблокированные даты КБ
                           (select distinct dd.bank_date  --, l.lock_from_date, l.lock_to_date
                                               from
                                                   (select vd_from_date + level - 1 bank_date
                                                           from dual
                                                           connect by level <= vd_to_date - vd_from_date + 1
                                                   ) dd,
                                                   (select *
                                                           from xxpi_acc_locks
                                                           where bank_account_id = pn_acc_id
                                                                 and deleted_flg = 'N') l
                                               where dd.bank_date >= nvl(l.lock_from_date, dd.bank_date)
                                                      and dd.bank_date <= nvl(l.lock_to_date, dd.bank_date)
                                                      and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                                                      and pv_is1C = 'N'
                           ) -- заблокированные даты КБ
                           )
               UNION
               SELECT 'Расхождения входящих и исходящих cальдо - в' || count(*)||' днях.' s, count(*) cnt, 4 ordr
                      FROM (select bank_date from xxpi_osv_v
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and (
                                     -- расхождения вх и исх сальдо
                                     (beg_balance_null is not null and
                                     prev_end_balance <> beg_balance_null) or
                                     (end_balance_null is not null and
                                     next_beg_balance <> end_balance_null)
                                     )
                           MINUS -- заблокированные даты КБ
                           (select distinct dd.bank_date  --, l.lock_from_date, l.lock_to_date
                                               from
                                                   (select vd_from_date + level - 1 bank_date
                                                           from dual
                                                           connect by level <= vd_to_date - vd_from_date + 1
                                                   ) dd,
                                                   (select *
                                                           from xxpi_acc_locks
                                                           where bank_account_id = pn_acc_id
                                                                 and deleted_flg = 'N') l
                                               where dd.bank_date >= (nvl(l.lock_from_date, dd.bank_date) - 1) -- предыдущий периоду блок-ки день не в счет
                                                      and dd.bank_date <= (nvl(l.lock_to_date, dd.bank_date) + 1)  -- следующий после периода блок-ки день не в счет
                                                      and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                                                      and pv_is1C = 'N'
                           ) -- заблокированные даты КБ
                           )
               UNION
               SELECT 'Расхождения расчётных сальдо с выписками - в' || count(*)||' днях.' s, count(*) cnt, 5 ordr
                      FROM (select bank_date from xxpi_osv_v
                           WHERE 1 = 1
                                 and bank_date >= vd_from_date
                                 and bank_date <= vd_to_date
                                 AND bank_acc_id = pn_acc_id
                                 AND is_1c = pv_is1c
                                 and (
                                     -- расхождения сальдо с выпискаме
                                     (beg_balance_null is not null and
                                     -- AMOMZYAKOV 05/06/2012: не включаем в сводку расх-я с сальдо перегруженных выписок
                                     --beg_balance_null <> nvl(f_beg_balance, beg_balance_null)) or
                                     beg_balance_null <> nvl(f_beg_balance, beg_balance_null) and f_file_id=f_beg_file_id ) or
                                     (end_balance_null is not null and
                                     -- AMOMZYAKOV 05/06/2012: не включаем в сводку расх-я с сальдо перегруженных выписок
                                     --end_balance_null <> nvl(f_end_balance, end_balance_null))
                                     end_balance_null <> nvl(f_end_balance, end_balance_null) and f_file_id=f_end_file_id)
                                     )
                           MINUS -- заблокированные даты КБ
                           (select distinct dd.bank_date  --, l.lock_from_date, l.lock_to_date
                                               from
                                                   (select vd_from_date + level - 1 bank_date
                                                           from dual
                                                           connect by level <= vd_to_date - vd_from_date + 1
                                                   ) dd,
                                                   (select *
                                                           from xxpi_acc_locks
                                                           where bank_account_id = pn_acc_id
                                                                 and deleted_flg = 'N') l
                                               where dd.bank_date >= nvl(l.lock_from_date, dd.bank_date)
                                                      and dd.bank_date <= nvl(l.lock_to_date, dd.bank_date)
                                                      and not (l.lock_from_date IS NULL and l.lock_to_date IS NULL)
                                                      and pv_is1C = 'N'
                           ) -- заблокированные даты КБ
                           )
               order by ordr

               ) loop

       if Rec.cnt > 0 then
          if nvl(length(result), 0) = 0 then
             result := Rec.s;
          else
             if p_break = 'Y' then
                result :=  result || '; ' || CHR(10) || Rec.s;
             else
                result :=  result || '; ' || Rec.s;
             end if;
          end if;
       end if;
    end loop;

    if nvl(length(result), 0)=0 then
       result := 'O.K.';
    end if;

    return result;

  end acc_errors_info_unblocked;

  -- Процедура формирования напоминания - предупреждение о приближении срока загрузки
  -- срок уведомления - каждый четверг и каждый вторник вечером,
  --   срок загрузки - каждая пятница -понедельник для счетов, загружаемых еженедельно,
  -- - уведомление в последний день каждого месяца в 20.00 для остальных РС

  -- Запускается из 2-х парал. программ
  -- - одна - для еженедельной загрузки - с параметром is_weekly='Y',
  -- - другая - для ежемесячной загрузки - с параметром is_weekly='N'
  procedure send_bk_reminder(errbuf     out nocopy varchar2,
                       retcode    out nocopy number,
                       is_weekly varchar2 default 'N' -- признак еженедельной загрузки для соотв. РС
                       ) is
    v_msg_subject varchar2(300);
    v_msg_body varchar2(4000);
    v_msg_body_test varchar2(4000);
    v_msg_header varchar2(4000);
    v_msg_row varchar2(4000);
    to_send boolean := FALSE;
    cnt number;
    l number := 0;
    v_MSG_ID number(15);
    v_msg_code varchar2(15) := '1C_reminder';
    is_msg_overflow boolean;
    part number;
    is_debug varchar2(1) := 'N';
    n_thu NUMBER(1); -- порядковый номер четверга в зависимости от настроек NLS
    n_tue NUMBER(1); -- ii?yaeiaue iiia? ?aoaa?aa a caaeneiinoe io iano?iae NLS
    n NUMBER(1);
    date_last DATE; -- последний день месяца
    date_last_thu DATE; -- крайний четверг (в т.ч. текущий день)
    curr_date DATE; -- текущая дата - дата и время запуска.

  procedure ins_msg(p_msg_subject in varchar2,
                    p_msg_body in varchar2,
                    p_rcpt_user_id in number,
                    p_rcpt_fio in varchar2,
                    p_rcpt_email_address in varchar2,
                    is_debug in varchar2 default 'Y') is

    v_msg_body varchar2(4000);
    v_msg_id number;
    sender_user_id NUMBER;
    sender_person_id NUMBER;
    rcpt_person_id NUMBER;

  begin

    v_msg_body := p_msg_body;

    v_msg_body := v_msg_body || '</table>';
    v_msg_body := v_msg_body || '<BR>--<BR><BR> Письмо сформировано OEBS автоматически. Не отвечайте на него.';
    v_msg_body := v_msg_body || '</body></html>';

    select XXPI_AP_MSG_ID_SEQ.NEXTVAL into v_MSG_ID from dual;

    begin
      select u.user_id, u.employee_id into sender_user_id, sender_person_id
      from fnd_user u
      where user_name = 'V@@@@@@@@';
    exception
      when no_data_found then
        sender_user_id := NULL;
        sender_person_id :=NULL;
    end;

    begin
      select u.employee_id into rcpt_person_id
      from fnd_user u
      where user_id = decode(is_debug, 'N', p_rcpt_user_id, 6568);
    exception
      when no_data_found then
        sender_user_id := NULL;
        sender_person_id :=NULL;
    end;

    insert into XXPI_AP_MSG
      (MSG_ID,
       CODE_NAME,
       SENDER_COMENT,
       CREATED_DATE,
       sender_user_id--,
       --sender_person_id
       )
    values
      (v_MSG_ID,
       '1C_reminder',
       v_msg_body,
       sysdate,
       sender_user_id--,
       --sender_person_id
       );

     insert into xxpi_msg_subject
       (msg_id,
       msg_subject)
     values
       (v_msg_id,
       p_msg_subject);

    insert into XXPI_AP_MSG_RCPT
      (MSG_ID,
       USER_ID,
       RCPT_FIO,
       RCPT_EMAIL,
       MRC_ID
       , person_id )
    values
      (v_MSG_ID,
       decode(is_debug, 'Y', 6568, p_rcpt_user_id),
       decode(is_debug, 'Y', 'Момзякофф', p_rcpt_fio),
       decode(is_debug, 'Y', 'amomzyakov@kuku.ru',p_rcpt_email_address),
       XXPI_AP_MSG_RCPT_SEQ.nextval,
       rcpt_person_id);

    commit;

  end ins_msg;


  begin

    curr_date := SYSDATE;

    -- определяем порядковые номера вторника и четверга
    if n_user_id IS NOT NULL and n_user_id > 0 then
       n_thu := 4;-- если из-под oebs
       n_tue := 2;
    else
       n_thu := 5; -- если  в tool'e
       n_tue := 3;
    end if;

    n := to_number(to_char(curr_date, 'D')); -- тек день недели

    if is_debug = 'Y' then
       n := n_thu;
    end if;

    if n >= n_thu then
       date_last_thu := trunc(curr_date) - ( n - n_thu );
    else
       date_last_thu := trunc(curr_date) - 7 + ( n_thu - n);
    end if;

    date_last := add_months( to_date('01' || to_char(curr_date, 'mmyyyy'), 'ddmmyyyy'), 1 ) - 1;

    if is_debug = 'Y' then
       date_last := trunc(curr_date);
    end if;

    if is_debug = 'N' then -- проверка дней
      if is_weekly = 'Y' then
         if not ( n = n_thu or n = n_tue ) then
            errbuf := to_char(curr_date, 'dd.mm.yyyy') || ' не является вторником или четвергом, осуществляется выход.';
            retcode := 0;
            RETURN;
         end if;
      else
         if trunc(curr_date) <> date_last then
            errbuf := to_char(curr_date, 'dd.mm.yyyy') || ' не является крайним днём месяца, осуществляется выход.';
            retcode := 0;
            RETURN;
         end if;
      end if;
    end if;

    for R in (
        select
             t.pi_ul_fv_id,
             t.user_id buh_user_id,
             u.bk_imp_flag,
             u.user_name buh_user,
             nvl(u.full_name, u.user_name) buh_name,
             u.role_name,
             u1.email_address,
             t.ul_val,
             ul.description ul_name
        from pi_ap.xxpi_shed_user_ul t,
             xxpi_shed_user_v u,
             XXPI_UL_V ul,
             fnd_user u1
       where t.user_id = u.user_id(+)
         and t.user_id = u1.user_id(+)
         and t.pi_ul_fv_id = ul.flex_value_id
         and u.role_name = 'PI_APPS_PAY_BUH'
         and t.is_kb_loader = 'Y'
       order by buh_user, ul_val
    ) loop -- цикл по парам бухгалтер - ЮЛ

      if is_weekly = 'Y' then -- обрабатываем счета с еженедельной загрузкой
        -- если четверг
        if n = n_thu then
           v_msg_subject := 'До '|| to_char(curr_date + 5, 'dd.mm.yyyy') ||' Вам необходимо выполнить еженедельную загрузку в Oracle EBS банковских выписок по ' || r.ul_name ||'.';
        -- если вторник
        elsif n = n_tue then
           v_msg_subject := 'Еженедельная загрузка банковских выписок в Oracle EBS по ' || r.ul_name ||' выполнена неполностью.';
        else
           v_msg_subject := 'До '|| to_char(curr_date + 5, 'dd.mm.yyyy') ||' Вам необходимо выполнить еженедельную загрузку в Oracle EBS банковских выписок по ' || r.ul_name ||'.';
        end if;
      else
        if trunc(curr_date) = date_last then
           v_msg_subject := 'Вам необходимо выполнить загрузку в Oracle EBS банковских выписок по ' || r.ul_name ||' за ' || to_char(date_last, 'MONTH') || '.';
        end if;

      end if;

      if length(v_msg_subject) > 230 then
         v_msg_subject := substr(v_msg_subject, 1, 230);
      end if;

      -- формирование тушки сообщения

        v_msg_header := '<HTML><BODY>';
        v_msg_header := v_msg_header || '<H3>Данные о состоянии загрузки РС по '|| r.ul_name ||'</h3>';
        v_msg_header := v_msg_header || '<TABLE border="2"><TR><TH>Наименование РС</th><TH>№ РС</th><TH>Загружены данные<BR/> КБ по</th><TH>Требуются данные<BR/> КБ по</th></tr>';

        v_msg_body := v_msg_header;

        cnt := 0;
        part := 0;
        l := length(v_msg_body);

        -- цикл по РС текущего ЮЛ
        for acc in (select /*+RULE */ acc.bank_account_id,
                 acc.bank_account_name,
                 acc.bank_account_num,
                 acc.inactive_date,
                 acc.attribute1 ul_val,
                 bb.bank_name,
                 bb.city,
                 bb.eft_swift_code bik,
                 bb.attribute1 ks,
                 bb.end_date bb_end_date,
                 osv.bank_date last_loaded_date,
                 decode(dc.weekly_ctrl, 'Y', 'Y', 'N') weekly_ctrl
            from ap_bank_accounts_all acc,
                 ap_bank_branches bb,
                 (select * from xxpi_osv o
                         where o.bank_date = (select /*+ INDEX(xxpi_osv xxpi_osv_fileid) */ max(oo.bank_date)
                                                     from xxpi_osv oo
                                                     where oo.file_id is not null
                                                     and oo.bank_acc_id = o.bank_acc_id
                                                     and oo.is_1c = 'N' )
                               and o.is_1c = 'N'
                                      ) osv,
                  (select d.bank_account_id, 'Y' weekly_ctrl from xxpi_acc_daily_ctrl d
                          where
                            curr_date > d.ctrl_start_date
                            and curr_date < nvl(d.ctrl_end_date, curr_date + 1)) dc
            where acc.attribute1 = r.ul_val
                  and acc.account_type='INTERNAL'
                  and acc.attribute1 is not null
                  and bb.bank_branch_id = acc.bank_branch_id
                  and (acc.inactive_date is NULL or acc.inactive_date > curr_date)
                  and (bb.end_date is NULL or bb.end_date >=curr_date)
                  and bb.bank_name not in ('Взаимозачеты', 'Вексели', 'Фиктивный', 'Кассы')
                  and osv.bank_acc_id(+) = acc.bank_account_id
                  and dc.bank_account_id(+) = acc.bank_account_id
                  and ((is_weekly = 'Y' and decode(dc.weekly_ctrl, 'Y', 'Y', 'N') = 'Y') or
                        is_weekly = 'N' and decode(dc.weekly_ctrl, 'Y', 'Y', 'N') = 'N')
                  and acc.attribute4 = 'Y' -- есть КБ
                  --and acc.attribute3 = 'Y' -- Загружать выписку
                  ) loop

            -- включаем в рассылку только счета, по которым требуется загрузка
            if is_weekly = 'Y' then
               if acc.inactive_date is null then
                 if not trunc(acc.last_loaded_date) < trunc(date_last_thu) then
                    goto end_acc;
                 end if;
               else
                 if trunc(acc.inactive_date) < trunc(date_last_thu) then
                    if not trunc(acc.last_loaded_date) < trunc(acc.inactive_date) then
                       goto end_acc;
                    end if;
                 else
                    if not trunc(acc.last_loaded_date) < trunc(date_last_thu) then
                       goto end_acc;
                    end if;
                 end if;
               end if;

            else
               if acc.inactive_date is null then
                 if not trunc(acc.last_loaded_date) < trunc(date_last) then
                    goto end_acc;
                 end if;
               else
                 if trunc(acc.inactive_date) < trunc(date_last) then
                    if not trunc(acc.last_loaded_date) < trunc(acc.inactive_date) then
                       goto end_acc;
                    end if;
                 else
                    if not trunc(acc.last_loaded_date) < trunc(date_last) then
                       goto end_acc;
                    end if;
                 end if;
               end if;

            end if;

            cnt := cnt + 1;

            if part = 0 then
               part := 1;
            end if;

            v_msg_row := '<TR><TD>'|| acc.bank_account_name ||'</td>';
            v_msg_row := v_msg_row || '<TD>'|| acc.bank_account_num ||'</td>';
            v_msg_row := v_msg_row || '<TD>'|| to_char(acc.last_loaded_date, 'dd.mm.yyyy') ||'</td>';

            if is_weekly = 'Y' then

               if acc.inactive_date IS NOT NULL and trunc(acc.inactive_date) < date_last_thu then
                  v_msg_row := v_msg_row || '<TD>'|| to_char(acc.inactive_date, 'dd.mm.yyyy') ||'</td></tr>';
               else
                  v_msg_row := v_msg_row || '<TD>'|| to_char(date_last_thu, 'dd.mm.yyyy') ||'</td></tr>';
               end if;

            else

               if acc.inactive_date IS NOT NULL and trunc(acc.inactive_date) < date_last then
                  v_msg_row := v_msg_row || '<TD>'|| to_char(acc.inactive_date, 'dd.mm.yyyy') ||'</td></tr>';
               else
                  v_msg_row := v_msg_row || '<TD>'|| to_char(date_last, 'dd.mm.yyyy') ||'</td></tr>';
               end if;

            end if;

            if length(v_msg_body || v_msg_row) > 3800 then -- переполнение, новое сообщение!!!

                ins_msg(p_msg_subject => v_msg_subject || ' (часть ' || part || ')',
                        p_msg_body => v_msg_body,
                        p_rcpt_user_id => r.buh_user_id,
                        p_rcpt_fio => r.buh_name,
                        p_rcpt_email_address => r.email_address,
                        is_debug => is_debug);

                v_msg_body := v_msg_header || v_msg_row;
                cnt := 1;
                part := part + 1;
            else
                v_msg_body := v_msg_body || v_msg_row;
            end if;

            <<end_acc>> null;

        end loop;

        if cnt > 0 then

          if part >1 then
           v_msg_subject := v_msg_subject || ' (часть ' || part || ')';
          end if;

          ins_msg(p_msg_subject => v_msg_subject,
                  p_msg_body => v_msg_body,
                  p_rcpt_user_id => r.buh_user_id,
                  p_rcpt_fio => r.buh_name,
                  p_rcpt_email_address => r.email_address,
                  is_debug => is_debug);

        end if;

    end loop;

  end send_bk_reminder;

  -- Процедура формирования напоминания - предупреждение о приближении срока загрузки
  -- срок загрузки - 10 число тек. месяца, уведомления начинают отправляться начиная с 1го числа месяца
  -- для запуска по графику
  procedure send_1c_reminder(errbuf     out nocopy varchar2,
                       retcode    out nocopy number) is
    v_msg_subject varchar2(300);
    v_msg_body varchar2(4000);
    v_msg_body_test varchar2(4000);
    v_msg_header varchar2(4000);
    v_msg_row varchar2(4000);
    to_send boolean := FALSE;
    cnt number;
    l number := 0;
    v_MSG_ID number(15);
    v_msg_code varchar2(15) := '1C_reminder';
    is_msg_overflow boolean;
    part number;
    is_debug varchar2(1) := 'N';
    max_end_date DATE;

  procedure ins_msg(p_msg_subject in varchar2,
                    p_msg_body in varchar2,
                    p_rcpt_user_id in number,
                    p_rcpt_fio in varchar2,
                    p_rcpt_email_address in varchar2,
                    is_debug in varchar2 default 'Y') is

    v_msg_body varchar2(4000);
    v_msg_id number;
    sender_user_id NUMBER;
    sender_person_id NUMBER;
    rcpt_person_id NUMBER;

  begin

    v_msg_body := p_msg_body;

    v_msg_body := v_msg_body || '</table>';
    v_msg_body := v_msg_body || '<BR>--<BR><BR> Письмо сформировано OEBS автоматически. Не отвечайте на него.';
    v_msg_body := v_msg_body || '</body></html>';

    -- вставка сообщения
    select XXPI_AP_MSG_ID_SEQ.NEXTVAL into v_MSG_ID from dual;

    begin
      select u.user_id, u.employee_id into sender_user_id, sender_person_id
      from fnd_user u
      where user_name = 'V@@@@@@@';
    exception
      when no_data_found then
        sender_user_id := NULL;
        sender_person_id :=NULL;
    end;

    begin
      select u.employee_id into rcpt_person_id
      from fnd_user u
      where user_id = decode(is_debug, 'N', p_rcpt_user_id, 6568);
    exception
      when no_data_found then
        sender_user_id := NULL;
        sender_person_id :=NULL;
    end;

    insert into XXPI_AP_MSG
      (MSG_ID,
       CODE_NAME,
       SENDER_COMENT,
       CREATED_DATE,
       sender_user_id--,
       --sender_person_id
       )
    values
      (v_MSG_ID,
       '1C_reminder',
       v_msg_body,
       sysdate,
       sender_user_id--,
       --sender_person_id
       );

     insert into xxpi_msg_subject
       (msg_id,
       msg_subject)
     values
       (v_msg_id,
       p_msg_subject);

    -- вставка получателя
    insert into XXPI_AP_MSG_RCPT
      (MSG_ID,
       USER_ID,
       RCPT_FIO,
       RCPT_EMAIL,
       MRC_ID
       , person_id )
    values
      (v_MSG_ID,
       decode(is_debug, 'Y', 6568, p_rcpt_user_id),
       decode(is_debug, 'Y', 'Момзякофф', p_rcpt_fio),
       decode(is_debug, 'Y', 'amomzyakov@kuku.ru',p_rcpt_email_address),
       XXPI_AP_MSG_RCPT_SEQ.nextval,
       rcpt_person_id);

    commit;

  end ins_msg;


  begin
    for R in (
        select
             --t.pi_ul_fv_id,
             t.user_id buh_user_id,
             u.bk_imp_flag,
             u.user_name buh_user,
             nvl(u.full_name, u.user_name) buh_name,
             u.role_name,
             u1.email_address --,
             --t.ul_val,
             --ul.description ul_name
        from pi_ap.xxpi_shed_user_ul t,
             xxpi_shed_user_v u,
             XXPI_UL_V ul,
             fnd_user u1
       where t.user_id = u.user_id(+)
         and t.user_id = u1.user_id(+)
         and t.pi_ul_fv_id = ul.flex_value_id
         and u.role_name = 'PI_APPS_PAY_BUH'
         and t.is_accountant = 'Y'
       group by
             t.user_id,
             u.bk_imp_flag,
             u.user_name,
             nvl(u.full_name, u.user_name),
             u.role_name,
             u1.email_address
       order by u.user_name    ) loop

        -- формирование темы и тушки сообщения
        if sysdate < to_date(to_char(sysdate, 'YYYYMM') || '10', 'YYYYMMDD') then -- предупреждение

           v_msg_subject := 'До '||'10.'|| to_char(sysdate, 'MM.YYYY')  ||' Вам необходимо загрузить в Oracle EBS данные 1С';

        else -- напоминание

           v_msg_subject := 'Пожалуйста, загрузите в Oracle EBS данные 1С';

        end if;

        if length(v_msg_subject) > 230 then
           v_msg_subject := substr(v_msg_subject, 1, 230);
        end if;

        -- формирование тушки сообщения

        v_msg_header := '<HTML><BODY>';
        v_msg_header := v_msg_header || '<H3>Данные о состоянии загрузки по юридическим лицам</h3>';
        v_msg_header := v_msg_header || '<TABLE border="2"><TR><TH>Наименование ЮЛ</th><TH>Код ЮЛ</th><TH>Загружены данные<BR/> 1С по</th><TH>Требуются данные<BR/> 1С по</th></tr>';

        v_msg_body := v_msg_header;

        cnt := 0;
        part := 0;
        l := length(v_msg_body);

        for ULS in (
              select
                   t.pi_ul_fv_id,
                   t.user_id buh_user_id,
                   u.bk_imp_flag,
                   u.user_name buh_user,
                   nvl(u.full_name, u.user_name) buh_name,
                   u.role_name,
                   u1.email_address,
                   t.ul_val,
                   ul.description ul_name,
                   to_date(to_char(sysdate, 'yyyymm') || '01', 'yyyymmdd') - 1 last_prev_date
              from pi_ap.xxpi_shed_user_ul t,
                   xxpi_shed_user_v u,
                   XXPI_UL_V ul,
                   fnd_user u1
             where t.user_id = u.user_id(+)
               and t.user_id = u1.user_id(+)
               and t.pi_ul_fv_id = ul.flex_value_id
               and u.role_name = 'PI_APPS_PAY_BUH'
               and t.is_accountant = 'Y'
               and t.user_id = r.buh_user_id
             order by buh_user, ul_val
        ) loop

            cnt := cnt + 1;

            if part = 0 then
               part := 1;
            end if;

            v_msg_row := '<TR><TD>'|| uls.ul_name ||'</td>';
            v_msg_row := v_msg_row || '<TD>'|| uls.ul_val ||'</td>';

            select max(end_date) into max_end_date
            from xxpi_bk_imp_file f
            where f.is_1c = 'Y'
                  and f.loaded = 'Y'
                  and f.ul_val_1c = uls.ul_val;

            if max_end_date is null then
               v_msg_row := v_msg_row || '<TD>Нет данных.</td>';
            else
               v_msg_row := v_msg_row || '<TD>'|| to_char(max_end_date, 'dd.mm.yyyy') ||'</td>';
            end if;

            if max_end_date < uls.last_prev_date then

              v_msg_row := v_msg_row || '<TD>'|| to_char(uls.last_prev_date, 'dd.mm.yyyy') ||'</td></tr>';

              if length(v_msg_body || v_msg_row) > 3800 then -- переполнение, новое сообщение!!!

                  ins_msg(p_msg_subject => v_msg_subject || ' (часть ' || part || ')',
                          p_msg_body => v_msg_body,
                          p_rcpt_user_id => r.buh_user_id,
                          p_rcpt_fio => r.buh_name,
                          p_rcpt_email_address => r.email_address,
                          is_debug => is_debug);

                  v_msg_body := v_msg_header || v_msg_row;
                  cnt := 1;
                  part := part + 1;
              else
                  v_msg_body := v_msg_body || v_msg_row;
              end if;

            else

              cnt := cnt - 1;

            end if;

        end loop;

        if cnt > 0 then

          if part >1 then
           v_msg_subject := v_msg_subject || ' (часть ' || part || ')';
          end if;

          ins_msg(p_msg_subject => v_msg_subject,
                  p_msg_body => v_msg_body,
                  p_rcpt_user_id => r.buh_user_id,
                  p_rcpt_fio => r.buh_name,
                  p_rcpt_email_address => r.email_address,
                  is_debug => is_debug);

        end if;

    end loop;

  end send_1c_reminder;

BEGIN
  n_user_id := nvl(FND_GLOBAL.user_id, XXPI_BK_IMPU.f_get_user_id);
end xxpi_bk_err_stats_pkg;
/
